open Common

module Functions = struct
  let rec sexp_to_string = function
    (* | Atom (_, s) when String.starts_with ~prefix:":" s -> unpack_symbol s *)
    | Atom (_, s) -> s
    | RBList (_, xs) -> "(" ^ String.concat " " (List.map sexp_to_string xs) ^ ")"
    | SBList (_, xs) -> "[" ^ String.concat " " (List.map sexp_to_string xs) ^ "]"
    | CBList (_, xs) -> "{" ^ String.concat " " (List.map sexp_to_string xs) ^ "}"

  let rec sexp_to_string2 = function
    | Atom (_, s) when String.starts_with ~prefix:":" s -> unpack_symbol s
    | Atom (_, s) when String.starts_with ~prefix:"\"" s -> unpack_string s
    | Atom (_, s) -> s
    | RBList (_, xs) -> "(" ^ String.concat " " (List.map sexp_to_string2 xs) ^ ")"
    | SBList (_, xs) -> "[" ^ String.concat " " (List.map sexp_to_string2 xs) ^ "]"
    | CBList (_, xs) -> "{" ^ String.concat " " (List.map sexp_to_string2 xs) ^ "}"

  let functions : ((cljexp -> cljexp list -> cljexp) -> cljexp list -> cljexp) StringMap.t =
    StringMap.empty
    |> StringMap.add "vector" (fun _ args -> SBList (unknown_location, args))
    |> StringMap.add "list" (fun _ args -> RBList (unknown_location, args))
    |> StringMap.add "hash-map" (fun _ args -> CBList (unknown_location, args))
    |> StringMap.add "str" (fun _ args ->
           args
           |> List.map (function
                | Atom (_, s) when String.starts_with ~prefix:":" s -> unpack_symbol s
                | Atom (_, x) when String.starts_with ~prefix:"\"" x && String.ends_with ~suffix:"\"" x ->
                    String.sub x 1 (String.length x - 2)
                | Atom (_, x) -> x
                | n -> sexp_to_string n)
           |> String.concat ""
           |> fun xs -> Atom (unknown_location, "\"" ^ xs ^ "\""))
    |> StringMap.add "println" (fun _ args ->
           args |> List.map sexp_to_string2 |> String.concat " " |> print_endline;
           Atom (unknown_location, "nil"))
    |> StringMap.add "FIXME" (fun _ args ->
           let msg = args |> List.map sexp_to_string2 |> String.concat " " in
           failwith msg)
    |> StringMap.add "concat" (fun _ args ->
           args
           |> List.concat_map (function RBList (_, xs) -> xs | SBList (_, xs) -> xs | n -> failnode __LOC__ [ n ])
           |> fun x -> RBList (unknown_location, x))
    |> StringMap.add "+" (fun _ args ->
           args
           |> List.map (function Atom (_, x) -> int_of_string x | n -> failnode __LOC__ [ n ])
           |> List.fold_left ( + ) 0
           |> fun x -> Atom (unknown_location, string_of_int x))
    |> StringMap.add "*" (fun _ args ->
           args
           |> List.map (function Atom (_, x) -> int_of_string x | n -> failnode __LOC__ [ n ])
           |> List.fold_left ( * ) 1
           |> fun x -> Atom (unknown_location, string_of_int x))
    |> StringMap.add "-" (fun _ args ->
           match args with
           | Atom (_, x) :: [] -> Atom (unknown_location, "-" ^ x)
           | Atom (_, x) :: args ->
               args
               |> List.map (function Atom (_, x) -> int_of_string x | n -> failnode __LOC__ [ n ])
               |> List.fold_left ( - ) (int_of_string x)
               |> fun x -> Atom (unknown_location, string_of_int x)
           | n -> failnode __LOC__ n)
    |> StringMap.add "/" (fun _ args ->
           match args with
           | Atom (_, x) :: args ->
               args
               |> List.map (function Atom (_, x) -> int_of_string x | n -> failnode __LOC__ [ n ])
               |> List.fold_left ( / ) (int_of_string x)
               |> fun x -> Atom (unknown_location, string_of_int x)
           | n -> failnode __LOC__ n)
    |> StringMap.add "=" (fun _ args ->
           match args with
           | _ :: [] -> Atom (unknown_location, "true")
           | Atom (_, x) :: xs ->
               Atom (unknown_location, string_of_bool (List.for_all (function Atom (_, y) -> x = y | _ -> false) xs))
           | n -> failnode __LOC__ n)
    |> StringMap.add "get" (fun _ args ->
           (* TODO: merge matches *)
           match args with
           | [ map; key ] -> (
               match (map, key) with
               | CBList (_, xs), Atom (_, key) ->
                   List.split_into_pairs xs
                   |> List.find (fun (k, _) -> match k with Atom (_, k) -> k = key | n -> failnode __LOC__ [ n ])
                   |> snd
               | SBList (_, xs), Atom (_, i) when int_of_string_opt i |> Option.is_some -> List.nth xs (int_of_string i)
               | m, k -> failnode __LOC__ [ m; k ])
           | n -> failnode __LOC__ n)
    |> StringMap.add "reduce" (fun interpret -> function
         | [ f; def_; xs ] -> (
             (* print_endline @@ "LOG2: " ^ debug_show_cljexp [ xs ]; *)
             match xs with
             | SBList (_, xs) -> List.fold_left (fun acc x -> interpret f [ acc; x ]) def_ xs
             | RBList (_, xs) -> List.fold_left (fun acc x -> interpret f [ acc; x ]) def_ xs
             | CBList (_, xs) ->
                 let rec loop = function [] -> [] | k :: v :: tail -> (k, v) :: loop tail | _ -> failwith __LOC__ in
                 let xs = loop xs in
                 List.fold_left (fun acc (k, v) -> interpret f [ acc; SBList (unknown_location, [ k; v ]) ]) def_ xs
             | n -> failnode __LOC__ [ n ])
         | n -> failnode __LOC__ n)
    |> StringMap.add "map" (fun interpret -> function
         | [ f; xs ] -> (
             match xs with
             | SBList (m, xs) -> SBList (m, List.map (fun x -> interpret f [ x ]) xs)
             | n -> failnode __LOC__ [ n ])
         | n -> failnode __LOC__ n)
    |> StringMap.add "map?" (fun _ args ->
           match args with [ CBList _ ] -> Atom (unknown_location, "true") | _ -> Atom (unknown_location, "false"))
    |> StringMap.add "vector?" (fun _ args ->
           match args with [ SBList _ ] -> Atom (unknown_location, "true") | _ -> Atom (unknown_location, "false"))
    |> StringMap.add "count" (fun _ args ->
           match args with
           | [ SBList (_, xs) ] -> Atom (unknown_location, string_of_int (List.length xs))
           | n -> failnode __LOC__ n)
    |> StringMap.add "drop" (fun _ args ->
           match args with
           | [ Atom (_, str_count); SBList (_, xs) ] ->
               let count = int_of_string str_count in
               let xs = xs |> List.filteri (fun i _ -> i >= count) in
               SBList (unknown_location, xs)
           | n -> failnode __LOC__ n)

  let is_registered fname = StringMap.mem fname functions

  let handle interpret fname args =
    let args = List.map interpret args in
    let f = StringMap.find fname functions in
    f (fun f args -> interpret (RBList (unknown_location, f :: args))) args
end

let rec interpret (context : context) (node : cljexp) : context * cljexp =
  (* log_sexp "INTERPRET: " node |> ignore; *)
  (* prerr_endline @@ "SCOPE: " ^ debug_show_scope context ^ "\n"; *)
  let interpret_ x = interpret context x |> snd in
  let with_context x = (context, x) in
  match node with
  | Atom (m, x) when String.starts_with ~prefix:"'" x -> (context, Atom (m, String.sub x 1 (String.length x - 1)))
  | Atom (_, v) as x
    when v = "true" || v = "false" || v = "null" || String.starts_with ~prefix:"\"" v
         || String.starts_with ~prefix:":" v || String.starts_with ~prefix:"'" v || String.starts_with ~prefix:"-" v
         ||
         let ch = String.get v 0 in
         ch >= '0' && ch <= '9' ->
      (context, x)
  | CBList (m, vec_args) -> (context, CBList (m, List.map interpret_ vec_args))
  | SBList (m, vec_args) -> (context, SBList (m, List.map interpret_ vec_args))
  | RBList (m, [ Atom (_, "transform_nodes"); RBList (_, _ :: opt); xs ]) ->
      let rec unpack_to_map = function
        | [] -> []
        | Atom (_, k) :: v :: tail -> (k, v) :: unpack_to_map tail
        | n -> failnode __LOC__ n
      in
      let xs = match interpret_ xs with RBList (_, xs) -> xs | n -> failnode __LOC__ [ n ] in
      let sep =
        unpack_to_map opt |> List.assoc_opt ":sep"
        |> ( function Some x -> x | None -> failnode __LOC__ opt )
        |> interpret_
      in
      let len = List.length xs in
      let r =
        xs |> List.mapi (fun i x -> (i, x)) |> List.concat_map (fun (i, x) -> if i < len - 1 then [ x; sep ] else [ x ])
      in
      (context, RBList (m, r))
  | RBList (_, [ Atom (l, "symbol"); n ]) ->
      Atom
        ( l,
          match interpret_ n with
          | Atom (_, x) when String.starts_with ~prefix:"\"" x -> String.sub x 1 (String.length x - 2)
          | n -> failnode __LOC__ [ n ] )
      |> with_context
  (* Constants *)
  | Atom (_, "nil") as x -> (context, x)
  | Atom (_, "__FILENAME__") -> (context, Atom (unknown_location, context.filename))
  | Atom (_, "__LINE__") ->
      Atom (unknown_location, string_of_int (context.loc.line - context.start_line)) |> with_context
  | Atom (_, "__POSITION__") -> (context, Atom (unknown_location, string_of_int context.loc.pos))
  (* /Constants *)
  (* Resolve scope value *)
  | Atom (m, x) when StringMap.exists (fun k _ -> k = x) context.scope -> (
      match StringMap.find x context.scope with
      | Atom (m2, arg_val), ctx -> (!ctx, Atom ({ m with pos = m2.pos; line = m2.line }, arg_val))
      | x, ctx -> (!ctx, x))
  | Atom (_, name) when String.contains name '/' ->
      let parts = String.split_on_char '/' name in
      (* prerr_endline @@ "SCOPE: [" ^ debug_show_imports context ^ "] " ^ List.hd parts; *)
      let context = StringMap.find (List.hd parts) context.imports in
      let fn, fn_ctx = StringMap.find (List.nth parts 1) context.scope in
      (!fn_ctx, fn)
  (* /Resolve scope value *)
  (* SPECIAL FORMS *)
  | RBList (m, [ Atom (l, "quote*"); arg ]) -> (
      match arg with Atom (l, x) -> (context, Atom (l, x)) | n -> (context, RBList (m, [ Atom (l, "quote*"); n ])))
  | RBList (_, Atom (_, "do*") :: body) ->
      let context2, results = body |> List.fold_left_map (fun ctx x -> interpret ctx x) context in
      let last_node = results |> List.rev |> List.hd in
      (context2, last_node)
  | RBList (_, [ Atom (_, "if*"); c; a; b ]) -> (
      match interpret_ c with
      | Atom (_, "true") -> (context, interpret_ a)
      | Atom (_, "false") -> (context, interpret_ b)
      | n -> failnode __LOC__ [ n ])
  | RBList (m, [ Atom (_, "def*"); Atom (_, name); body ]) ->
      let body = interpret context body |> snd in
      let ctx_ref = ref context in
      let context = { context with scope = context.scope |> StringMap.add name (body, ctx_ref) } in
      ctx_ref := context;
      (context, RBList (m, [ Atom (unknown_location, "comment") ]))
  | RBList (m, [ Atom (_, "let*"); Atom (_, name); body ]) ->
      let body = interpret context body |> snd in
      let context = { context with scope = context.scope |> StringMap.add name (body, ref context) } in
      (context, RBList (m, [ Atom (unknown_location, "comment") ]))
  | RBList (_, Atom (_, "let*") :: SBList (_, bindings) :: body) ->
      let scope =
        bindings |> List.split_into_pairs
        |> List.fold_left
             (fun ctx (k, v) ->
               let name = match k with Atom (_, s) -> s | n -> failnode __LOC__ [ n ] in
               let v = interpret { context with scope = ctx } v |> snd in
               StringMap.add name (v, ref context) ctx)
             context.scope
      in
      let _, results = body |> List.fold_left_map (fun ctx x -> interpret ctx x) { context with scope } in
      results |> List.rev |> List.hd |> with_context
  (* Lambda *)
  | RBList (_, Atom (_, "fn*") :: _ :: _) as x -> x |> with_context
  (* Function call *)
  | RBList (_, Atom (_, fname) :: args) when Functions.is_registered fname ->
      Functions.handle (fun n -> interpret context n |> snd) fname args |> with_context
  | RBList (_, target :: args) ->
      let arg_values = List.map interpret_ args in
      let arg_names, f_body, f_ctx =
        match interpret context target with
        (* TODO: Delete this *)
        | f_ctx, RBList (_, Atom (_, "fn*") :: SBList (_, args) :: body) ->
            (List.map (function Atom (_, x) -> x | n -> failnode __LOC__ [ n ]) args, body, f_ctx)
        | f_ctx, RBList (_, Atom (_, "fn*") :: RBList (_, args) :: body) ->
            (List.map (function Atom (_, x) -> x | n -> failnode __LOC__ [ n ]) args, body, f_ctx)
        | _, n ->
            prerr_endline @@ "SCOPE: [" ^ debug_show_scope context ^ "]";
            failnode __LOC__ [ n; node ]
      in

      (* Add function arguments to function scope *)
      let scope =
        List.fold_left2 (fun ctx k v -> StringMap.add k (v, ref context) ctx) f_ctx.scope arg_names arg_values
      in

      let _, results = f_body |> List.fold_left_map (fun ctx x -> interpret ctx x) { f_ctx with scope } in
      results |> List.rev |> List.hd |> with_context
  | node ->
      prerr_endline @@ "SCOPE: [" ^ debug_show_scope context ^ "]";
      prerr_endline @@ "MACROS: [" ^ debug_show_macro context ^ "]";
      prerr_endline @@ "IMPORTS: [" ^ debug_show_imports context ^ "]";
      failnode __LOC__ [ node ]

let main (log : bool) (filename : string) prelude_macros code =
  let prelude_ctx, prelude_sexp =
    prelude_macros |> Frontend.parse_and_simplify { empty_context with interpreter = interpret } "prelude"
  in
  let rec invoke filename code =
    let ctx, node = code |> Frontend.parse_and_simplify prelude_ctx filename in
    node
    |> try_log "Parse_and_simplify      ->" log
    |> Stage_simplify_let.invoke
    |> try_log "Stage_simplify_let      ->" log
    |> Stage_normalize_bracket.invoke
    |> try_log "Stage_normalize_bracket ->" log
    |> Stage_ns_inline.invoke invoke ctx
    |> fun (ctx, node) ->
    node
    |> try_log "Stage_ns_inline         ->" log
    |> Stage_linter.invoke ctx prelude_sexp
    |> try_log "Stage_linter            ->" log
    |> interpret ctx
  in
  invoke filename code |> snd |> show_sexp
  |> ( function x when String.starts_with ~prefix:"\"" x -> unpack_string x |> Scanf.unescaped | x -> x )
  |> String.trim
