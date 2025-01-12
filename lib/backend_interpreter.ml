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

  let rec debug_obj_to_string = function
    | OList xs -> "(" ^ String.concat " " (List.map debug_obj_to_string xs) ^ ")"
    | OVector xs -> "[" ^ String.concat " " (List.map debug_obj_to_string xs) ^ "]"
    | OMap xs ->
        "{" ^ String.concat " " (List.map (fun (k, v) -> debug_obj_to_string k ^ ": " ^ debug_obj_to_string v) xs) ^ "}"
    | OString s -> "\"" ^ Scanf.unescaped s ^ "\""
    | OInt i -> string_of_int i ^ "i"
    | OBool b -> string_of_bool b ^ "b"
    | ONil -> "nil"
    | OLambda _ -> "lambda"
    | OQuote n -> "(quote " ^ debug_show_cljexp1 n ^ ")"

  let rec obj_to_string = function
    | OList xs -> "(" ^ String.concat " " (List.map obj_to_string xs) ^ ")"
    | OVector xs -> "[" ^ String.concat " " (List.map obj_to_string xs) ^ "]"
    | OMap xs -> "{" ^ String.concat " " (List.map (fun (k, v) -> obj_to_string k ^ " " ^ obj_to_string v) xs) ^ "}"
    | OString s -> "\"" ^ s ^ "\""
    | OInt i -> string_of_int i
    | OBool b -> string_of_bool b
    | ONil -> "nil"
    | OLambda _ -> "lambda"
    | OQuote n -> "(quote " ^ debug_show_cljexp [ n ] ^ ")"

  let failobj loc objs = failwith @@ loc ^ " - " ^ String.concat " " (List.map debug_obj_to_string objs)

  let functions : (obj list -> obj) StringMap.t =
    StringMap.empty
    |> StringMap.add "vector" (fun args -> OVector args)
    |> StringMap.add "list" (fun args -> OList args)
    |> StringMap.add "hash-map" (fun args -> OMap (args |> List.split_into_pairs))
    |> StringMap.add "str" (fun args ->
           args
           |> List.map (function
                (* | Atom (_, s) when String.starts_with ~prefix:":" s -> unpack_symbol s
                | Atom (_, x) when String.starts_with ~prefix:"\"" x && String.ends_with ~suffix:"\"" x ->
                    String.sub x 1 (String.length x - 2)
                | Atom (_, x) -> x *)
                | OString x -> x
                | OInt x -> string_of_int x
                | OQuote x -> show_sexp x
                | n -> failobj __LOC__ [ n ])
           |> String.concat ""
           |> fun xs -> OString xs)
    |> StringMap.add "println" (fun args ->
           args |> List.map obj_to_string |> String.concat " " |> print_endline;
           ONil)
    |> StringMap.add "vec" (fun args -> match args with [ OList xs ] -> OVector xs | n -> failobj __LOC__ n)
    |> StringMap.add "FIXME" (fun args ->
           let msg = args |> List.map obj_to_string |> String.concat " " in
           failwith msg)
    |> StringMap.add "concat" (fun args ->
           args |> List.concat_map (function OList xs -> xs | OVector xs -> xs | n -> failobj __LOC__ [ n ])
           |> fun x -> OList x)
    |> StringMap.add "+" (fun args ->
           args |> List.map (function OInt x -> x | n -> failobj __LOC__ [ n ]) |> List.fold_left ( + ) 0 |> fun x ->
           OInt x)
    |> StringMap.add "*" (fun args ->
           args |> List.map (function OInt x -> x | n -> failobj __LOC__ [ n ]) |> List.fold_left ( * ) 1 |> fun x ->
           OInt x)
    |> StringMap.add "-" (fun args ->
           match args with
           | OInt x :: [] -> OInt (-x)
           | OInt x :: args ->
               args |> List.map (function OInt x -> x | n -> failobj __LOC__ [ n ]) |> List.fold_left ( - ) x
               |> fun x -> OInt x
           | n -> failobj __LOC__ n)
    |> StringMap.add "/" (fun args ->
           match args with
           | OInt x :: args ->
               args |> List.map (function OInt x -> x | n -> failobj __LOC__ [ n ]) |> List.fold_left ( / ) x
               |> fun x -> OInt x
           | n -> failobj __LOC__ n)
    |> StringMap.add "=" (fun args ->
           match args with
           | _ :: [] -> OBool true
           | x :: xs -> OBool (List.for_all (( = ) x) xs)
           | n -> failobj __LOC__ n)
    |> StringMap.add "get" (fun args ->
           (* TODO: merge matches *)
           match args with
           | [ map; key ] -> (
               match (map, key) with
               | OMap xs, key ->
                   xs |> List.find_opt (fun (k, _) -> k = key) |> Option.map snd |> Option.value ~default:ONil
               | OList xs, OInt i -> List.nth xs i
               | OVector xs, OInt i -> List.nth xs i
               | m, k -> failobj __LOC__ [ m; k ])
           | n -> failobj __LOC__ n)
    |> StringMap.add "reduce" (function
         | [ OLambda f; def_; xs ] -> (
             (* print_endline @@ "LOG2: " ^ debug_show_cljexp [ xs ]; *)
             match xs with
             | OVector xs -> List.fold_left (fun acc x -> f [ acc; x ]) def_ xs
             | OList xs -> List.fold_left (fun acc x -> f [ acc; x ]) def_ xs
             | OMap xs -> List.fold_left (fun acc (k, v) -> f [ acc; OVector [ k; v ] ]) def_ xs
             | n -> failobj __LOC__ [ n ])
         | n -> failobj __LOC__ n)
    |> StringMap.add "map" (function
         | [ OLambda f; xs ] -> (
             (* prerr_endline @@ "MAP1: " ^ debug_show_cljexp [ f ]; *)
             (* prerr_endline @@ "MAP2: " ^ debug_obj_to_string xs; *)
             match xs with
             | OVector xs -> OVector (List.map (fun x -> f [ x ]) xs)
             | OList xs -> OList (List.map (fun x -> f [ x ]) xs)
             (* | OMap xs -> OMap ( List.map (fun (k,v) -> interpret f [ OVector [k;v] ]) xs) *)
             | n -> failobj __LOC__ [ n ])
         | n -> failobj __LOC__ n)
    |> StringMap.add "map?" (fun args -> match args with [ OMap _ ] -> OBool true | _ -> OBool false)
    |> StringMap.add "vector?" (fun args -> match args with [ OVector _ ] -> OBool true | _ -> OBool false)
    |> StringMap.add "count" (fun args ->
           match args with
           | [ OList xs ] -> OInt (List.length xs)
           | [ OVector xs ] -> OInt (List.length xs)
           | [ OMap xs ] -> OInt (List.length xs)
           | n -> failobj __LOC__ n)
    |> StringMap.add "drop" (fun args ->
           match args with
           | [ OInt count; OList xs ] -> OList (xs |> List.filteri (fun i _ -> i >= count))
           | [ OInt count; OVector xs ] -> OVector (xs |> List.filteri (fun i _ -> i >= count))
           | n -> failobj __LOC__ n)
    |> StringMap.add "some?" (fun args -> match args with [ ONil ] -> OBool false | _ -> OBool true)
end

let attach_meta m = function OQuote x -> OQuote (change_meta m x) | x -> x

let rec interpret (context : context) (node : cljexp) : context * obj =
  log_sexp "INTERPRET:" node |> ignore;
  let interpret_ x = interpret context x |> snd in
  let with_context x = (context, x) in
  match node with
  | Atom (_, "true") -> (context, OBool true)
  | Atom (_, "false") -> (context, OBool false)
  | Atom (_, "nil") -> (context, ONil)
  | Atom (_, v) when String.starts_with ~prefix:"\"" v -> (context, OString (unpack_string v))
  | Atom (_, v) when String.starts_with ~prefix:":" v -> (context, OString (unpack_symbol v))
  | Atom (m, x) when String.starts_with ~prefix:"'" x -> (context, OQuote (Atom (m, unpack_symbol x)))
  | Atom (_, v)
    when (String.starts_with ~prefix:"-" v && v <> "-")
         ||
         let ch = String.get v 0 in
         ch >= '0' && ch <= '9' ->
      (* prerr_endline @@ "INTERPRET: INT " ^ debug_show_cljexp [ node ]; *)
      (context, OInt (int_of_string v))
  | CBList (_, vec_args) -> (context, OMap (List.map interpret_ vec_args |> List.split_into_pairs))
  | SBList (_, vec_args) -> (context, OVector (List.map interpret_ vec_args))
  | RBList (_, [ Atom (_, "transform_nodes"); RBList (_, _ :: opt); xs ]) ->
      let rec unpack_to_map = function
        | [] -> []
        | Atom (_, k) :: v :: tail -> (k, v) :: unpack_to_map tail
        | n -> failnode __LOC__ n
      in
      let xs =
        match interpret_ xs with
        | OList xs -> xs
        | n ->
            (* prerr_endline @@ __LOC__ ^ " ["
            ^ (StringMap.find "xs" context.scope |> fst |> Functions.debug_obj_to_string)
            ^ "]"; *)
            (* failnode __LOC__ [xs] |> ignore; *)
            Functions.failobj __LOC__ [ n ]
      in
      let sep =
        unpack_to_map opt |> List.assoc_opt ":sep"
        |> ( function Some x -> x | None -> failnode __LOC__ opt )
        |> interpret_
      in
      let len = List.length xs in
      let r =
        xs |> List.mapi (fun i x -> (i, x)) |> List.concat_map (fun (i, x) -> if i < len - 1 then [ x; sep ] else [ x ])
      in
      (context, OList r)
  (* | RBList (_, [ Atom (l, "symbol"); n ]) ->
      Atom
        ( l,
          match interpret_ n with
          | Atom (_, x) when String.starts_with ~prefix:"\"" x -> String.sub x 1 (String.length x - 2)
          | n -> failnode __LOC__ [ n ] )
      |> with_context *)
  (* Constants *)
  | Atom (_, "__FILENAME__") -> (context, OString context.filename)
  | Atom (_, "__LINE__") -> OInt (context.loc.line - context.start_line) |> with_context
  | Atom (_, "__POSITION__") -> (context, OInt context.loc.pos)
  (* /Constants *)
  (* Resolve scope value *)
  | Atom (m, x) when StringMap.exists (fun k _ -> k = x) context.scope -> (
      match StringMap.find x context.scope with
      | arg_val, ctx ->
          prerr_endline @@ "- VALUE: " ^ Functions.debug_obj_to_string arg_val;
          (!ctx, attach_meta m arg_val))
  | Atom (_, name) when String.contains name '/' ->
      let parts = String.split_on_char '/' name in
      (* prerr_endline @@ "SCOPE: [" ^ debug_show_imports context ^ "] " ^ List.hd parts; *)
      let context = StringMap.find (List.hd parts) context.imports in
      let fn, fn_ctx = StringMap.find (List.nth parts 1) context.scope in
      (!fn_ctx, fn)
  (* /Resolve scope value *)
  (* SPECIAL FORMS *)
  | RBList (_, [ Atom (_, "quote*"); arg ]) ->
      (* match arg with
      | Atom (l, x) -> (context, Atom (l, x))
      | n -> (context, RBList (m, [ Atom (l, "quote*"); n ])) *)
      OQuote arg |> with_context
  | RBList (_, Atom (_, "do*") :: body) ->
      let context2, results = body |> List.fold_left_map (fun ctx x -> interpret ctx x) context in
      let last_node = results |> List.rev |> List.hd in
      (context2, last_node)
  | RBList (_, [ Atom (_, "if*"); c; a; b ]) -> (
      match interpret_ c with
      | OBool true -> (context, interpret_ a)
      | OBool false -> (context, interpret_ b)
      | _ -> failwith __LOC__)
  | RBList (_, [ Atom (_, "def*"); Atom (_, name); body ]) ->
      let body = interpret context body |> snd in
      let ctx_ref = ref context in
      let context = { context with scope = context.scope |> StringMap.add name (body, ctx_ref) } in
      ctx_ref := context;
      (context, ONil)
  | RBList (_, [ Atom (_, "let*"); Atom (_, name); body ]) ->
      let body = interpret context body |> snd in
      let context = { context with scope = context.scope |> StringMap.add name (body, ref context) } in
      (context, ONil)
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
  | RBList (_, Atom (_, "fn*") :: args :: body) ->
      let arg_names =
        (match args with SBList (_, x) -> x | RBList (_, x) -> x | x -> failnode __LOC__ [ x ])
        |> List.map (function Atom (_, x) -> x | n -> failnode __LOC__ [ n ])
      in
      let f =
       fun (args : obj list) : obj ->
        let scope =
          List.map2 (fun n v -> (n, v)) arg_names args
          |> List.fold_left (fun scope (n, v) -> StringMap.add n (v, ref context) scope) context.scope
        in
        let context = { context with scope } in
        interpret context (RBList (unknown_location, Atom (unknown_location, "do*") :: body)) |> snd
      in
      OLambda f |> with_context
  (* Function call *)
  | RBList (_, target :: args) as fc ->
      let target = interpret_ target in
      let arg_values = List.map interpret_ args in
      (match target with OLambda f -> f arg_values | _ -> failnode __LOC__ [ fc ]) |> with_context
  | Atom (_, fname) when StringMap.mem fname context.functions ->
      prerr_endline @@ "- FUNCTION: " ^ fname;
      let f, _ctx = StringMap.find fname context.functions in
      interpret context f
  | node ->
      prerr_endline @@ "==========================================";
      prerr_endline @@ "FUNCS: [" ^ (context.functions |> StringMap.bindings |> List.map fst |> String.concat ",") ^ "]";
      prerr_endline @@ "SCOPE: [" ^ debug_show_scope context ^ "]";
      prerr_endline @@ "MACROS: [" ^ debug_show_macro context ^ "]";
      prerr_endline @@ "IMPORTS: [" ^ debug_show_imports context ^ "]";
      failnode __LOC__ [ node ]

let interpret_with_prelude (context : context) node : context * obj =
  let scope =
    Functions.functions |> StringMap.bindings
    |> List.fold_left (fun scope (fname, f) -> StringMap.add fname (OLambda f, ref context) scope) context.scope
  in
  interpret { context with scope } node

let rec obj_to_cljexp = function
  | OString x -> Atom (unknown_location, "\"" ^ x ^ "\"")
  | OInt x -> Atom (unknown_location, string_of_int x)
  | OQuote x -> x
  | OList xs -> RBList (unknown_location, List.map obj_to_cljexp xs)
  | OVector xs -> SBList (unknown_location, List.map obj_to_cljexp xs)
  | OMap xs -> CBList (unknown_location, xs |> List.concat_map (fun (k, v) -> [ obj_to_cljexp k; obj_to_cljexp v ]))
  | n -> failwith @@ __LOC__ ^ " - " ^ show_obj n

let mk_interpret context code =
  let ctx, result = interpret_with_prelude context code in
  (ctx, obj_to_cljexp result)

let mk_eval () =
  let ctx, _ = Frontend.parse_and_simplify empty_context "prelude.clj" Preludes.interpreter in
  fun _ node ->
    (* prerr_endline @@ "LOG:EVAL:1: " ^ debug_show_cljexp [ node ]; *)
    let ctx, node = Frontend.desugar_and_register ctx node in
    (* prerr_endline @@ "LOG:EVAL:2: " ^ debug_show_cljexp [ node ]; *)
    let result = interpret_with_prelude ctx node |> snd in
    (* Functions.failobj __LOC__ [ result ] *)
    obj_to_cljexp result

let main (log : bool) (filename : string) prelude_macros code =
  let prelude_ctx, prelude_sexp =
    prelude_macros
    |> Frontend.parse_and_simplify
         {
           empty_context with
           interpreter =
             (fun ctx node ->
               let ctx, o = interpret_with_prelude ctx node in
               (ctx, obj_to_cljexp o));
         }
         "prelude"
  in
  let prelude_ctx = interpret_with_prelude prelude_ctx prelude_sexp |> fst in
  let rec invoke filename code : context * obj =
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
    |> interpret_with_prelude ctx
  in
  invoke filename code |> snd |> Functions.obj_to_string |> unpack_string |> Scanf.unescaped |> String.trim
