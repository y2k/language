open Common

module Functions = struct
  let rec sexp_to_string = function
    | SAtom (_, s) -> s
    | SList (_, xs) -> "(" ^ String.concat " " (List.map sexp_to_string xs) ^ ")"

  let rec sexp_to_string2 = function
    | SAtom (_, s) when String.starts_with ~prefix:":" s -> unpack_symbol s
    | SAtom (_, s) when String.starts_with ~prefix:"\"" s -> unpack_string s
    | SAtom (_, s) -> s
    | SList (_, xs) -> "(" ^ String.concat " " (List.map sexp_to_string2 xs) ^ ")"

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
    | OQuote n -> "(quote " ^ debug_show_sexp1 n ^ ")"

  let rec obj_to_string = function
    | OList xs -> "(" ^ String.concat " " (List.map obj_to_string xs) ^ ")"
    | OVector xs -> "[" ^ String.concat " " (List.map obj_to_string xs) ^ "]"
    | OMap xs -> "{" ^ String.concat " " (List.map (fun (k, v) -> obj_to_string k ^ " " ^ obj_to_string v) xs) ^ "}"
    | OString s -> "\"" ^ s ^ "\""
    | OInt i -> string_of_int i
    | OBool b -> string_of_bool b
    | ONil -> "nil"
    | OLambda _ -> "lambda"
    | OQuote n -> "(quote " ^ debug_show_sexp [ n ] ^ ")"

  let failobj loc objs = failwith @@ loc ^ " - " ^ String.concat " " (List.map debug_obj_to_string objs)

  let functions : (obj list -> obj) StringMap.t =
    StringMap.empty
    |> StringMap.add "vector" (fun args -> OVector args)
    |> StringMap.add "list" (fun args -> OList args)
    |> StringMap.add "hash-map" (fun args -> OMap (args |> List.split_into_pairs))
    |> StringMap.add "str" (fun args ->
           args
           |> List.map (function
                | OString x -> x
                | OInt x -> string_of_int x
                | OQuote x -> show_sexp2 x
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

let attach_meta m = function OQuote x -> OQuote (change_smeta m x) | x -> x

let rec interpret (context : context) (node : sexp) : context * obj =
  (* log_sexp2 "INTERPRET:" node |> ignore; *)
  let interpret_ x = interpret context x |> snd in
  let with_context x = (context, x) in
  match node with
  | SAtom (_, "true") -> (context, OBool true)
  | SAtom (_, "false") -> (context, OBool false)
  | SAtom (_, "nil") -> (context, ONil)
  | SAtom (_, v) when String.starts_with ~prefix:"\"" v -> (context, OString (unpack_string v))
  | SAtom (_, v) when String.starts_with ~prefix:":" v -> (context, OString (unpack_symbol v))
  | SAtom (m, x) when String.starts_with ~prefix:"'" x -> (context, OQuote (SAtom (m, unpack_symbol x)))
  | SAtom (_, v)
    when (String.starts_with ~prefix:"-" v && v <> "-")
         ||
         let ch = String.get v 0 in
         ch >= '0' && ch <= '9' ->
      (* prerr_endline @@ "INTERPRET: INT " ^ debug_show_cljexp [ node ]; *)
      (context, OInt (int_of_string v))
  | SList (_, [ SAtom (_, "transform_nodes"); SList (_, _ :: opt); xs ]) ->
      let rec unpack_to_map = function
        | [] -> []
        | SAtom (_, k) :: v :: tail -> (k, v) :: unpack_to_map tail
        | n -> failsexp __LOC__ n
      in
      let xs =
        match interpret_ xs with
        | OList xs -> xs
        | n ->
            (* prerr_endline @@ __LOC__ ^ " ["
            ^ (StringMap.find "xs" context.scope |> fst |> Functions.debug_obj_to_string)
            ^ "]"; *)
            (* failsexp __LOC__ [xs] |> ignore; *)
            Functions.failobj __LOC__ [ n ]
      in
      let sep =
        unpack_to_map opt |> List.assoc_opt ":sep"
        |> ( function Some x -> x | None -> failsexp __LOC__ opt )
        |> interpret_
      in
      let len = List.length xs in
      let r =
        xs |> List.mapi (fun i x -> (i, x)) |> List.concat_map (fun (i, x) -> if i < len - 1 then [ x; sep ] else [ x ])
      in
      (context, OList r)
  (* Constants *)
  | SAtom (_, "__FILENAME__") -> (context, OString context.filename)
  | SAtom (_, "__LINE__") -> OInt (context.loc.line - context.start_line) |> with_context
  | SAtom (_, "__POSITION__") -> (context, OInt context.loc.pos)
  (* /Constants *)
  (* Resolve scope value *)
  | SAtom (m, x) when StringMap.exists (fun k _ -> k = x) context.scope -> (
      match StringMap.find x context.scope with
      | arg_val, ctx ->
          (* prerr_endline @@ "- VALUE: " ^ Functions.debug_obj_to_string arg_val; *)
          (!ctx, attach_meta m arg_val))
  | SAtom (_, name) when String.contains name '/' ->
      let parts = String.split_on_char '/' name in
      (* prerr_endline @@ "SCOPE: [" ^ debug_show_imports context ^ "] " ^ List.hd parts; *)
      let context = StringMap.find (List.hd parts) context.imports in
      let fn, fn_ctx = StringMap.find (List.nth parts 1) context.scope in
      (!fn_ctx, fn)
  (* /Resolve scope value *)
  (* SPECIAL FORMS *)
  | SList (_, [ SAtom (_, "quote*"); arg ]) -> OQuote arg |> with_context
  | SList (_, SAtom (_, "do*") :: body) ->
      let context2, results = body |> List.fold_left_map (fun ctx x -> interpret ctx x) context in
      let last_node = results |> List.rev |> List.hd in
      (context2, last_node)
  | SList (_, [ SAtom (_, "if*"); c; a; b ]) -> (
      match interpret_ c with
      | OBool true -> (context, interpret_ a)
      | OBool false -> (context, interpret_ b)
      | _ -> failwith __LOC__)
  | SList (_, [ SAtom (_, "def*"); SAtom (_, name); body ]) ->
      let body = interpret context body |> snd in
      let ctx_ref = ref context in
      let context = { context with scope = context.scope |> StringMap.add name (body, ctx_ref) } in
      ctx_ref := context;
      (context, ONil)
  | SList (_, [ SAtom (_, "let*"); SAtom (_, name); body ]) ->
      let body = interpret context body |> snd in
      let context = { context with scope = context.scope |> StringMap.add name (body, ref context) } in
      (context, ONil)
  | SList (_, SAtom (_, "let*") :: SList (_, bindings) :: body) ->
      let scope =
        bindings |> List.split_into_pairs
        |> List.fold_left
             (fun ctx (k, v) ->
               let name = match k with SAtom (_, s) -> s | n -> failsexp __LOC__ [ n ] in
               let v = interpret { context with scope = ctx } v |> snd in
               StringMap.add name (v, ref context) ctx)
             context.scope
      in
      let _, results = body |> List.fold_left_map (fun ctx x -> interpret ctx x) { context with scope } in
      results |> List.rev |> List.hd |> with_context
  (* Lambda *)
  | SList (_, SAtom (_, "fn*") :: args :: body) ->
      let arg_names =
        (match args with SList (_, x) -> x | x -> failsexp __LOC__ [ x ])
        |> List.map (function SAtom (_, x) -> x | n -> failsexp __LOC__ [ n ])
      in
      let f =
       fun (args : obj list) : obj ->
        let scope =
          List.map2 (fun n v -> (n, v)) arg_names args
          |> List.fold_left (fun scope (n, v) -> StringMap.add n (v, ref context) scope) context.scope
        in
        let context = { context with scope } in
        interpret context (SList (meta_empty, SAtom (meta_empty, "do*") :: body)) |> snd
      in
      OLambda f |> with_context
  (* Function call *)
  | SList (_, target :: args) as fc ->
      let target = interpret_ target in
      let arg_values = List.map interpret_ args in
      (match target with OLambda f -> f arg_values | _ -> failsexp __LOC__ [ fc ]) |> with_context
  | SAtom (_, fname) when StringMap.mem fname context.functions ->
      (* prerr_endline @@ "- FUNCTION: " ^ fname; *)
      let f, _ctx = StringMap.find fname context.functions in
      let f = f |> Stage_normalize_bracket.invoke |> Stage_simplify_let.invoke in
      interpret context f
  | node ->
      prerr_endline @@ "==========================================";
      prerr_endline @@ "FUNCS: [" ^ (context.functions |> StringMap.bindings |> List.map fst |> String.concat ",") ^ "]";
      prerr_endline @@ "SCOPE: [" ^ debug_show_scope context ^ "]";
      prerr_endline @@ "MACROS: [" ^ debug_show_macro context ^ "]";
      prerr_endline @@ "IMPORTS: [" ^ debug_show_imports context ^ "]";
      failsexp __LOC__ [ node ]

let interpret_with_prelude (context : context) node : context * obj =
  let scope =
    Functions.functions |> StringMap.bindings
    |> List.fold_left (fun scope (fname, f) -> StringMap.add fname (OLambda f, ref context) scope) context.scope
  in
  interpret { context with scope } node

(* let rec obj_to_sexp = function
  | OString x -> SAtom (meta_empty, "\"" ^ x ^ "\"")
  | OInt x -> SAtom (meta_empty, string_of_int x)
  | OQuote x -> x
  | OList xs -> SList (meta_empty, List.map obj_to_sexp xs)
  | OVector xs -> SList (meta_empty, SAtom (meta_empty, "vector") :: List.map obj_to_sexp xs)
  | OMap xs ->
      SList
        ( meta_empty,
          SAtom (meta_empty, "hash-map") :: (xs |> List.concat_map (fun (k, v) -> [ obj_to_sexp k; obj_to_sexp v ])) )
  (* | OVector xs -> SBList (meta_empty, List.map obj_to_sexp xs)
  | OMap xs -> CBList (meta_empty, xs |> List.concat_map (fun (k, v) -> [ obj_to_sexp k; obj_to_sexp v ])) *)
  | n -> failwith @@ __LOC__ ^ " - " ^ show_obj n *)

let rec obj_to_sexp node =
  let rec sexp_to_cljexp = function
    | SAtom (m, x) -> Atom (m, x)
    | SList (m, SAtom (_, "vector") :: xs) -> SBList (m, List.map sexp_to_cljexp xs)
    | SList (m, xs) -> RBList (m, List.map sexp_to_cljexp xs)
  in
  match node with
  | OString x -> Atom (meta_empty, "\"" ^ x ^ "\"")
  | OInt x -> Atom (meta_empty, string_of_int x)
  | OQuote x -> sexp_to_cljexp x
  | OList xs -> RBList (meta_empty, List.map obj_to_sexp xs)
  | OVector xs -> SBList (meta_empty, List.map obj_to_sexp xs)
  | OMap xs -> CBList (meta_empty, xs |> List.concat_map (fun (k, v) -> [ obj_to_sexp k; obj_to_sexp v ]))
  | n -> failwith @@ __LOC__ ^ " - " ^ show_obj n

let mk_interpret context (code : cljexp) =
  let code = Stage_normalize_bracket.invoke code in
  let code = Stage_simplify_let.invoke code in
  let ctx, result = interpret_with_prelude context code in
  (ctx, obj_to_sexp result)

let mk_eval () =
  let ctx, _ = Frontend.parse_and_simplify empty_context "prelude.clj" Preludes.interpreter in
  fun _ node ->
    (* prerr_endline @@ "LOG:EVAL:1: " ^ debug_show_cljexp [ node ]; *)
    let ctx, node = Frontend.desugar_and_register ctx node in
    (* prerr_endline @@ "LOG:EVAL:2: " ^ debug_show_cljexp [ node ]; *)
    (* let node = Stage_simplify_let.invoke node in *)
    let node = Stage_normalize_bracket.invoke node in
    let result = interpret_with_prelude ctx node |> snd in
    (* Functions.failobj __LOC__ [ result ] *)
    obj_to_sexp result

let main (log : bool) (filename : string) prelude_macros code =
  let prelude_ctx, prelude_sexp =
    prelude_macros
    |> Frontend.parse_and_simplify
         {
           empty_context with
           interpreter =
             (fun ctx node ->
               let node = Stage_normalize_bracket.invoke node in
               let node = Stage_simplify_let.invoke node in
               let ctx, o = interpret_with_prelude ctx node in
               (ctx, obj_to_sexp o));
         }
         "prelude"
  in
  let prelude_ctx = Stage_normalize_bracket.invoke prelude_sexp |> interpret_with_prelude prelude_ctx |> fst in
  let rec invoke filename code : context * obj =
    let ctx, node = code |> Frontend.desugar log prelude_sexp prelude_ctx filename in
    node |> Stage_ns_inline.invoke invoke ctx |> fun (ctx, node) ->
    node |> try_slog "Stage_ns_inline                ->" log |> interpret_with_prelude ctx
  in
  invoke filename code |> snd |> Functions.obj_to_string |> unpack_string |> Scanf.unescaped |> String.trim
