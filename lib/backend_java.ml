open Common

let pkg_name_from_file_name (context : context) = context.base_ns

let rec compile_ (context : context) (node : sexp) : context * string =
  let compile node = compile_ context node |> snd in
  let with_context node = (context, node) in
  let make_operator a b f =
    let _, ar = compile_ context a in
    let _, br = compile_ context b in
    f ar br |> with_context
  in
  match node with
  (* Atoms *)
  | SAtom (_, "nil") -> with_context "null"
  | SAtom (_, "unit") -> with_context "(Object)null"
  | SAtom (_, x) when String.starts_with ~prefix:":" x ->
      "\"" ^ String.sub x 1 (String.length x - 1) ^ "\"" |> with_context
  | SAtom (_, x) when String.starts_with ~prefix:"\"" x -> x |> with_context
  | SAtom (_, x) -> String.map (function '/' -> '.' | x -> x) x |> with_context
  (* Operators *)
  | SList (_, [ SAtom (_, op); a; b ])
    when op = "+" || op = "-" || op = "*" || op = "/" || op = ">" || op = "<" || op = ">=" || op = "<=" ->
      make_operator a b (fun a b -> Printf.sprintf "(%s%s%s)" a op b)
  | SList (_, [ SAtom (_, "let*"); SAtom (m, name) ]) ->
      let js_code = Printf.sprintf "%s %s;" (get_type m) name in
      with_context js_code
  | SList (_, [ SAtom (_, "let*"); SAtom (m, name); value ]) ->
      let js_code = Printf.sprintf "%s %s = %s;" (get_type_or_var m) name (compile value) in
      with_context js_code
  | SList (_, [ SAtom (_, "bind-update*"); SAtom (_, name); value ]) ->
      let js_code = Printf.sprintf "%s = %s;" name (compile value) in
      with_context js_code
  | SList (_, [ SAtom (_, "set!"); name; value ]) ->
      let js_code = Printf.sprintf "%s = %s;" (compile name) (compile value) in
      with_context js_code
  | SList (_, [ SAtom (_, "if*"); (SAtom _ as cond); then_; else_ ]) ->
      Printf.sprintf "if (%s) {\n%s\n} else {\n%s\n}" (compile cond) (compile then_) (compile else_) |> with_context
  | SList (_, [ SAtom (_, "if*"); cond; then_; else_ ]) ->
      Printf.sprintf "if (%s) {\n%s\n} else {\n%s\n}" (compile cond) (compile then_) (compile else_) |> with_context
  | SList (_, [ SAtom (_, "spread"); SAtom (_, value) ]) -> Printf.sprintf "...%s" value |> with_context
  | SList (_, [ SAtom (_, "not"); x ]) -> compile x |> Printf.sprintf "!%s" |> with_context
  | SList (_, [ SAtom (_, "is"); instance; SAtom (tm, type_) ]) ->
      let unescp_type = if String.starts_with ~prefix:"\"" type_ then unpack_string type_ else type_ in
      make_operator instance (SAtom (tm, unescp_type)) (Printf.sprintf "(%s instanceof %s)")
  | SList (_, [ SAtom (_, "as"); instance; SAtom (type_meta, type_) ]) ->
      let unescp_type = if String.starts_with ~prefix:"\"" type_ then unpack_string type_ else type_ in
      make_operator (SAtom (type_meta, unescp_type)) instance (Printf.sprintf "((%s)%s)")
  | SList (_, [ SAtom (_, "quote"); arg ]) -> compile arg |> with_context
  | SList (_, [ SAtom (_, "class-inner"); SAtom (_, cls_name) ]) -> cls_name |> unpack_string |> with_context
  (* Hahs-map *)
  | SList (m, SAtom (_, "hash-map") :: xs) ->
      compile (SList (m, SAtom (unknown_location, "y2k.RT.hash_map") :: xs)) |> with_context
  (* Vector *)
  | SList (m, SAtom (_, "vector") :: xs) ->
      compile (SList (m, SAtom (unknown_location, "java.util.Arrays/asList") :: xs)) |> with_context
  (* Namespaces *)
  | SList (_, SAtom (_, "do*") :: (SList (_, SAtom (_, "ns") :: _) as ns) :: body) ->
      let name_start_pos = (String.rindex_opt context.filename '/' |> Option.value ~default:(-1)) + 1 in
      let filename = String.sub context.filename name_start_pos (String.length context.filename - name_start_pos) in
      let cls_name =
        String.sub filename 0 1 ^ String.sub filename 1 (String.length filename - 5)
        |> String.map (function '.' -> '_' | x -> x)
      in
      let ns_ = compile ns in
      body |> List.fold_left_map compile_ context |> snd
      |> List.reduce_opt (Printf.sprintf "%s\n%s")
      |> Option.value ~default:""
      |> Printf.sprintf "%s\n/** @noinspection ALL*/\npublic class %s{\n%s}" ns_ cls_name
      |> with_context
  | SList (_, [ SAtom (_, "ns"); SList (_, [ SAtom (_, "quote*"); SList (_, SAtom _ :: ns_params) ]) ]) ->
      let imports =
        ns_params
        |> List.map (function
             | SList (_, SAtom (_, ":import") :: imports) ->
                 imports
                 |> List.map (function
                      | SList (_, SAtom (_, pkg) :: classes) ->
                          List.map (fun x -> compile x) classes
                          |> List.map (fun c -> Printf.sprintf "import %s.%s;\n" pkg c)
                          |> List.reduce __LOC__ (Printf.sprintf "%s%s")
                      | n -> failsexp __LOC__ [ n ])
                 |> List.reduce __LOC__ (Printf.sprintf "%s%s")
             | n -> failsexp __LOC__ [ n ])
        |> List.fold_left (Printf.sprintf "%s%s") ""
      in
      let pkg_name = pkg_name_from_file_name context in
      Printf.sprintf "package %s;\n%s" pkg_name imports |> with_context
  | SList (_, SAtom (_, "do*") :: body) ->
      let js_body =
        body |> List.fold_left_map compile_ context |> snd
        |> List.filter (( <> ) "")
        |> List.reduce_opt (Printf.sprintf "%s;\n%s")
        |> Option.value ~default:""
      in
      with_context js_body
  (* Lambda *)
  | SList (m, [ SAtom (_, "fn*"); SList (_, args); body ]) ->
      let sargs =
        args
        |> List.map (function SAtom (_, aname) -> Printf.sprintf "%s" aname | x -> failsexp __LOC__ [ x ])
        |> List.reduce_opt (Printf.sprintf "%s,%s")
        |> Option.value ~default:""
      in
      let body = body |> unwrap_sexp_do in
      let sbody =
        let body = body |> List.concat_map unwrap_sexp_do in
        let length = List.length body in
        body
        |> List.filteri (fun i _ -> i < length - 1)
        |> List.map (fun node -> compile node)
        |> List.reduce_opt (Printf.sprintf "%s;\n%s")
        |> Option.map (Printf.sprintf "%s;\n")
        |> Option.value ~default:""
      in
      (* print_endline @@ "LOG[fn*]: " ^ m.symbol; *)
      let last_exp = body |> List.rev |> List.hd |> compile in
      (match m.symbol with
      | "" -> Printf.sprintf "y2k.RT.fn((%s)->{\n%sreturn %s;\n})" sargs sbody last_exp
      | type_ -> Printf.sprintf "(%s)(%s)->{\n%sreturn %s;\n}" type_ sargs sbody last_exp)
      |> with_context
  (* Constructor *)
  | SList (_, SAtom (_, "new") :: SAtom (_, cnst_name) :: args) ->
      let cnst_name = unpack_string cnst_name in
      let args = args |> List.map compile |> List.reduce_opt (Printf.sprintf "%s,%s") |> Option.value ~default:"" in
      Printf.sprintf "new %s(%s)" cnst_name args |> with_context
  (* Functions *)
  | SList (_, [ SAtom (_, "def*"); SAtom (fname_meta, fname); SList (_, [ SAtom (_, "fn*"); SList (_, args); body ]) ])
    ->
      let context_ref = ref context in
      let context = { context with scope = context.scope |> StringMap.add fname (ONil, context_ref) } in
      context_ref := context;
      let modifier = match fname_meta.symbol with ":private" -> "private" | _ -> "public" in
      let sargs =
        args
        |> List.map (function
             | SAtom (am, aname) -> Printf.sprintf "final %s %s" (get_type am) aname
             | x -> failsexp __LOC__ [ x ])
        |> List.reduce_opt (Printf.sprintf "%s,%s")
        |> Option.value ~default:""
      in
      let body = body |> unwrap_sexp_do in
      let sbody =
        let length = List.length body in
        body
        |> List.filteri (fun i _ -> i < length - 1)
        |> List.fold_left_map compile_ context |> snd
        |> List.reduce_opt (Printf.sprintf "%s;\n%s")
        |> Option.map (Printf.sprintf "%s;\n")
        |> Option.value ~default:""

        (* let body_rev = unwrap_sexp_do body |> List.rev in
        let last_body = body_rev |> List.hd |> compile |> Printf.sprintf "return %s" in
        match List.tl body_rev |> List.rev with
        | [] -> last_body
        | xs ->
            let body = SList (meta_empty, SAtom (meta_empty, "do*") :: xs) |> compile in
            Printf.sprintf "%s;\n%s" body last_body *)
      in
      let return_ = match fname_meta.symbol with "void" -> "" | _ -> "return " in
      let last_exp = body |> List.rev |> List.hd |> compile_ context |> snd in
      let code =
        Printf.sprintf "%s static %s %s (%s) {\n%s%s%s;\n}\n" modifier (get_type fname_meta) fname sargs sbody return_
          last_exp
      in
      (context, code)
  (* Static field *)
  | SList (_, [ SAtom (_, "def*"); SAtom (fname_meta, fname); body ]) ->
      let vis = if fname_meta.symbol = ":private" then "private" else "public" in
      let get_type am = if am.symbol = "" || am.symbol = ":private" then "Object" else am.symbol in
      let context_ref = ref context in
      let context = { context with scope = context.scope |> StringMap.add fname (ONil, context_ref) } in
      context_ref := context;
      let result =
        Printf.sprintf "%s static %s %s=%s;" vis (get_type fname_meta) fname (compile_ context body |> snd)
      in
      (context, result)
  (* Empty declaration *)
  | SList (_, [ SAtom (_, "def*"); SAtom (_, _name) ]) ->
      ({ context with scope = context.scope |> StringMap.add _name (ONil, ref context) }, "")
  (* Interop field *)
  | SList (_, [ SAtom (_, "."); target; SAtom (_, field) ]) when String.starts_with ~prefix:":-" field ->
      Printf.sprintf "%s.%s" (compile target) (String.sub field 2 (String.length field - 2)) |> with_context
  (* Interop method *)
  | SList (_, SAtom (_, ".") :: target :: SAtom (_, mname) :: args) ->
      let sargs =
        match args with [] -> "" | args -> args |> List.map compile |> List.reduce __LOC__ (Printf.sprintf "%s, %s")
      in
      let result =
        match get_sexp_symbol target with
        | "" -> Printf.sprintf "%s.%s(%s)" (compile target) (unpack_symbol mname) sargs
        | type_ -> Printf.sprintf "((%s)%s).%s(%s)" type_ (compile target) (unpack_symbol mname) sargs
      in
      result |> with_context
  | SList (_, SAtom (_, "gen-class-inner") :: _) as node -> Macro_gen_class.invoke compile context node
  (* Call runtime function *)
  | SList (m2, SAtom (m, "call-runtime") :: SList (_, [ _; SAtom (_, name) ]) :: args) ->
      compile (SList (m2, SAtom (m, "y2k.RT/" ^ name) :: args)) |> with_context
  (* Function call *)
  | SList (_, head :: args) ->
      let sargs = args |> List.map compile |> String.concat ",\n" in
      let sargs = if sargs = "" then ")" else "\n" ^ sargs ^ ")" in
      let fname =
        match head with
        | SList (_, SAtom (_, "fn*") :: _) -> "(" ^ compile head ^ ")("
        | SAtom (_, fname) when String.contains fname '/' -> String.map (function '/' -> '.' | x -> x) fname ^ "("
        | SAtom (_, fname) when String.contains fname '.' -> String.map (function '/' -> '.' | x -> x) fname ^ "("
        | SAtom (_, fname) when not (StringMap.mem fname context.scope) ->
            (* prerr_endline @@ "LOG: " ^ fname ^ " ["
            ^ (context.scope |> StringMap.bindings |> List.map fst |> String.concat ", ")
            ^ "]"; *)
            Printf.sprintf "y2k.RT.invoke(%s%s" fname (if List.is_empty args then "" else ", ")
        | _ -> compile head ^ "("
      in
      fname ^ sargs |> with_context
  | n -> failsexp __LOC__ [ n ]

let rec make_scope_for_prelude (context : context) node =
  match node with
  | SList (_, SAtom (_, "do*") :: body) ->
      body |> List.fold_left_map (fun context n -> (make_scope_for_prelude context n, n)) context |> fst
  | x -> failsexp __LOC__ [ x ]

let main base_ns (log : bool) (filename : string) prelude_macros code =
  let prelude_ctx, prelude_sexp =
    prelude_macros
    |> Frontend.parse_and_simplify
         { empty_context with interpreter = Backend_interpreter.mk_interpret; eval = Backend_interpreter.mk_eval () }
         "prelude"
  in
  let ctx, node = code |> Frontend.desugar config_default log prelude_sexp { prelude_ctx with base_ns } filename in
  node |> Stage_java_require.main ctx
  |> try_slog "Stage_java_require      ->" log
  |> Stage_convert_if_to_statment.invoke
  |> try_slog "Stage_a_normal_form     ->" log
  |> compile_ ctx |> snd |> String.trim
