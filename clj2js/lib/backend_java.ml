open Common

let unpack_string x = String.sub x 1 (String.length x - 2)
let unpack_symbol x = String.sub x 1 (String.length x - 1)
let get_type meta = if meta.symbol = "" || meta.symbol = ":private" then "Object" else meta.symbol
let get_type_or_var meta = if meta.symbol = "" || meta.symbol = ":private" then "var" else meta.symbol

let generate_class (compile_exp : cljexp -> string) prefix params clsName methods superCls =
  let prefix = String.sub prefix 1 (String.length prefix - 2) in
  let cnt_params =
    match params with
    | [] -> ""
    | params ->
        params
        |> List.mapi (fun i x ->
               let type1 = compile_exp x in
               let type2 =
                 if String.starts_with ~prefix:"\"" type1 then String.sub type1 1 (String.length type1 - 2) else type1
               in
               Printf.sprintf "%s p%i" type2 i)
        |> List.reduce __LOC__ (Printf.sprintf "%s,%s")
  in
  let state =
    match params with
    | [] -> ""
    | params ->
        params
        |> List.mapi (fun i _ -> Printf.sprintf "p%i" i)
        |> List.reduce __LOC__ (Printf.sprintf "%s,%s")
        |> Printf.sprintf "public %s(%s) {\nstate=java.util.List.of(%s);\n}\n" clsName cnt_params
  in
  let ms =
    methods
    |> List.map (function
         | SBList (_, [ Atom (m, mname); SBList (_, args); Atom (_, rtype) ]) ->
             let args_ =
               args
               |> List.mapi (fun i a ->
                      match a with
                      | Atom (_, a) when String.starts_with ~prefix:"\"" a ->
                          Printf.sprintf "%s p%i" (unpack_string a) i
                      | Atom (_, a) -> Printf.sprintf "%s p%i" a i
                      | x -> failnode __LOC__ [ x ])
               |> List.reduce_opt (Printf.sprintf "%s, %s")
               |> Option.value ~default:""
             in
             let args__ =
               args
               |> List.mapi (fun i _ -> Printf.sprintf "p%i" i)
               |> List.reduce_opt (Printf.sprintf "%s, %s")
               |> Option.value ~default:""
             in
             let annot = match m.symbol with "" -> "" | x -> "@" ^ x ^ " " in
             let return_ = if rtype = "void" then "" else Printf.sprintf "return (%s)" rtype in
             let call_super = if annot = "@Override " then Printf.sprintf "super.%s(%s);\n" mname args__ else "" in
             let full_args = match args__ with "" -> "this" | x -> "this, " ^ x in
             Printf.sprintf "%spublic %s %s(%s) {\n%s%s%s%s(%s); }\n" annot rtype mname args_ call_super return_ prefix
               mname full_args
         | x -> failnode __LOC__ [ x ])
    |> List.reduce __LOC__ (Printf.sprintf "%s%s")
  in
  Printf.sprintf "public static class %s extends %s {\npublic java.util.List<Object> state;\n%s%s}" clsName superCls
    state ms

let pkg_name_from_file_name (context : context) = context.base_ns

let rec compile_ (context : context) (node : cljexp) : context * string =
  let compile node = compile_ context node |> snd in
  let with_context node = (context, node) in
  let make_operator a b f =
    let _, ar = compile_ context a in
    let _, br = compile_ context b in
    f ar br |> with_context
  in
  match node with
  (* Atoms *)
  | Atom (_, "nil") -> with_context "null"
  | Atom (_, "unit") -> with_context "(Object)null"
  | Atom (_, x) when String.starts_with ~prefix:":" x ->
      "\"" ^ String.sub x 1 (String.length x - 1) ^ "\"" |> with_context
  | Atom (_, x) when String.starts_with ~prefix:"\"" x -> x |> with_context
  | Atom (_, x) -> String.map (function '/' -> '.' | x -> x) x |> with_context
  (* Operators *)
  | RBList (_, [ Atom (_, op); a; b ])
    when op = "+" || op = "-" || op = "*" || op = "/" || op = ">" || op = "<" || op = ">=" || op = "<=" ->
      make_operator a b (fun a b -> Printf.sprintf "(%s%s%s)" a op b)
  (* Version 2.0 *)
  | RBList (_, [ Atom (_, "let*"); Atom (m, name) ]) ->
      let js_code = Printf.sprintf "%s %s;" (get_type m) name in
      with_context js_code
  | RBList (_, [ Atom (_, "let*"); Atom (m, name); value ]) ->
      let js_code = Printf.sprintf "%s %s = %s;" (get_type_or_var m) name (compile value) in
      with_context js_code
  | RBList (_, [ Atom (_, "bind-update*"); Atom (_, name); value ]) ->
      let js_code = Printf.sprintf "%s = %s;" name (compile value) in
      with_context js_code
  | RBList (_, [ Atom (_, "set!"); Atom (_, name); value ]) ->
      let js_code = Printf.sprintf "%s = %s;" name (compile value) in
      with_context js_code
  | RBList (_, [ Atom (_, "set!"); name; value ]) ->
      let js_code = Printf.sprintf "%s = %s;" (compile name) (compile value) in
      with_context js_code
  | RBList (_, [ Atom (_, "if*"); (Atom _ as cond); then_; else_ ]) ->
      Printf.sprintf "if (%s) {\n%s\n} else {\n%s\n}" (compile cond) (compile then_) (compile else_) |> with_context
  | RBList (_, [ Atom (_, "if*"); cond; then_; else_ ]) ->
      Printf.sprintf "if (%s) {\n%s\n} else {\n%s\n}" (compile cond) (compile then_) (compile else_) |> with_context
  | RBList (_, [ Atom (_, "spread"); Atom (_, value) ]) -> Printf.sprintf "...%s" value |> with_context
  (* /Version 2.0 *)
  | RBList (_, [ Atom (_, "not"); x ]) -> compile x |> Printf.sprintf "!%s" |> with_context
  | RBList (_, [ Atom (_, "is"); instance; Atom (tm, type_) ]) ->
      let unescp_type = if String.starts_with ~prefix:"\"" type_ then unpack_string type_ else type_ in
      make_operator instance (Atom (tm, unescp_type)) (Printf.sprintf "(%s instanceof %s)")
  | RBList (_, [ Atom (_, "as"); instance; Atom (type_meta, type_) ]) ->
      let unescp_type = if String.starts_with ~prefix:"\"" type_ then unpack_string type_ else type_ in
      make_operator (Atom (type_meta, unescp_type)) instance (Printf.sprintf "((%s)%s)")
  | RBList (_, [ Atom (_, "quote"); arg ]) -> compile arg |> with_context
  | RBList (_, [ Atom (_, "class-inner"); RBList (_, [ _; Atom (_, cls_name) ]) ]) -> cls_name |> with_context
  (* Hahs-map *)
  | RBList (m, Atom (_, "hash-map") :: xs) ->
      compile (RBList (m, Atom (unknown_location, "y2k.RT.hash_map") :: xs)) |> with_context
  | CBList (_, xs) -> failnode __LOC__ xs
  (* Vector *)
  | RBList (m, Atom (_, "vector") :: xs) ->
      compile (RBList (m, Atom (unknown_location, "java.util.Arrays/asList") :: xs)) |> with_context
  | SBList (_, xs) -> failnode __LOC__ xs
  (* Namespaces *)
  | RBList (_, Atom (_, "do*") :: (RBList (_, Atom (_, "ns") :: _) as ns) :: body) ->
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
  | RBList (_, [ Atom (_, "ns"); RBList (_, [ Atom (_, "quote*"); RBList (_, Atom _ :: ns_params) ]) ]) ->
      let imports =
        ns_params
        |> List.map (function
             | RBList (_, Atom (_, ":import") :: imports) ->
                 imports
                 |> List.map (function
                      | SBList (_, Atom (_, pkg) :: classes) ->
                          List.map (fun x -> compile x) classes
                          |> List.map (fun c -> Printf.sprintf "import %s.%s;\n" pkg c)
                          |> List.reduce __LOC__ (Printf.sprintf "%s%s")
                      | n -> failnode __LOC__ [ n ])
                 |> List.reduce __LOC__ (Printf.sprintf "%s%s")
             | n -> failnode __LOC__ [ n ])
        |> List.fold_left (Printf.sprintf "%s%s") ""
      in
      let pkg_name = pkg_name_from_file_name context in
      Printf.sprintf "package %s;\n%s" pkg_name imports |> with_context
  | RBList (_, Atom (_, "do*") :: body) ->
      let js_body =
        body |> List.fold_left_map compile_ context |> snd
        |> List.filter (( <> ) "")
        |> List.reduce_opt (Printf.sprintf "%s;\n%s")
        |> Option.value ~default:""
      in
      with_context js_body
  (* Lambda *)
  | RBList (m, Atom (_, "fn*") :: RBList (_, args) :: body) ->
      let sargs =
        args
        |> List.map (function Atom (_, aname) -> Printf.sprintf "%s" aname | x -> failnode __LOC__ [ x ])
        |> List.reduce_opt (Printf.sprintf "%s,%s")
        |> Option.value ~default:""
      in
      let body = body |> List.concat_map unwrap_do in
      let sbody =
        let body = body |> List.concat_map unwrap_do in
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
  | RBList (_, Atom (_, "new") :: Atom (_, cnst_name) :: args) ->
      let cnst_name = unpack_string cnst_name in
      let args = args |> List.map compile |> List.reduce_opt (Printf.sprintf "%s,%s") |> Option.value ~default:"" in
      Printf.sprintf "new %s(%s)" cnst_name args |> with_context
  (* Functions *)
  | RBList (_, [ Atom (_, "def*"); Atom (fname_meta, fname); RBList (_, Atom (_, "fn*") :: RBList (_, args) :: body) ])
    ->
      let context = { context with scope = StringMap.add fname (node, context) context.scope } in
      let modifier = match fname_meta.symbol with ":private" -> "private" | _ -> "public" in
      let sargs =
        args
        |> List.map (function
             | Atom (am, aname) -> Printf.sprintf "final %s %s" (get_type am) aname
             | x -> failnode __LOC__ [ x ])
        |> List.reduce_opt (Printf.sprintf "%s,%s")
        |> Option.value ~default:""
      in
      let body = body |> List.concat_map unwrap_do in
      let sbody =
        let length = List.length body in
        body
        |> List.filteri (fun i _ -> i < length - 1)
        |> List.fold_left_map compile_ context |> snd
        |> List.reduce_opt (Printf.sprintf "%s;\n%s")
        |> Option.map (Printf.sprintf "%s;\n")
        |> Option.value ~default:""
      in
      let return_ = match fname_meta.symbol with "void" -> "" | _ -> "return " in
      let last_exp = body |> List.rev |> List.hd |> compile in
      let code =
        Printf.sprintf "%s static %s %s (%s) {\n%s%s%s;\n}\n" modifier (get_type fname_meta) fname sargs sbody return_
          last_exp
      in
      (context, code)
  (* Static field *)
  | RBList (_, [ Atom (_, "def*"); Atom (fname_meta, fname); body ]) ->
      let context = { context with scope = StringMap.add fname (node, context) context.scope } in
      let vis = if fname_meta.symbol = ":private" then "private" else "public" in
      let get_type am = if am.symbol = "" || am.symbol = ":private" then "Object" else am.symbol in
      let result =
        Printf.sprintf "%s static %s %s=%s;" vis (get_type fname_meta) fname (compile_ context body |> snd)
      in
      (context, result)
  (* Empty declaration *)
  | RBList (_, [ Atom (_, "def*"); Atom (_, name) ]) ->
      ({ context with scope = StringMap.add name (node, context) context.scope }, "")
  (* Interop field *)
  | RBList (_, [ Atom (_, "."); target; Atom (_, field) ]) when String.starts_with ~prefix:":-" field ->
      Printf.sprintf "%s.%s" (compile target) (String.sub field 2 (String.length field - 2)) |> with_context
  (* Interop method *)
  | RBList (_, Atom (_, ".") :: target :: Atom (_, mname) :: args) ->
      let sargs =
        match args with [] -> "" | args -> args |> List.map compile |> List.reduce __LOC__ (Printf.sprintf "%s, %s")
      in
      let result =
        match get_symbol target with
        | "" -> Printf.sprintf "%s.%s(%s)" (compile target) (unpack_symbol mname) sargs
        | type_ -> Printf.sprintf "((%s)%s).%s(%s)" type_ (compile target) (unpack_symbol mname) sargs
      in
      result |> with_context
  | RBList
      ( _,
        [
          Atom (_, "gen-class-inner");
          RBList
            ( _,
              [
                _;
                RBList
                  ( _,
                    [
                      Atom (_, ":name");
                      Atom (_, clsName);
                      Atom (_, ":extends");
                      Atom (_, superCls);
                      Atom (_, ":constructors");
                      CBList (_, [ SBList (_, params); SBList _ ]);
                      Atom (_, ":prefix");
                      Atom (_, prefix);
                      Atom (_, ":methods");
                      SBList (_, methods);
                    ] );
              ] );
        ] ) ->
      generate_class compile prefix params clsName methods superCls |> with_context
  (* Call runtime function *)
  | RBList (m2, Atom (m, "call-runtime") :: RBList (_, [ _; Atom (_, name) ]) :: args) ->
      compile (RBList (m2, Atom (m, "y2k.RT/" ^ name) :: args)) |> with_context
  (* Function call *)
  | RBList (_, head :: args) ->
      let sargs =
        if List.length args = 0 then "" else args |> List.map compile |> List.reduce __LOC__ (Printf.sprintf "%s, %s")
      in
      let fname =
        match head with
        | RBList (_, Atom (_, "fn*") :: _) -> "(" ^ compile head ^ ")("
        | Atom (_, fname) when String.contains fname '/' -> String.map (function '/' -> '.' | x -> x) fname ^ "("
        | Atom (_, fname) when String.contains fname '.' -> String.map (function '/' -> '.' | x -> x) fname ^ "("
        | Atom (_, fname) when not (StringMap.mem fname context.scope) ->
            print_endline @@ "LOG: " ^ fname ^ " ["
            ^ (context.scope |> StringMap.bindings |> List.map fst |> String.concat ", ")
            ^ "]";
            Printf.sprintf "y2k.RT.invoke(%s%s" fname (if List.is_empty args then "" else ", ")
        | _ -> compile head ^ "("
      in
      fname ^ sargs ^ ")" |> with_context
  | n -> failnode __LOC__ [ n ]

let rec make_scope_for_prelude (context : context) node =
  match node with
  | RBList (_, Atom (_, "do*") :: body) ->
      body |> List.fold_left_map (fun context n -> (make_scope_for_prelude context n, n)) context |> fst
  | RBList (_, Atom (_, "def*") :: Atom (_, name) :: _) ->
      { context with scope = StringMap.add name (node, context) context.scope }
  | x -> failnode __LOC__ [ x ]

let main base_ns (log : bool) (filename : string) prelude_macros code =
  let macros_ctx, _macro_sexp =
    prelude_macros
    |> Frontend.parse_and_simplify { empty_context with interpreter = Backend_interpreter.interpret } "prelude"
  in
  let ctx, node = code |> Frontend.parse_and_simplify { macros_ctx with log; base_ns } filename in
  node
  |> try_log "parse_and_simplify      ->" log
  |> Stage_simplify_let.invoke
  |> try_log "Stage_simplify_let      ->" log
  |> Stage_normalize_bracket.invoke
  |> try_log "Stage_normalize_bracket ->" log
  |> Stage_linter.invoke ctx _macro_sexp
  (* *)
  |> Stage_java_require.main ctx
  |> try_log "Stage_java_require      ->" log
  |> Stage_convert_if_to_statment.invoke
  |> try_log "Stage_a_normal_form     ->" log
  |> compile_ (make_scope_for_prelude ctx _macro_sexp)
  |> snd |> String.trim
