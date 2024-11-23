open Common

let unpack_string x = String.sub x 1 (String.length x - 2)
let unpack_symbol x = String.sub x 1 (String.length x - 1)

let get_type meta =
  if meta.symbol = "" || meta.symbol = ":private" then "Object" else meta.symbol

let get_type_or_var meta =
  if meta.symbol = "" || meta.symbol = ":private" then "var" else meta.symbol

let generate_class (compile_exp : cljexp -> string) prefix params clsName
    methods superCls =
  let prefix = String.sub prefix 1 (String.length prefix - 2) in
  let cnt_params =
    match params with
    | [] -> ""
    | params ->
        params
        |> List.mapi (fun i x ->
               let type1 = compile_exp x in
               let type2 =
                 if String.starts_with ~prefix:"\"" type1 then
                   String.sub type1 1 (String.length type1 - 2)
                 else type1
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
        |> Printf.sprintf "public %s(%s) {\nstate=java.util.List.of(%s);\n}"
             clsName cnt_params
  in
  let ms =
    methods
    |> List.map (function
         | SBList [ Atom (m, mname); SBList args; Atom (_, rtype) ] ->
             let args_ =
               args
               |> List.mapi (fun i a ->
                      match a with
                      | Atom (_, a) when String.starts_with ~prefix:"\"" a ->
                          Printf.sprintf "%s p%i" (unpack_string a) i
                      | Atom (_, a) -> Printf.sprintf "%s p%i" a i
                      | x -> failnode __LOC__ [ x ])
               |> List.reduce __LOC__ (Printf.sprintf "%s, %s")
             in
             let args__ =
               args
               |> List.mapi (fun i _ -> Printf.sprintf "p%i" i)
               |> List.reduce __LOC__ (Printf.sprintf "%s, %s")
             in
             let annot = match m.symbol with "" -> "" | x -> "@" ^ x ^ " " in
             let return_ =
               if rtype = "void" then "" else Printf.sprintf "return (%s)" rtype
             in
             let call_super =
               if annot = "@Override " then
                 Printf.sprintf "super.%s(%s);\n" mname args__
               else ""
             in
             Printf.sprintf "%spublic %s %s(%s) {\n%s%s%s%s(this, %s); }\n"
               annot rtype mname args_ call_super return_ prefix mname args__
         | x -> failnode __LOC__ [ x ])
    |> List.reduce __LOC__ (Printf.sprintf "%s%s")
  in
  Printf.sprintf
    "public static class %s extends %s {\n\
     public java.util.List<Object> state;\n\
     %s%s}"
    clsName superCls state ms

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
  | Atom (_, "unit") -> with_context "(Object)null"
  | Atom (_, x) when String.starts_with ~prefix:":" x ->
      "\"" ^ String.sub x 1 (String.length x - 1) ^ "\"" |> with_context
  | Atom (_, x) when String.starts_with ~prefix:"\"" x -> x |> with_context
  | Atom (_, x) -> String.map (function '/' -> '.' | x -> x) x |> with_context
  (* Operators *)
  | RBList [ Atom (_, op); a; b ]
    when op = "+" || op = "-" || op = "*" || op = "/" || op = ">" || op = "<"
         || op = ">=" || op = "<=" ->
      make_operator a b (fun a b -> Printf.sprintf "(%s%s%s)" a op b)
  (* Version 2.0 *)
  | RBList [ Atom (_, "bind*"); Atom (m, name) ] ->
      let js_code = Printf.sprintf "%s %s;" (get_type m) name in
      with_context js_code
  | RBList [ Atom (_, "bind*"); Atom (m, name); value ] ->
      let js_code =
        Printf.sprintf "%s %s = %s;" (get_type_or_var m) name (compile value)
      in
      with_context js_code
  | RBList [ Atom (_, "bind-update*"); Atom (_, name); value ] ->
      let js_code = Printf.sprintf "%s = %s;" name (compile value) in
      with_context js_code
  | RBList [ Atom (_, "if*"); (Atom _ as cond); then_; else_ ] ->
      Printf.sprintf "if (%s) {\n%s\n} else {\n%s\n}" (compile cond)
        (compile then_) (compile else_)
      |> with_context
  | RBList [ Atom (_, "spread"); Atom (_, value) ] ->
      Printf.sprintf "...%s" value |> with_context
  (* /Version 2.0 *)
  | RBList [ Atom (_, "not"); x ] ->
      compile x |> Printf.sprintf "!%s" |> with_context
  | RBList [ Atom (_, "is*"); a; b ] ->
      make_operator a b (Printf.sprintf "(%s instanceof %s)")
  | RBList [ Atom (_, "as*"); instance; Atom (type_meta, type_) ] ->
      let unescp_type =
        if String.starts_with ~prefix:"\"" type_ then unpack_string type_
        else type_
      in
      make_operator
        (Atom (type_meta, unescp_type))
        instance
        (Printf.sprintf "((%s)%s)")
  | RBList [ Atom (_, "quote"); arg ] -> compile arg |> with_context
  | SBList xs ->
      xs |> List.map compile
      |> List.reduce_opt (Printf.sprintf "%s, %s")
      |> Option.value ~default:"" |> Printf.sprintf "[%s]" |> with_context
  | CBList xs ->
      compile (RBList (Atom (unknown_location, "java.util.Map/of") :: xs))
      |> with_context
  (* Namespaces *)
  | RBList (Atom (_, "do") :: (RBList (Atom (_, "ns") :: _) as ns) :: body) ->
      let name_start_pos =
        (String.rindex_opt context.filename '/' |> Option.value ~default:(-1))
        + 1
      in
      let filename =
        String.sub context.filename name_start_pos
          (String.length context.filename - name_start_pos)
      in
      let cls_name =
        String.capitalize_ascii (String.sub filename 0 1)
        ^ String.sub filename 1 (String.length filename - 5)
        |> String.map (function '.' -> '_' | x -> x)
      in
      let ns_ = compile ns in
      body
      |> List.map (fun x -> compile x)
      |> List.reduce_opt (Printf.sprintf "%s%s")
      |> Option.value ~default:""
      |> Printf.sprintf "%spublic class %s{\n%s}" ns_ cls_name
      |> with_context
  | RBList (Atom (_, "ns") :: Atom (_, name) :: ns_params) ->
      let imports =
        ns_params
        |> List.map (function
             | RBList (Atom (_, ":import") :: imports) ->
                 imports
                 |> List.map (function
                      | SBList (Atom (_, pkg) :: classes) ->
                          List.map (fun x -> compile x) classes
                          |> List.map (fun c ->
                                 Printf.sprintf "import %s.%s;\n" pkg c)
                          |> List.reduce __LOC__ (Printf.sprintf "%s%s")
                      | n -> failnode __LOC__ [ n ])
                 |> List.reduce __LOC__ (Printf.sprintf "%s%s")
             | n -> failnode __LOC__ [ n ])
        |> List.fold_left (Printf.sprintf "%s%s") ""
      in
      Printf.sprintf "package %s;\n%s" name imports |> with_context
  | RBList (Atom (_, "do") :: _body) ->
      (* failwith __LOC__ *)
      let js_body =
        _body |> List.map compile
        |> List.reduce_opt (Printf.sprintf "%s\n%s")
        |> Option.value ~default:""
      in
      with_context js_body
  | RBList (Atom (_, "fn*") :: SBList args :: body) ->
      let sargs =
        args
        |> List.map (function
             | Atom (_, aname) -> Printf.sprintf "%s" aname
             | x -> failnode __LOC__ [ x ])
        |> List.reduce_opt (Printf.sprintf "%s,%s")
        |> Option.value ~default:""
      in
      let sbody =
        let length = List.length body in
        body
        |> List.filteri (fun i _ -> i < length - 1)
        |> List.map (fun node -> compile node)
        |> List.reduce_opt (Printf.sprintf "%s;\n%s")
        |> Option.map (Printf.sprintf "%s;\n")
        |> Option.value ~default:""
      in
      let last_exp = body |> List.rev |> List.hd |> compile in
      Printf.sprintf "(%s)->{\n%sreturn %s;\n}" sargs sbody last_exp
      |> with_context
  (* Constructor *)
  | RBList (Atom (_, "new") :: Atom (_, cnst_name) :: args) ->
      let cnst_name = unpack_string cnst_name in
      let args =
        args |> List.map compile
        |> List.reduce_opt (Printf.sprintf "%s,%s")
        |> Option.value ~default:""
      in
      Printf.sprintf "new %s(%s)" cnst_name args |> with_context
  (* Functions *)
  | RBList
      [
        Atom (_, "def");
        Atom (fname_meta, fname);
        RBList (Atom (_, "fn*") :: SBList args :: body);
      ] ->
      let modifier =
        match fname_meta.symbol with ":private" -> "private" | _ -> "public"
      in
      let sargs =
        args
        |> List.map (function
             | Atom (am, aname) ->
                 Printf.sprintf "final %s %s" (get_type am) aname
             | x -> failnode __LOC__ [ x ])
        |> List.reduce_opt (Printf.sprintf "%s,%s")
        |> Option.value ~default:""
      in
      let sbody =
        let length = List.length body in
        body
        |> List.filteri (fun i _ -> i < length - 1)
        |> List.map (fun node -> compile node)
        |> List.reduce_opt (Printf.sprintf "%s;\n%s")
        |> Option.map (Printf.sprintf "%s;\n")
        |> Option.value ~default:""
      in
      let return_ =
        match fname_meta.symbol with "void" -> "" | _ -> "return "
      in
      let last_exp = body |> List.rev |> List.hd |> compile in
      Printf.sprintf "%s static %s %s (%s) {\n%s%s%s;\n}" modifier
        (get_type fname_meta) fname sargs sbody return_ last_exp
      |> with_context
  (* Static field *)
  | RBList [ Atom (_, "def"); Atom (fname_meta, fname); body ] ->
      let vis =
        if fname_meta.symbol = ":private" then "private" else "public"
      in
      let get_type am =
        if am.symbol = "" || am.symbol = ":private" then "Object" else am.symbol
      in
      let result =
        Printf.sprintf "%s static %s %s=%s;" vis (get_type fname_meta) fname
          (compile body)
      in
      result |> with_context
  (* Interop field *)
  | RBList [ Atom (_, "."); target; Atom (_, field) ]
    when String.starts_with ~prefix:":-" field ->
      Printf.sprintf "%s.%s" (compile target)
        (String.sub field 2 (String.length field - 2))
      |> with_context
  (* Interop method *)
  | RBList (Atom (_, ".") :: target :: Atom (_, mname) :: args) ->
      let sargs =
        match args with
        | [] -> ""
        | args ->
            args |> List.map compile
            |> List.reduce __LOC__ (Printf.sprintf "%s, %s")
      in
      Printf.sprintf "%s.%s(%s)" (compile target) (unpack_symbol mname) sargs
      |> with_context
  | RBList
      [
        Atom (_, "gen-class*");
        RBList
          [
            Atom (_, "quote");
            RBList
              [
                Atom (_, ":name");
                Atom (_, clsName);
                Atom (_, ":extends");
                Atom (_, superCls);
                Atom (_, ":constructors");
                CBList [ SBList params; SBList _ ];
                Atom (_, ":prefix");
                Atom (_, prefix);
                Atom (_, ":methods");
                SBList methods;
              ];
          ];
      ] ->
      generate_class compile prefix params clsName methods superCls
      |> with_context
  (* Function call *)
  | RBList (head :: args) ->
      let sargs =
        if List.length args = 0 then ""
        else
          args |> List.map compile
          |> List.reduce __LOC__ (Printf.sprintf "%s, %s")
      in
      let fname =
        match head with
        | RBList (Atom (_, "fn*") :: _) -> "(" ^ compile head ^ ")"
        | Atom (_, fname) -> String.map (function '/' -> '.' | x -> x) fname
        | _ -> compile head
      in
      fname ^ "(" ^ sargs ^ ")" |> with_context
  | n -> failnode __LOC__ [ n ]

let main (log : bool) (filename : string) prelude_macros code =
  let macros_ctx =
    prelude_macros
    |> Frontend.parse_and_simplify
         { empty_context with interpreter = Backend_interpreter.interpret }
         "prelude"
    |> fst
  in
  code |> Frontend.parse_and_simplify { macros_ctx with log } filename
  (* |> run_linter prelude_macros filename *)
  |> fun (ctx, exp) -> compile_ ctx exp |> snd |> String.trim
