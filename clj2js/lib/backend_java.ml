module A = Angstrom
open Frontend

let rec compile_ (context : context) (node : cljexp) : context * string =
  let compile node = compile_ context node |> snd in
  let compile2 node =
    let ctx2, _ = compile_ context node in
    ctx2.out_var
  in
  let with_context node = (context, node) in
  let with_context2 out_var node = ({ context with out_var }, node) in
  match node with
  | Atom (_, x) when String.starts_with ~prefix:":" x ->
      let r = "\"" ^ String.sub x 1 (String.length x - 1) ^ "\"" in
      ({ context with out_var = r }, "")
  | Atom (_, x) when String.starts_with ~prefix:"\"" x ->
      ({ context with out_var = x }, "")
  | Atom (_, x) ->
      let x = String.map (function '/' -> '.' | x -> x) x in
      ({ context with out_var = x }, "")
  | RBList [ Atom (_, "+"); a; b ] ->
      "" |> with_context2 (Printf.sprintf "(%s+%s)" (compile2 a) (compile2 b))
  | RBList [ Atom (_, "-"); a; b ] ->
      "" |> with_context2 (Printf.sprintf "(%s-%s)" (compile2 a) (compile2 b))
  | RBList [ Atom (_, "*"); a; b ] ->
      "" |> with_context2 (Printf.sprintf "(%s*%s)" (compile2 a) (compile2 b))
  | RBList [ Atom (_, "/"); a; b ] ->
      "" |> with_context2 (Printf.sprintf "(%s/%s)" (compile2 a) (compile2 b))
  | RBList [ Atom (_, ">"); a; b ] ->
      "" |> with_context2 (Printf.sprintf "(%s>%s)" (compile2 a) (compile2 b))
  | RBList [ Atom (_, "<"); a; b ] ->
      "" |> with_context2 (Printf.sprintf "(%s<%s)" (compile2 a) (compile2 b))
  | RBList [ Atom (_, ">="); a; b ] ->
      "" |> with_context2 (Printf.sprintf "(%s>=%s)" (compile2 a) (compile2 b))
  | RBList [ Atom (_, "<="); a; b ] ->
      "" |> with_context2 (Printf.sprintf "(%s<=%s)" (compile2 a) (compile2 b))
  | RBList [ Atom (_, "not"); x ] ->
      "" |> with_context2 (Printf.sprintf "!%s" (compile2 x))
  | RBList [ Atom (_, "="); a; b ] ->
      ""
      |> with_context2
           (Printf.sprintf "Objects.equals(%s,%s)" (compile2 a) (compile2 b))
  | RBList (Atom (_, "vector") :: xs) ->
      let args_init =
        xs
        |> List.map (fun x -> compile_ context x |> snd)
        |> List.filter (fun x -> x <> "")
        |> List.reduce_opt ( ^ ) |> Option.value ~default:""
      in
      let args =
        xs |> List.map compile2
        |> List.reduce_opt (Printf.sprintf "%s,%s")
        |> Option.value ~default:""
      in
      let out_var = Printf.sprintf "List.of(%s)" args in
      args_init |> with_context2 out_var
  | RBList (Atom (_, "hash-map") :: xs) ->
      let args_init =
        xs
        |> List.map (fun x -> compile_ context x |> snd)
        |> List.filter (fun x -> x <> "")
        |> List.reduce_opt ( ^ ) |> Option.value ~default:""
      in
      let args =
        xs |> List.map compile2
        |> List.reduce_opt (Printf.sprintf "%s,%s")
        |> Option.value ~default:""
      in
      let out_var = Printf.sprintf "java.util.Map.of(%s)" args in
      args_init |> with_context2 out_var
  | RBList [ Atom (_, "if"); c; a; b ] ->
      let c_ctx, c_stmt = compile_ context c in
      let a_ctx, a_stmt = compile_ context a in
      let b_ctx, b_stmt = compile_ context b in
      let out_var = NameGenerator.get_new_var () in
      Printf.sprintf "%sfinal Object %s;if(%s){%s%s=%s;}else{%s%s=%s;}" c_stmt
        out_var c_ctx.out_var a_stmt out_var a_ctx.out_var b_stmt out_var
        b_ctx.out_var
      |> with_context2 out_var
  | RBList (Atom (_, "ns") :: Atom (_, name) :: ns_params) ->
      let imports =
        ns_params
        |> List.map (function
             | RBList (Atom (_, ":import") :: imports) ->
                 imports
                 |> List.map (function
                      | SBList (Atom (_, pkg) :: classes) ->
                          List.map compile2 classes
                          |> List.map (fun c ->
                                 Printf.sprintf "import %s.%s;" pkg c)
                          |> List.reduce (Printf.sprintf "%s%s")
                      | n -> fail_node [ n ])
                 |> List.reduce (Printf.sprintf "%s%s")
             | n -> fail_node [ n ])
        |> List.fold_left (Printf.sprintf "%s%s") ""
      in
      Printf.sprintf "package %s;%s%s" name imports "" |> with_context
  | RBList [ Atom (_, "is"); x; Atom (_, type_) ] ->
      let out_var = Printf.sprintf "(%s instanceof %s)" (compile2 x) type_ in
      "" |> with_context2 out_var
  | RBList [ Atom (_, "as"); x; Atom (_, type_) ] ->
      let out_var = Printf.sprintf "(%s)%s" type_ (compile2 x) in
      "" |> with_context2 out_var
  | RBList
      [
        Atom (_, "gen-class");
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
      ] ->
      let prefix = String.sub prefix 1 (String.length prefix - 2) in
      let cnt_params =
        match params with
        | [] -> ""
        | params ->
            params
            |> List.mapi (fun i x ->
                   let type1 = compile2 x in
                   let type2 =
                     if String.starts_with ~prefix:"\"" type1 then
                       String.sub type1 1 (String.length type1 - 2)
                     else type1
                   in
                   Printf.sprintf "%s p%i" type2 i)
            |> List.reduce (Printf.sprintf "%s,%s")
      in
      let state =
        match params with
        | [] -> ""
        | params ->
            params
            |> List.mapi (fun i _ -> Printf.sprintf "p%i" i)
            |> List.reduce (Printf.sprintf "%s,%s")
            |> Printf.sprintf "public %s(%s){state=java.util.List.of(%s);}"
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
                          | Atom (_, a) -> Printf.sprintf "%s p%i" a i
                          | x -> fail_node [ x ])
                   |> List.reduce (Printf.sprintf "%s, %s")
                 in
                 let args__ =
                   args
                   |> List.mapi (fun i _ -> Printf.sprintf "p%i" i)
                   |> List.reduce (Printf.sprintf "%s,%s")
                 in
                 let annot =
                   match m.symbol with "" -> "" | x -> "@" ^ x ^ " "
                 in
                 let return_ =
                   if rtype = "void" then ""
                   else Printf.sprintf "return (%s)" rtype
                 in
                 let call_super =
                   if annot = "@Override " then
                     Printf.sprintf "super.%s(%s);" mname args__
                   else ""
                 in
                 Printf.sprintf "%spublic %s %s(%s){%s%s%s%s(this,%s);}" annot
                   rtype mname args_ call_super return_ prefix mname args__
             | x -> fail_node [ x ])
        |> List.reduce (Printf.sprintf "%s%s")
      in
      Printf.sprintf
        "public static class %s extends %s{public java.util.List<Object> \
         state;%s%s}"
        clsName superCls state ms
      |> with_context
  | RBList (Atom (_, "let*") :: SBList vals :: body) ->
      let rec parse_vals nodes =
        match nodes with
        | Atom (m, val_name) :: val_body :: remain ->
            let ctx2, s = compile_ context val_body in
            let type_ =
              if m.symbol = "" then "" else Printf.sprintf "(%s)" m.symbol
            in
            Printf.sprintf "%sfinal var %s=%s%s;%s" s val_name type_
              ctx2.out_var (parse_vals remain)
        | [] -> ""
        | xs -> fail_node xs
      in
      let svals = parse_vals vals in
      let out_var = NameGenerator.get_new_var () in
      let sbody =
        body
        |> List.map (compile_ context)
        |> List.rev
        |> List.mapi (fun i (ctx2, x) ->
               if i = 0 then
                 Printf.sprintf "%sfinal var %s=%s;" x out_var ctx2.out_var
               else Printf.sprintf "%s;%s" ctx2.out_var x)
        |> List.rev |> String.concat ""
      in
      Printf.sprintf "%s%s" svals sbody |> with_context2 out_var
  | RBList [ Atom (_, "class"); Atom (_, name) ] ->
      "" |> with_context2 (Printf.sprintf "%s.class" name)
  (* Lambda *)
  | RBList (Atom (m, "fn*") :: SBList args :: body) ->
      let get_type am = if am.symbol = "" then "" else am.symbol ^ " " in
      let sargs =
        args
        |> List.map (function
             | Atom (am, aname) -> Printf.sprintf "%s%s" (get_type am) aname
             | x -> fail_node [ x ])
        |> List.reduce_opt (Printf.sprintf "%s,%s")
        |> Option.value ~default:""
      in
      let rec loop_body = function
        | [] -> fail_node []
        | [ x ] ->
            let ctx2, s = compile_ context x in
            let s = if s = "" then "" else s in
            (Printf.sprintf "%s" s, ctx2)
        | x :: xs ->
            let _, s = compile_ context x in
            let s2, ctx2 = loop_body xs in
            (Printf.sprintf "%s%s" s s2, ctx2)
      in
      let sbody, ctx2 = loop_body body in
      let returns = if m.symbol = "void" then "" else "return " in
      let out_var =
        Printf.sprintf "(%s)->{%s%s%s;}" sargs sbody returns ctx2.out_var
      in
      "" |> with_context2 out_var
  | RBList (Atom (_, "module") :: (RBList (Atom (_, "ns") :: _) as ns) :: body)
    ->
      let ns_ = compile ns in
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
      body |> List.map compile
      |> List.reduce (Printf.sprintf "%s\n%s")
      |> Printf.sprintf "%sclass %s {%s}" ns_ cls_name
      |> with_context
  | RBList (Atom (_, "module") :: body) ->
      body |> List.map compile
      |> List.reduce (Printf.sprintf "%s\n%s")
      |> Printf.sprintf "class Application {%s}"
      |> with_context
  (* Function definition *)
  | RBList
      [
        Atom (_, "def");
        Atom (fname_meta, fname);
        RBList (Atom (_, "fn*") :: SBList args :: body);
      ] ->
      let get_type am =
        if am.symbol = "" || am.symbol = ":private" then "Object" else am.symbol
      in
      let sargs =
        args
        |> List.map (function
             | Atom (am, aname) ->
                 Printf.sprintf "final %s %s" (get_type am) aname
             | x -> fail_node [ x ])
        |> List.reduce_opt (Printf.sprintf "%s,%s")
        |> Option.value ~default:""
      in
      let rec loop_body = function
        | [] -> fail_node []
        | [ x ] ->
            let ctx2, stm = compile_ context x in
            (Printf.sprintf "%s" stm, ctx2)
        | x :: xs ->
            let ctx1, stm1 = compile_ context x in
            let stm2, ctx2 = loop_body xs in
            (Printf.sprintf "%s%s;%s" stm1 ctx1.out_var stm2, ctx2)
      in
      let sbody, ctx2 = loop_body body in
      let vis =
        if fname_meta.symbol = ":private" then "private" else "public"
      in
      Printf.sprintf "%s static %s %s(%s){%sreturn %s;}" vis
        (get_type fname_meta) fname sargs sbody ctx2.out_var
      |> with_context
  (* Interop method call *)
  | RBList (Atom (_, ".") :: target :: Atom (_, mname) :: args) ->
      let args2 = args |> List.map (compile_ context) in
      let sargs =
        match args2 with
        | [] -> ""
        | args ->
            args
            |> List.map (fun (ctx2, _) -> ctx2.out_var)
            |> List.reduce (Printf.sprintf "%s,%s")
            |> Printf.sprintf "%s"
      in
      let args_init =
        match args2 with
        | [] -> ""
        | args2 ->
            args2 |> List.map snd
            |> List.filter (fun x -> x <> "")
            |> List.reduce_opt (Printf.sprintf "%s;%s")
            |> Option.value ~default:""
      in
      let mname, is_void =
        if String.ends_with ~suffix:"!" mname then
          (String.sub mname 0 (String.length mname - 1), true)
        else (mname, false)
      in
      if is_void then
        with_context2 "(Void)null"
          (args_init ^ Printf.sprintf "%s.%s(%s);" (compile2 target) mname sargs)
      else
        with_context2
          (Printf.sprintf "%s.%s(%s)" (compile2 target) mname sargs)
          args_init
  (* Constructor *)
  | RBList (Atom (_, "new") :: Atom (_, cnst_name) :: args) ->
      let a =
        if List.length args = 0 then ""
        else args |> List.map compile2 |> List.reduce (Printf.sprintf "%s,%s")
      in
      let out_var = Printf.sprintf "new %s(%s)" cnst_name a in
      "" |> with_context2 out_var
  (* Function call *)
  | RBList (fname :: args) ->
      let stms, sargs =
        if List.length args = 0 then ("", "")
        else
          let xs = args |> List.map (fun x -> compile_ context x) in
          ( xs
            |> List.map (fun (_, stm) -> stm)
            |> List.reduce (Printf.sprintf "%s%s"),
            xs
            |> List.map (fun (ctx2, _) -> ctx2.out_var)
            |> List.reduce (Printf.sprintf "%s,%s") )
      in
      let clear_fname =
        let ctx2, _ = compile_ context fname in
        String.map (function '/' -> '.' | x -> x) ctx2.out_var
      in
      let out_var = Printf.sprintf "%s%s(%s)" stms clear_fname sargs in
      "" |> with_context2 out_var
  (* Default *)
  | x -> fail_node [ x ]

let main (filename : string) code =
  let prelude_macros =
    {|(defmacro not= [a b] (list 'not (list '= a b)))
      (defmacro gen-class [& body] (list '__inject_raw_sexp (concat (list 'gen-class) body)))
      (defmacro fn! [& body] (concat (list ^void 'fn) body))
      (defmacro str [& xs] (concat (list 'y2k.RT/str) xs))
      (defmacro checked! [f] (list 'y2k.RT/try_ (list 'fn (vector) f)))
      (defmacro get [target key] (list 'y2k.RT/get target key))
      (defmacro println [& xs] (list 'System.out/println (concat (list 'str) xs)))
      |}
  in
  let prefix_lines_count =
    String.fold_left
      (fun acc c -> if c = '\n' then acc + 1 else acc)
      1 prelude_macros
  in
  String.concat "\n" [ prelude_macros; code ]
  |> Frontend.parse_and_simplify prefix_lines_count filename
  |> fun (ctx, exp) ->
  (* print_endline (show_cljexp exp); *)
  let ctx, result = compile_ ctx exp in
  result ^ ctx.out_var |> String.trim
