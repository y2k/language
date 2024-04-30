module A = Angstrom
open Frontend

let unpack_string x = String.sub x 1 (String.length x - 2)
let unpack_symbol x = String.sub x 1 (String.length x - 1)

let rec compile_ (context : context) (node : cljexp) : context * string =
  let compile_smt node = compile_ context node |> snd in
  let compile_exp node =
    let ctx2, _ = compile_ context node in
    ctx2.out_var
  in
  let with_context node = (context, node) in
  let with_context2 out_var node = ({ context with out_var }, node) in
  match node with
  | Atom (_, x) when String.starts_with ~prefix:":" x ->
      let r = "\"" ^ String.sub x 1 (String.length x - 1) ^ "\"" in
      ({ context with out_var = r }, "")
  | Atom (_, x) when String.starts_with ~prefix:"'" x ->
      let r = String.sub x 1 (String.length x - 1) in
      ({ context with out_var = r }, "")
  | Atom (_, x) when String.starts_with ~prefix:"\"" x ->
      ({ context with out_var = x }, "")
  | Atom (_, x) ->
      let x = String.map (function '/' -> '.' | x -> x) x in
      ({ context with out_var = x }, "")
  | RBList [ Atom (_, "+"); a; b ] ->
      ""
      |> with_context2
           (Printf.sprintf "(%s+%s)" (compile_exp a) (compile_exp b))
  | RBList [ Atom (_, "-"); a; b ] ->
      ""
      |> with_context2
           (Printf.sprintf "(%s-%s)" (compile_exp a) (compile_exp b))
  | RBList [ Atom (_, "*"); a; b ] ->
      ""
      |> with_context2
           (Printf.sprintf "(%s*%s)" (compile_exp a) (compile_exp b))
  | RBList [ Atom (_, "/"); a; b ] ->
      ""
      |> with_context2
           (Printf.sprintf "(%s/%s)" (compile_exp a) (compile_exp b))
  | RBList [ Atom (_, ">"); a; b ] ->
      ""
      |> with_context2
           (Printf.sprintf "(%s>%s)" (compile_exp a) (compile_exp b))
  | RBList [ Atom (_, "<"); a; b ] ->
      ""
      |> with_context2
           (Printf.sprintf "(%s<%s)" (compile_exp a) (compile_exp b))
  | RBList [ Atom (_, ">="); a; b ] ->
      ""
      |> with_context2
           (Printf.sprintf "(%s>=%s)" (compile_exp a) (compile_exp b))
  | RBList [ Atom (_, "<="); a; b ] ->
      ""
      |> with_context2
           (Printf.sprintf "(%s<=%s)" (compile_exp a) (compile_exp b))
  | RBList [ Atom (_, "not"); x ] ->
      "" |> with_context2 (Printf.sprintf "!%s" (compile_exp x))
  | RBList [ Atom (_, "="); a; b ] ->
      ""
      |> with_context2
           (Printf.sprintf "Objects.equals(%s,%s)" (compile_exp a)
              (compile_exp b))
  | RBList (Atom (_, "vector") :: xs) ->
      let args_init =
        xs
        |> List.map (fun x -> compile_ context x |> snd)
        |> List.filter (fun x -> x <> "")
        |> List.reduce_opt ( ^ ) |> Option.value ~default:""
      in
      let args =
        xs |> List.map compile_exp
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
        xs |> List.map compile_exp
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
                          List.map compile_exp classes
                          |> List.map (fun c ->
                                 Printf.sprintf "import %s.%s;" pkg c)
                          |> List.reduce (Printf.sprintf "%s%s")
                      | n -> fail_node [ n ])
                 |> List.reduce (Printf.sprintf "%s%s")
             | n -> fail_node [ n ])
        |> List.fold_left (Printf.sprintf "%s%s") ""
      in
      Printf.sprintf "package %s;%s%s" name imports "" |> with_context
  | RBList [ Atom (_, "is"); x; Atom (_, type_) ]
    when String.starts_with ~prefix:"'" type_ ->
      let type_ = String.sub type_ 1 (String.length type_ - 1) in
      let out_var = Printf.sprintf "(%s instanceof %s)" (compile_exp x) type_ in
      "" |> with_context2 out_var
  | RBList [ Atom (_, "as"); x; Atom (_, type_) ]
    when String.starts_with ~prefix:"'" type_ ->
      let type_ = String.sub type_ 1 (String.length type_ - 1) in
      let out_var = Printf.sprintf "(%s)%s" type_ (compile_exp x) in
      "" |> with_context2 out_var
  (* | RBList [ Atom (_, "quote"); x ] -> compile_ context x *)
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
      let rec body_loop = function
        | [ x ] ->
            let ctx2, x = compile_ context x in
            Printf.sprintf "%sfinal var %s=%s;" x out_var ctx2.out_var
        | [ x; xs ] ->
            let ctx1, stm1 = compile_ context x in
            let line = if stm1 = "" then ctx1.out_var ^ ";" else stm1 in
            line ^ "" ^ body_loop [ xs ] ^ ""
        | x :: xs ->
            (* let ctx1, stm1 = compile_ context x in
               let line = if stm1 = "" then ctx1.out_var else stm1 in *)
            let ctx1, stm1 = compile_ context x in
            (* print_endline @@ "LOG: " ^ ctx1.out_var ^ " | " ^ stm1; *)
            let line = if stm1 = "" then ctx1.out_var ^ ";" else stm1 in
            (* let stm_of_exp =
                 match ctx1.out_var with "null" -> "" | x -> x ^ ";"
               in *)
            (* let a = Printf.sprintf "%s%s" stm1 stm_of_exp in *)
            line ^ "" ^ body_loop xs
        | [] -> fail_node []
      in
      let sbody = body_loop body in
      Printf.sprintf "%s%s" svals sbody |> with_context2 out_var
  | RBList [ Atom (_, "class"); Atom (_, name) ]
    when String.starts_with ~prefix:"'" name ->
      let name = String.sub name 1 (String.length name - 1) in
      "" |> with_context2 (Printf.sprintf "%s.class" name)
  | RBList (Atom (_, "comment") :: _) -> "" |> with_context
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
      let ns_ = compile_smt ns in
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
      body |> List.map compile_smt
      |> List.reduce_opt (Printf.sprintf "%s\n%s")
      |> Option.value ~default:""
      |> Printf.sprintf "%sclass %s {%s}" ns_ cls_name
      |> with_context
  | RBList (Atom (_, "module") :: body) ->
      body |> List.map compile_smt
      |> List.reduce (Printf.sprintf "%s\n%s")
      |> with_context
  | RBList (Atom (_, "do") :: body) ->
      let rec loop ctx = function
        | [ x ] -> ({ ctx with out_var = compile_exp x }, compile_smt x)
        | x :: xs ->
            let stm = compile_smt x in
            let exp = compile_exp x in
            let ctx2, other = loop ctx xs in
            (ctx2, Printf.sprintf "%s%s;%s" stm exp other)
        | [] -> fail_node []
      in
      loop context body
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
            let line = if stm1 = "" then ctx1.out_var else stm1 in
            let stm2, ctx2 = loop_body xs in
            (Printf.sprintf "%s;%s" line stm2, ctx2)
      in
      let sbody, ctx2 = loop_body body in
      let vis =
        if fname_meta.symbol = ":private" then "private" else "public"
      in
      Printf.sprintf "%s static %s %s(%s){%sreturn %s;}" vis
        (get_type fname_meta) fname sargs sbody ctx2.out_var
      |> with_context
  (* Static field *)
  | RBList [ Atom (_, "def"); Atom (fname_meta, fname); body ] ->
      let vis =
        if fname_meta.symbol = ":private" then "private" else "public"
      in
      let get_type am =
        if am.symbol = "" || am.symbol = ":private" then "Object" else am.symbol
      in
      Printf.sprintf "%s static %s %s=%s;" vis (get_type fname_meta) fname
        (compile_exp body)
      |> with_context
  (* Interop method call *)
  | RBList (Atom (_, ".") :: target :: Atom (_, mname) :: args) ->
      let mname = unpack_symbol mname in
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
          (args_init
          ^ Printf.sprintf "%s.%s(%s);" (compile_exp target) mname sargs)
      else
        with_context2
          (Printf.sprintf "%s.%s(%s)" (compile_exp target) mname sargs)
          args_init
  (* Constructor *)
  | RBList (Atom (_, "new") :: Atom (_, cnst_name) :: args) ->
      let a =
        if List.length args = 0 then ""
        else
          args |> List.map compile_exp |> List.reduce (Printf.sprintf "%s,%s")
      in
      let out_var = Printf.sprintf "new %s(%s)" (unpack_string cnst_name) a in
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

let main (filename : string) prelude_macros code =
  let macros_ctx =
    prelude_macros
    |> Frontend.parse_and_simplify StringMap.empty 0 "prelude"
    |> fst
  in
  code |> Frontend.parse_and_simplify macros_ctx.macros 0 filename
  |> fun (ctx, exp) ->
  (ctx, Linter.lint prelude_macros filename exp) |> fun (ctx, exp) ->
  let ctx, result = compile_ ctx exp in
  result ^ ctx.out_var |> String.trim
