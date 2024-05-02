module A = Angstrom
open Frontend

let unpack_string x = String.sub x 1 (String.length x - 2)
let unpack_symbol x = String.sub x 1 (String.length x - 1)

module Option = struct
  include Option

  let value_or f x = match x with Some y -> y | None -> f ()
end

module CompilerResult = struct
  type compile_result = { body : string; out_var : string option }

  let empty = { body = ""; out_var = None }
  let of_body body = { body; out_var = None }

  let get_val cr =
    match cr.out_var with Some x -> x | None -> failwith "No output variable"

  let get_body cr = cr.body
  let get_body_or_val cr = if cr.body <> "" then cr.body else get_val cr
  let make_val s = { body = ""; out_var = Some s }

  let merge_bodies xs =
    xs
    |> List.map (fun x -> x.body)
    |> List.filter (( <> ) "")
    |> List.reduce_opt (Printf.sprintf "%s\n%s")
    |> Option.value ~default:""
end

type result2 = Literal of string | Call of string list * string

let result_get_expression = function Literal s -> s | Call (_, s) -> s
let result_get_statments = function Literal _ -> [] | Call (xs, _) -> xs

let rec compile_ (ctx : context) (node : cljexp) : result2 =
  let make_function2 a b op =
    let ar = compile_ ctx a in
    let br = compile_ ctx b in

    let la = result_get_expression ar in
    let lb = result_get_expression br in

    let res = Printf.sprintf op la lb in
    let stmts = result_get_statments ar @ result_get_statments br in

    Call (stmts, res)
  in
  let make_operator a b op =
    let ar = compile_ ctx a in
    let br = compile_ ctx b in

    let la = result_get_expression ar in
    let lb = result_get_expression br in

    let res = Printf.sprintf "(%s%s%s)" la op lb in
    let stmts = result_get_statments ar @ result_get_statments br in

    Call (stmts, res)
  in
  (* let compile_smt node = compile_ context node |> snd in
     let compile_exp node =
       let ctx2, _ = compile_ context node in
       ctx2.out_var
     in
     let with_context node = (context, node) in
     let with_context2 out_var node = ({ context with out_var }, node) in *)
  match node with
  (* Keyword *)
  | Atom (_, x) when String.starts_with ~prefix:":" x ->
      let r = "\"" ^ String.sub x 1 (String.length x - 1) ^ "\"" in
      Literal r
  (* Quote *)
  | Atom (_, x) when String.starts_with ~prefix:"'" x ->
      let r = String.sub x 1 (String.length x - 1) in
      Literal r
  (* String *)
  | Atom (_, x) when String.starts_with ~prefix:"\"" x -> Literal x
  (* Symbol *)
  | Atom (_, x) ->
      let x = String.map (function '/' -> '.' | x -> x) x in
      Literal x
  (* ==================== *)
  | RBList [ Atom (_, op); a; b ]
    when op = "+" || op = "-" || op = "*" || op = "/" || op = ">" || op = "<"
         || op = ">=" || op = "<=" ->
      make_operator a b op
  | RBList [ Atom (_, op); a; b ] when op = "is*" ->
      make_function2 a b "(%s instanceof %s)"
  | RBList [ Atom (_, op); a; b ] when op = "as*" -> make_function2 b a "(%s)%s"
  | RBList [ Atom (_, op); a; b ] when op = "=" ->
      make_function2 a b "Objects.equals(%s,%s)"
  | RBList [ Atom (_, "not"); x ] ->
      let rx = compile_ ctx x in
      let r_call = rx |> result_get_expression |> Printf.sprintf "!%s" in
      Call (result_get_statments rx, r_call)
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
      let sbody =
        let length = List.length body in
        body
        |> List.filteri (fun i _ -> i < length - 1)
        |> List.map (fun node -> compile_ ctx node)
        |> List.concat_map (function
             | Literal x ->
                 failwith @@ "Literal '" ^ x ^ "' is not supported here"
             | Call (s, r) -> s @ [ r ^ ";" ])
      in

      let vis =
        if fname_meta.symbol = ":private" then "private" else "public"
      in
      let fn_defenition =
        Printf.sprintf "%s static %s %s(%s){" vis (get_type fname_meta) fname
          sargs
      in

      let last_r =
        List.reduce_opt (fun _ x -> x) body
        |> Option.value_or (fun _ -> failwith __LOC__)
        |> compile_ ctx
      in

      let result_statments, result_expresion =
        match last_r with
        | Literal x -> ([], Printf.sprintf "return %s;}" x)
        | Call (s, r) -> (s, Printf.sprintf "return %s;}" r)
      in

      let statments = [ fn_defenition ] @ sbody @ result_statments in

      Call (statments, result_expresion)
  (* Lambda *)
  | RBList (Atom (_, "fn*") :: SBList args :: body) ->
      let sargs =
        args
        |> List.map (function
             | Atom (_, aname) -> Printf.sprintf "%s" aname
             | x -> fail_node [ x ])
        |> List.reduce_opt (Printf.sprintf "%s,%s")
        |> Option.value ~default:""
      in
      let sbody =
        let length = List.length body in
        body
        |> List.filteri (fun i _ -> i < length - 1)
        |> List.map (fun node -> compile_ ctx node)
        |> List.concat_map (function
             | Literal x ->
                 failwith @@ "Literal '" ^ x ^ "' is not supported here"
             | Call (s, r) -> s @ [ r ^ ";" ])
      in

      let fn_defenition = Printf.sprintf "(%s)->{" sargs in

      let last_r =
        List.reduce_opt (fun _ x -> x) body
        |> Option.value_or (fun _ -> failwith __LOC__)
        |> compile_ ctx
      in

      let result_statments, result_expresion =
        match last_r with
        | Literal x -> ([], Printf.sprintf "return %s;}" x)
        | Call (s, r) -> (s, Printf.sprintf "return %s;}" r)
      in

      let statments = [ fn_defenition ] @ sbody @ result_statments in
      let statments_text = statments |> List.reduce ( ^ ) in

      Call ([], statments_text ^ result_expresion)
  | RBList [ Atom (_, "if"); c; a; b ] ->
      let c_r = compile_ ctx c in
      let c_statments = match c_r with Literal _ -> [] | Call (xs, _) -> xs in
      let c_exp = match c_r with Literal x -> x | Call (_, x) -> x in

      let a_c = compile_ ctx a in
      let a_exp = match a_c with Literal x -> x | Call (_, x) -> x in
      let a_statments = match a_c with Literal _ -> [] | Call (xs, _) -> xs in

      let b_c = compile_ ctx b in
      let b_exp = match b_c with Literal x -> x | Call (_, x) -> x in
      let b_statments = match b_c with Literal _ -> [] | Call (xs, _) -> xs in

      let temp_val = NameGenerator.get_new_var () in

      let statments =
        List.concat
          [
            c_statments;
            [ Printf.sprintf "final Object %s;if(%s){" temp_val c_exp ];
            a_statments;
            [ Printf.sprintf "%s=%s;}else{" temp_val a_exp ];
            b_statments;
            [ Printf.sprintf "%s=%s;}" temp_val b_exp ];
          ]
      in

      Call (statments, temp_val)
  (* | RBList (Atom (_, "hash-map") :: xs) ->
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
  *)
  | RBList (Atom (_, "ns") :: Atom (_, name) :: ns_params) ->
      let imports =
        ns_params
        |> List.map (function
             | RBList (Atom (_, ":import") :: imports) ->
                 imports
                 |> List.map (function
                      | SBList (Atom (_, pkg) :: classes) ->
                          List.map
                            (fun x ->
                              match compile_ ctx x with
                              | Literal x -> x
                              | Call _ -> failwith __LOC__)
                            classes
                          |> List.map (fun c ->
                                 Printf.sprintf "import %s.%s;" pkg c)
                          |> List.reduce (Printf.sprintf "%s%s")
                      | n -> fail_node [ n ])
                 |> List.reduce (Printf.sprintf "%s%s")
             | n -> fail_node [ n ])
        |> List.fold_left (Printf.sprintf "%s%s") ""
      in
      Literal (Printf.sprintf "package %s;%s" name imports)
  (*
      | RBList [ Atom (_, "is*"); x; type_ ] ->
          let type_ = compile_exp type_ in
          let out_var = Printf.sprintf "(%s instanceof %s)" (compile_exp x) type_ in
          "" |> with_context2 out_var
      | RBList [ Atom (_, "as*"); x; type_ ] ->
          let type_ = compile_exp type_ in
          let out_var = Printf.sprintf "(%s)%s" type_ (compile_exp x) in
          "" |> with_context2 out_var
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
  *)
  | RBList (Atom (_, "let*") :: SBList vals :: body) ->
      let rec val_loop = function
        | [] -> []
        | Atom (m, key) :: value :: tail ->
            let value_r = compile_ ctx value in
            let value_exp =
              match value_r with Literal x -> x | Call (_, x) -> x
            in
            let value_cast =
              if m.symbol = "" then "" else Printf.sprintf "(%s)" m.symbol
            in
            let value_statments =
              match value_r with Literal _ -> [] | Call (xs, _) -> xs
            in
            let x =
              Printf.sprintf "final var %s=%s%s;" key value_cast value_exp
            in
            List.concat [ value_statments; [ x ]; val_loop tail ]
        | xs -> failnode __LOC__ xs
      in
      let val_statments = val_loop vals in

      let body_statments =
        let length = List.length body in
        body
        |> List.filteri (fun i _ -> i < length - 1)
        |> List.map (fun node -> compile_ ctx node)
        |> List.concat_map (function
             | Literal x ->
                 failwith @@ "Literal '" ^ x ^ "' is not supported here"
             | Call (s, r) -> s @ [ r ^ ";" ])
      in
      if List.is_empty body then failnode __LOC__ [ node ];
      let last_r =
        List.reduce_opt (fun _ x -> x) body
        |> Option.value_or (fun _ -> failwith __LOC__)
        |> compile_ ctx
      in
      let result_statments, result_expresion =
        match last_r with
        (* | Literal x -> ([], x) *)
        | Literal x ->
            let out_val = NameGenerator.get_new_var () in
            ( List.concat [ []; [ Printf.sprintf "final var %s=%s;" out_val x ] ],
              out_val )
        | Call (s, r) ->
            let out_val = NameGenerator.get_new_var () in
            ( List.concat [ s; [ Printf.sprintf "final var %s=%s;" out_val r ] ],
              out_val )
      in

      Call
        ( List.concat [ val_statments; body_statments; result_statments ],
          result_expresion )
  (* let rec parse_vals nodes =
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
           let ctx1, stm1 = compile_ context x in
           let line = if stm1 = "" then ctx1.out_var ^ ";" else stm1 in
           line ^ "" ^ body_loop xs
       | [] -> fail_node []
     in
     let sbody = body_loop body in
     Printf.sprintf "%s%s" svals sbody |> with_context2 out_var
  *)
  (*
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
      *)
  | RBList (Atom (_, "module") :: (RBList (Atom (_, "ns") :: _) as ns) :: body)
    ->
      let ns_ =
        match compile_ ctx ns with
        | Literal x -> x
        | _ -> failnode __LOC__ [ node ]
      in

      let name_start_pos =
        (String.rindex_opt ctx.filename '/' |> Option.value ~default:(-1)) + 1
      in
      let filename =
        String.sub ctx.filename name_start_pos
          (String.length ctx.filename - name_start_pos)
      in
      let cls_name =
        String.capitalize_ascii (String.sub filename 0 1)
        ^ String.sub filename 1 (String.length filename - 5)
        |> String.map (function '.' -> '_' | x -> x)
      in
      let result =
        body
        |> List.map (fun x ->
               match compile_ ctx x with
               | Literal x -> x
               | Call (xs, x) ->
                   (xs |> List.reduce_opt ( ^ ) |> Option.value ~default:"") ^ x)
        |> List.reduce_opt (Printf.sprintf "%s\n%s")
        |> Option.value ~default:""
        |> Printf.sprintf "%sclass %s {%s}" ns_ cls_name
      in
      Literal result
  (*
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
  *)
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
          (match compile_ ctx body with
          | Literal x -> x
          | Call (xs, x) ->
              (List.reduce_opt ( ^ ) xs |> Option.value ~default:"") ^ x)
      in
      Literal result
  (* Interop method call *)
  | RBList (Atom (_, ".") :: target :: method_ :: args) ->
      let sfname =
        match compile_ ctx method_ with
        | Literal x -> x
        | _ -> failnode "JB:IMC:M" [ method_ ]
      in

      let target_r = compile_ ctx target in
      let target_statments =
        match target_r with Literal _ -> [] | Call (xs, _) -> xs
      in
      let target_exp =
        match target_r with Literal x -> x | Call (_, x) -> x
      in

      let results = args |> List.map (compile_ ctx) in

      let statments =
        List.concat
          [
            target_statments;
            results
            |> List.concat_map (function Literal _ -> [] | Call (xs, _) -> xs);
          ]
      in

      let args =
        results
        |> List.map (function Literal x -> x | Call (_, x) -> x)
        |> List.reduce_opt (Printf.sprintf "%s,%s")
        |> Option.value_or (fun _ -> failwith __LOC__)
      in

      Call (statments, Printf.sprintf "%s.%s(%s)" target_exp sfname args)
  (* Constructor *)
  | RBList (Atom (_, "new") :: Atom (_, cnst_name) :: args) ->
      let cnst_name = unpack_string cnst_name in
      let results = args |> List.map (compile_ ctx) in

      let statments =
        List.concat
          [
            results
            |> List.concat_map (function Literal _ -> [] | Call (xs, _) -> xs);
          ]
      in

      let args =
        results
        |> List.map (function Literal x -> x | Call (_, x) -> x)
        |> List.reduce_opt (Printf.sprintf "%s,%s")
        |> Option.value_or (fun _ -> failwith __LOC__)
      in

      Call (statments, Printf.sprintf "new %s(%s)" cnst_name args)
  (* Function call *)
  | RBList (fname :: args) ->
      let sfname =
        compile_ ctx fname |> function
        | Literal x -> x
        | _ -> failnode "BJ:FC:N" [ fname ]
      in
      let results = args |> List.map (compile_ ctx) in

      let statments =
        results
        |> List.concat_map (function Literal _ -> [] | Call (xs, _) -> xs)
      in

      let args =
        results
        |> List.map (function Literal x -> x | Call (_, x) -> x)
        |> List.reduce_opt (Printf.sprintf "%s,%s")
        |> Option.value ~default:""
      in

      Call (statments, Printf.sprintf "%s(%s)" sfname args)
  (* Default *)
  | x -> failnode "JB:CMP" [ x ]

let main (filename : string) prelude_macros code =
  let macros_ctx =
    prelude_macros
    |> Frontend.parse_and_simplify StringMap.empty 0 "prelude"
    |> fst
  in
  code |> Frontend.parse_and_simplify macros_ctx.macros 0 filename
  (* |> fun (ctx, exp) ->
     (ctx, Linter.lint prelude_macros filename exp) *)
  |>
  fun (ctx, exp) ->
  compile_ ctx exp
  |> (function
       | Literal x -> x
       | Call (xs, x) ->
           (xs |> List.reduce_opt ( ^ ) |> Option.value ~default:"") ^ x)
  |> String.trim
