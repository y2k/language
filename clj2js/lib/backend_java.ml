module A = Angstrom
open Common

let unpack_string x = String.sub x 1 (String.length x - 2)
let unpack_symbol x = String.sub x 1 (String.length x - 1)

module Option = struct
  include Option

  let value_or f x = match x with Some y -> y | None -> f ()
end

type result2 =
  | Literal of string
  | Call of string * string
  | IfCall of string * string

let result_get_expression = function
  | Literal s -> s
  | Call (_, s) -> s
  | IfCall _ -> failwith __LOC__

let result_get_statments = function
  | Literal _ -> ""
  | Call (xs, _) -> xs
  | IfCall (xs, _) -> xs

let generate_class (compile_exp : cljexp -> result2) prefix params clsName
    methods superCls =
  let prefix = String.sub prefix 1 (String.length prefix - 2) in
  let cnt_params =
    match params with
    | [] -> ""
    | params ->
        params
        |> List.mapi (fun i x ->
               let type1 =
                 match compile_exp x with
                 | Literal x -> x
                 | Call _ -> failwith __LOC__
                 | IfCall _ -> failwith __LOC__
               in
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
        |> Printf.sprintf "public %s(%s){state=java.util.List.of(%s);}" clsName
             cnt_params
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
                      | x -> failnode __LOC__ [ x ])
               |> List.reduce (Printf.sprintf "%s, %s")
             in
             let args__ =
               args
               |> List.mapi (fun i _ -> Printf.sprintf "p%i" i)
               |> List.reduce (Printf.sprintf "%s,%s")
             in
             let annot = match m.symbol with "" -> "" | x -> "@" ^ x ^ " " in
             let return_ =
               if rtype = "void" then "" else Printf.sprintf "return (%s)" rtype
             in
             let call_super =
               if annot = "@Override " then
                 Printf.sprintf "super.%s(%s);" mname args__
               else ""
             in
             Printf.sprintf "%spublic %s %s(%s){%s%s%s%s(this,%s);}" annot rtype
               mname args_ call_super return_ prefix mname args__
         | x -> failnode __LOC__ [ x ])
    |> List.reduce (Printf.sprintf "%s%s")
  in
  Printf.sprintf
    "public static class %s extends %s{public java.util.List<Object> \
     state;%s%s}"
    clsName superCls state ms

let rec compile_ (ctx : context) (node : cljexp) : result2 =
  let make_operator a b f =
    let ar = compile_ ctx a in
    let br = compile_ ctx b in
    let la = result_get_expression ar in
    let lb = result_get_expression br in
    let stmts = result_get_statments ar ^ result_get_statments br in
    Call (stmts, f la lb)
  in
  match node with
  | Atom (_, "unit") -> Literal "(Object)null"
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
  | RBList [ Atom (_, "quote"); arg ] -> compile_ ctx arg
  | RBList [ Atom (_, op); a; b ]
    when op = "+" || op = "-" || op = "*" || op = "/" || op = ">" || op = "<"
         || op = ">=" || op = "<=" ->
      make_operator a b (fun a b -> Printf.sprintf "(%s%s%s)" a op b)
  | RBList [ Atom (_, op); a; b ] when op = "is*" ->
      make_operator a b (Printf.sprintf "(%s instanceof %s)")
  | RBList [ Atom (_, op); a; b ] when op = "as*" ->
      make_operator b a (Printf.sprintf "(%s)%s")
  | RBList [ Atom (_, "not"); x ] ->
      let rx = compile_ ctx x in
      let r_call = rx |> result_get_expression |> Printf.sprintf "!%s" in
      Call (result_get_statments rx, r_call)
  (* Function definition *)
  | CBList xs ->
      compile_ ctx (RBList (Atom (unknown_location, "java.util.Map/of") :: xs))
  | SBList xs ->
      compile_ ctx (RBList (Atom (unknown_location, "java.util.List/of") :: xs))
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
             | x -> failnode __LOC__ [ x ])
        |> List.reduce_opt (Printf.sprintf "%s,%s")
        |> Option.value ~default:""
      in
      let sbody =
        let length = List.length body in
        body
        |> List.filteri (fun i _ -> i < length - 1)
        |> List.map (fun node -> compile_ ctx node)
        |> List.map (function
             | Literal x ->
                 failwith @@ "Literal '" ^ x ^ "' is not supported here"
             | Call (s, r) -> s ^ r ^ ";"
             | IfCall (s, _) -> s ^ ";")
        |> List.fold_left ( ^ ) ""
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
        | Literal x -> ("", Printf.sprintf "return %s;}" x)
        | Call (s, r) -> (s, Printf.sprintf "return %s;}" r)
        | IfCall (s, r) -> (s, Printf.sprintf "return %s;}" r)
      in

      let statments = fn_defenition ^ sbody ^ result_statments in

      Call (statments, result_expresion)
  (* Lambda *)
  | RBList (Atom (m, "fn*") :: SBList args :: body) ->
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
        |> List.map (fun node -> compile_ ctx node)
        |> List.map (function
             | Literal x ->
                 failwith @@ "Literal '" ^ x ^ "' is not supported here"
             | Call (s, r) -> s ^ r ^ ";"
             | IfCall _ -> failwith __LOC__)
        |> List.fold_left ( ^ ) ""
      in

      let fn_defenition = Printf.sprintf "(%s)->{" sargs in

      let last_r =
        List.reduce_opt (fun _ x -> x) body
        |> Option.value_or (fun _ -> failwith __LOC__)
        |> compile_ ctx
      in

      let result_statments, result_expresion =
        let return_ = match m.symbol with "void" -> "" | _ -> "return " in
        match last_r with
        | Literal x -> ("", Printf.sprintf "%s%s;}" return_ x)
        | Call (s, r) -> (s, Printf.sprintf "%s%s;}" return_ r)
        | IfCall _ -> failwith __LOC__
      in

      let statments = fn_defenition ^ sbody ^ result_statments in
      let statments_text = statments in

      Call ("", statments_text ^ result_expresion)
  | RBList [ Atom (_, "if"); c; a; b ] ->
      let c_r = compile_ ctx c in
      let c_exp =
        match c_r with Literal x -> x | Call (_, x) -> x | IfCall (_, x) -> x
      in
      let c_statments = result_get_statments c_r in

      let a_c = compile_ ctx a in
      let a_exp =
        match a_c with Literal x -> x | Call (_, x) -> x | IfCall (_, x) -> x
      in
      let a_statments = result_get_statments a_c in

      let b_c = compile_ ctx b in
      let b_exp =
        match b_c with Literal x -> x | Call (_, x) -> x | IfCall (_, x) -> x
      in
      let b_statments = result_get_statments b_c in

      let temp_val = NameGenerator.get_new_var () in
      let statments =
        c_statments
        ^ Printf.sprintf "final Object %s;if(%s){" temp_val c_exp
        ^ a_statments
        ^ Printf.sprintf "%s=%s;}else{" temp_val a_exp
        ^ b_statments
        ^ Printf.sprintf "%s=%s;}" temp_val b_exp
      in

      IfCall (statments, temp_val)
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
                              | Call _ -> failwith __LOC__
                              | IfCall _ -> failwith __LOC__)
                            classes
                          |> List.map (fun c ->
                                 Printf.sprintf "import %s.%s;" pkg c)
                          |> List.reduce (Printf.sprintf "%s%s")
                      | n -> failnode __LOC__ [ n ])
                 |> List.reduce (Printf.sprintf "%s%s")
             | n -> failnode __LOC__ [ n ])
        |> List.fold_left (Printf.sprintf "%s%s") ""
      in
      Literal (Printf.sprintf "package %s;%s" name imports)
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
      Literal
        (generate_class (compile_ ctx) prefix params clsName methods superCls)
  | RBList (Atom (_, "let*") :: SBList vals :: body) ->
      let rec val_loop = function
        | [] -> ""
        | Atom (m, key) :: value :: tail ->
            let value_r = compile_ ctx value in
            let value_exp = result_get_expression value_r in
            let value_cast =
              if m.symbol = "" then "" else Printf.sprintf "(%s)" m.symbol
            in
            let value_statments = result_get_statments value_r in
            let x =
              Printf.sprintf "final var %s=%s%s;" key value_cast value_exp
            in
            value_statments ^ x ^ val_loop tail
        | xs -> failnode __LOC__ xs
      in
      let val_statments = val_loop vals in

      let body_statments =
        let length = List.length body in
        body
        |> List.filteri (fun i _ -> i < length - 1)
        |> List.map (fun node -> compile_ ctx node)
        |> List.map (function
             | Literal x ->
                 failwith @@ "Literal '" ^ x ^ "' is not supported here"
             | Call (s, r) -> s ^ r ^ ";"
             | IfCall (s, _) -> s)
        |> List.fold_left ( ^ ) ""
      in
      if List.is_empty body then failnode __LOC__ [ node ];
      let last_r =
        List.reduce_opt (fun _ x -> x) body
        |> Option.value_or (fun _ -> failwith __LOC__)
        |> compile_ ctx
      in
      let result_statments, result_expresion =
        match last_r with
        | Literal x ->
            let out_val = NameGenerator.get_new_var () in
            (Printf.sprintf "final var %s=%s;" out_val x, out_val)
        | Call (s, r) ->
            let out_val = NameGenerator.get_new_var () in
            (s ^ Printf.sprintf "final var %s=%s;" out_val r, out_val)
        | IfCall (s, r) ->
            let out_val = NameGenerator.get_new_var () in
            (s ^ Printf.sprintf "final var %s=%s;" out_val r, out_val)
      in

      Call (val_statments ^ body_statments ^ result_statments, result_expresion)
  | RBList (Atom (_, "comment") :: _) -> Literal ""
  | RBList (Atom (_, "module") :: body) ->
      let ns_, body =
        match body with
        | (RBList (Atom (_, "ns") :: _) as ns) :: body ->
            ( (match compile_ ctx ns with
              | Literal x -> x
              | _ -> failnode __LOC__ [ node ]),
              body )
        | body -> ("", body)
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
               | Call (xs, x) -> xs ^ x
               | IfCall _ -> failwith __LOC__)
        |> List.reduce_opt (Printf.sprintf "%s%s")
        |> Option.value ~default:""
        |> Printf.sprintf "%sclass %s{%s}" ns_ cls_name
      in
      Literal result
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
          | Call (xs, x) -> xs ^ x
          | IfCall _ -> failwith __LOC__)
      in
      Literal result
  (* Interop method call *)
  | RBList (Atom (_, ".") :: target :: Atom (_, method_) :: args) ->
      let sfname = unpack_symbol method_ in
      let target_r = compile_ ctx target in
      let target_statments =
        match target_r with
        | Literal _ -> ""
        | Call (xs, _) -> xs
        | IfCall _ -> failwith __LOC__
      in
      let target_exp =
        match target_r with
        | Literal x -> x
        | Call (_, x) -> x
        | IfCall _ -> failwith __LOC__
      in

      let results = args |> List.map (compile_ ctx) in

      let statments =
        List.concat
          [
            [ target_statments ];
            results
            |> List.map (function
                 | Literal _ -> ""
                 | Call (xs, _) -> xs
                 | IfCall _ -> failwith __LOC__);
          ]
        |> List.fold_left ( ^ ) ""
      in

      let args =
        results
        |> List.map (function
             | Literal x -> x
             | Call (_, x) -> x
             | IfCall _ -> failwith __LOC__)
        |> List.reduce_opt (Printf.sprintf "%s,%s")
        |> Option.value ~default:""
      in

      Call (statments, Printf.sprintf "%s.%s(%s)" target_exp sfname args)
  (* Constructor *)
  | RBList (Atom (_, "new") :: Atom (_, cnst_name) :: args) ->
      let cnst_name = unpack_string cnst_name in
      let results = args |> List.map (compile_ ctx) in

      let statments =
        results
        |> List.map (function
             | Literal _ -> ""
             | Call (xs, _) -> xs
             | IfCall _ -> failwith __LOC__)
        |> List.fold_left ( ^ ) ""
      in

      let args =
        results
        |> List.map (function
             | Literal x -> x
             | Call (_, x) -> x
             | IfCall _ -> failwith __LOC__)
        |> List.reduce_opt (Printf.sprintf "%s,%s")
        |> Option.value ~default:""
      in

      Call (statments, Printf.sprintf "new %s(%s)" cnst_name args)
  (* Function call *)
  | RBList (fname :: args) ->
      let sfname =
        compile_ ctx fname |> function
        | Literal x -> x
        | _ -> failnode __LOC__ [ fname ]
      in
      let results = args |> List.map (compile_ ctx) in

      let statments =
        results
        |> List.map (function
             | Literal _ -> ""
             | Call (xs, _) -> xs
             | IfCall (xs, _) -> xs)
        |> List.fold_left ( ^ ) ""
      in

      let args =
        results
        |> List.map (function
             | Literal x -> x
             | Call (_, x) -> x
             | IfCall (_, x) -> x)
        |> List.reduce_opt (Printf.sprintf "%s,%s")
        |> Option.value ~default:""
      in

      Call (statments, Printf.sprintf "%s(%s)" sfname args)
  (* Default *)
  | x -> failnode __LOC__ [ x ]

let run_linter prelude_macros filename (ctx, exp) =
  (ctx, Linter.lint Backend_interpreter.interpret prelude_macros filename exp)

let main (filename : string) prelude_macros code =
  let macros_ctx =
    prelude_macros
    |> Frontend.parse_and_simplify
         { empty_context with interpreter = Backend_interpreter.interpret }
         "prelude"
    |> fst
  in
  code
  |> Frontend.parse_and_simplify macros_ctx filename
  |> run_linter prelude_macros filename
  |> (fun (ctx, exp) -> compile_ ctx exp)
  |> (function
       | Literal x -> x | Call (xs, x) -> xs ^ x | IfCall (xs, x) -> xs ^ x)
  |> String.trim
