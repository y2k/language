open Core__.Common
open Stage__

(* reify macro for java_v2 backend - creates anonymous class implementing interface *)

(* Parse a method definition: (methodName [this arg1 ...] body...) *)
(* Metadata like ^void on method name specifies return type *)
(* Returns: (return_type, method_name, args_with_types, original_arg_names, body) *)
let parse_method = function
  | SList (_, SAtom (m, name) :: SList (_, _ :: _ :: args) :: body) ->
      (* Skip first element (vector) and second element (this) *)
      (* m.symbol contains metadata like "void" from ^void *)
      let arg_names =
        List.map
          (function SAtom (_, n) -> n | x -> failsexp __LOC__ [ x ])
          args
      in
      (m.symbol, name, args, arg_names, body)
  | SList (_, SAtom (m, name) :: SList (_, _ :: []) :: body) ->
      (* Only [this] - no additional args *)
      (m.symbol, name, [], [], body)
  | x -> failsexp __LOC__ [ x ]

(* Generate method argument declarations *)
let generate_method_args args =
  args
  |> List.mapi (fun i a ->
      match a with
      | SAtom (m, _) ->
          let typ = if m.symbol = "" then "Object" else m.symbol in
          Printf.sprintf "%s p%i" typ i
      | x -> failsexp __LOC__ [ x ])
  |> String.concat ", "

(* Generate let bindings to map original arg names to p0, p1, etc *)
let generate_arg_bindings args arg_names body =
  if List.length arg_names = 0 then body
  else
    let bindings =
      List.concat_map
        (fun (i, (arg, name)) ->
          let typ = match arg with SAtom (m, _) -> m.symbol | _ -> "" in
          let meta = { meta_empty with symbol = typ } in
          (* Create binding pair: name p0 *)
          [ SAtom (meta, name); SAtom (meta_empty, Printf.sprintf "p%i" i) ])
        (List.mapi (fun i x -> (i, x)) (List.combine args arg_names))
    in
    (* Wrap body with let expression: (let [name1 p0 name2 p1 ...] body) *)
    SList
      ( meta_empty,
        [
          SAtom (meta_empty, "let");
          SList (meta_empty, SAtom (meta_empty, "vector") :: bindings);
          body;
        ] )

let invoke simplify = function
  | SList (_, SAtom (_, "reify") :: SAtom (_, interface) :: methods) ->
      let parsed = List.map parse_method methods in
      let method_codes =
        parsed
        |> List.map (fun (annot, name, args, arg_names, body) ->
            let ret_type = if annot = "" then "Object" else annot in
            let args_decl = generate_method_args args in
            (* For simplest version, just use the body directly *)
            let body_expr =
              match body with
              | [ single ] -> single
              | _ -> SList (meta_empty, SAtom (meta_empty, "do") :: body)
            in
            (* Wrap body with let bindings for parameter name mapping *)
            let body_with_bindings =
              generate_arg_bindings args arg_names body_expr
            in
            let simplified_body = simplify body_with_bindings in
            (* Flatten nested do* blocks so butlast/last work correctly *)
            let flat_body = Stage_flat_do.invoke simplified_body in
            (* Handle void vs non-void return types *)
            (* Use try-catch to handle exceptions without requiring throws clause *)
            if ret_type = "void" then
              [
                pack_string
                  (Printf.sprintf
                     "\n  @Override public void %s(%s) {\n    try {\n      "
                     name args_decl);
                flat_body;
                pack_string
                  ";\n\
                  \    } catch (Exception e) {\n\
                  \      throw new RuntimeException(e);\n\
                  \    }\n\
                  \  }";
              ]
            else
              (* For non-void return: emit statements first, then return last expression *)
              let butlast = SexpUtil.butlast flat_body in
              let last_expr = SexpUtil.last flat_body in
              if List.length butlast = 0 then
                (* Simple case: just one expression *)
                [
                  pack_string
                    (Printf.sprintf
                       "\n\
                       \  @Override public %s %s(%s) {\n\
                       \    try {\n\
                       \      return (%s)("
                       ret_type name args_decl ret_type);
                  flat_body;
                  pack_string
                    ");\n\
                    \    } catch (Exception e) {\n\
                    \      throw new RuntimeException(e);\n\
                    \    }\n\
                    \  }";
                ]
              else
                (* Multiple statements: emit all but last, then return last *)
                let statements =
                  SList (meta_empty, SAtom (meta_empty, "do*") :: butlast)
                in
                [
                  pack_string
                    (Printf.sprintf
                       "\n  @Override public %s %s(%s) {\n    try {\n      "
                       ret_type name args_decl);
                  statements;
                  pack_string ";\n      return (";
                  pack_string (ret_type ^ ")(");
                  last_expr;
                  pack_string
                    ");\n\
                    \    } catch (Exception e) {\n\
                    \      throw new RuntimeException(e);\n\
                    \    }\n\
                    \  }";
                ])
      in
      (* Build the anonymous class *)
      (* Convert $ to . for inner classes (e.g., Thread$UncaughtExceptionHandler -> Thread.UncaughtExceptionHandler) *)
      let interface_java =
        String.map (fun c -> if c = '$' then '.' else c) interface
      in
      let cls_code =
        [
          SAtom (meta_empty, "__compiler_emit");
          pack_string ("new " ^ interface_java ^ "() {");
        ]
        @ List.flatten method_codes
        @ [ pack_string "\n}" ]
      in
      Some (SList (meta_empty, cls_code))
  | _ -> None
