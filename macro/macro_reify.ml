open Core__.Common

(* reify macro for java_v2 backend - creates anonymous class implementing interface *)

(* Parse a method definition: (methodName [this arg1 ...] body...) *)
(* Metadata like ^void on method name specifies return type *)
let parse_method = function
  | SList (_, SAtom (m, name) :: SList (_, _ :: _ :: args) :: body) ->
      (* Skip first element (vector) and second element (this) *)
      (* m.symbol contains metadata like "void" from ^void *)
      (m.symbol, name, args, body)
  | SList (_, SAtom (m, name) :: SList (_, _ :: []) :: body) ->
      (* Only [this] - no additional args *)
      (m.symbol, name, [], body)
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

let invoke simplify = function
  | SList (_, SAtom (_, "reify") :: SAtom (_, interface) :: methods) ->
      let parsed = List.map parse_method methods in
      let method_codes =
        parsed
        |> List.map (fun (annot, name, args, body) ->
            let ret_type = if annot = "" then "Object" else annot in
            let args_decl = generate_method_args args in
            (* For simplest version, just use the body directly *)
            let body_expr =
              match body with
              | [ single ] -> single
              | _ -> SList (meta_empty, SAtom (meta_empty, "do") :: body)
            in
            let simplified_body = simplify body_expr in
            (* Build method code using __compiler_emit *)
            (* Handle void vs non-void return types *)
            (* Use try-catch to handle exceptions without requiring throws clause *)
            if ret_type = "void" then
              SList
                ( meta_empty,
                  [
                    SAtom (meta_empty, "__compiler_emit");
                    pack_string
                      (Printf.sprintf
                         "\n  @Override public void %s(%s) {\n    try {\n      "
                         name args_decl);
                    simplified_body;
                    pack_string
                      ";\n\
                      \    } catch (Exception e) {\n\
                      \      throw new RuntimeException(e);\n\
                      \    }\n\
                      \  }";
                  ] )
            else
              SList
                ( meta_empty,
                  [
                    SAtom (meta_empty, "__compiler_emit");
                    pack_string
                      (Printf.sprintf
                         "\n\
                         \  @Override public %s %s(%s) {\n\
                         \    try {\n\
                         \      return (Object)("
                         ret_type name args_decl);
                    simplified_body;
                    pack_string
                      ");\n\
                      \    } catch (Exception e) {\n\
                      \      throw new RuntimeException(e);\n\
                      \    }\n\
                      \  }";
                  ] ))
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
        @ method_codes
        @ [ pack_string "\n}" ]
      in
      Some (SList (meta_empty, cls_code))
  | _ -> None
