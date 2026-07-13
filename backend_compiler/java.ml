open Frontend
module StringMap = Map.Make (String)
module StringSet = Set.Make (String)

type ctx = { top_fns : int StringMap.t; locals : StringSet.t; requires : string StringMap.t }
type gen_method = { method_name : string; arg_types : string list; return_type : string; call_super : bool }
type gen_class = { class_name : string; extends_name : string; methods : gen_method list }
type return_mode = Value | Void
type java_lambda_annotation = { target_type : string; return_mode : return_mode }

let runtime_package = "y2k.language"
let is_number name = float_of_string_opt name |> Option.is_some
let is_string name = String.length name >= 2 && name.[0] = '"' && name.[String.length name - 1] = '"'
let string_value value = String.sub value 1 (String.length value - 2)
let java_string = Target_syntax.string_literal
let java_local_name name = if name = "this" then "this_" else Symbol_munge.munge name

let split_class_name name =
  match List.rev (String.split_on_char '.' name) with
  | class_name :: package -> (List.rev package |> String.concat ".", Symbol_munge.munge class_name)
  | [] -> failwith "empty class name"

let nested_class_name name =
  let _, class_name = split_class_name name in
  match List.rev (String.split_on_char '$' class_name) with
  | class_name :: _ when class_name <> "" -> class_name
  | _ -> failwith "empty class name"

let invalid_sexpr loc ((SAtom (meta, _) | SList (meta, _, _)) as code) =
  Printf.sprintf "%s\n%s [%d:%d]" loc (show_sexpr code) meta.loc.line meta.loc.column |> failwith

let fn_interface name arity =
  match arity with
  | 0 | 1 | 2 -> "Fn" ^ string_of_int arity
  | _ -> failwith ("Java backend does not support function arity " ^ string_of_int arity ^ " for " ^ name)

let parse_java_lambda_annotation = function
  | None -> None
  | Some annotation ->
      let void_prefix = "void:" in
      if String.starts_with ~prefix:void_prefix annotation then (
        let target_type =
          String.sub annotation (String.length void_prefix) (String.length annotation - String.length void_prefix)
        in
        if target_type = "" then failwith "Java lambda annotation ^void: requires a target type";
        Some { target_type; return_mode = Void })
      else Some { target_type = annotation; return_mode = Value }

let compile_atom ctx name =
  if name = "nil" then "null"
  else if is_number name then name
  else if is_string name then java_string (string_value name)
  else if StringSet.mem name ctx.locals then java_local_name name
  else
    match StringMap.find_opt name ctx.top_fns with
    | Some arity -> "(" ^ fn_interface name arity ^ ") user::" ^ Symbol_munge.munge name
    | None -> Symbol_munge.munge name

let compile_qualified_name ctx name =
  match String.split_on_char '/' name with
  | [ qualifier; member ] when qualifier <> "" && member <> "" -> (
      match StringMap.find_opt qualifier ctx.requires with
      | Some namespace -> namespace ^ "." ^ Symbol_munge.munge member
      | None -> Symbol_munge.munge qualifier ^ "." ^ Symbol_munge.munge member)
  | _ -> Symbol_munge.munge name

let rec compile_quote = function
  | SAtom (_, name) -> java_string (if is_string name then string_value name else name)
  | SList (_, _, items) -> "list(" ^ String.concat ", " (List.map compile_quote items) ^ ")"

let rec compile_expr ctx = function
  | SAtom (_, name) -> compile_atom ctx name
  | SList (meta, _, SAtom (_, "fn*") :: SList (_, _, args) :: body) ->
      let ctx =
        {
          ctx with
          locals =
            List.fold_left
              (fun locals -> function SAtom (_, name) -> StringSet.add name locals | _ -> failwith __LOC__)
              ctx.locals args;
        }
      in
      let rec compile_body = function
        | [ body ] -> "return " ^ compile_expr ctx body ^ ";"
        | body :: rest -> compile_expr ctx body ^ ";\n" ^ compile_body rest
        | [] -> "return null;"
      in
      let rec compile_void_body = function
        | [] -> []
        | body :: rest -> (compile_expr ctx body ^ ";") :: compile_void_body rest
      in
      let params =
        args |> List.map (function SAtom (_, x) -> java_local_name x | _ -> failwith __LOC__) |> String.concat ", "
      in
      (match parse_java_lambda_annotation meta.type_annotation with
        | Some { target_type; return_mode } ->
            let exception_name = java_local_name (Gensym.gensym_string meta) in
            let body = match return_mode with Value -> [ compile_body body ] | Void -> compile_void_body body in
            [
              [ "((" ^ target_type ^ ")"; "(" ^ params ^ ") -> {"; "try {" ];
              body;
              [
                "} catch (Exception " ^ exception_name ^ ") {"; "throw sneaky_throw(" ^ exception_name ^ ");"; "}"; "})";
              ];
            ]
            |> List.concat
        | None -> [ "((" ^ fn_interface "" (List.length args) ^ ")"; "(" ^ params ^ ") -> {"; compile_body body; "})" ])
      |> String.concat "\n"
  | SList (_, _, [ SAtom (_, "quote"); value ]) -> compile_quote value
  | SList (_, _, [ SAtom (_, "cast"); SAtom (_, type_name); value ]) ->
      "((" ^ type_name ^ ") " ^ compile_expr ctx value ^ ")"
  | SList (_, _, [ SAtom (_, "if"); condition; then_; else_ ]) ->
      [
        "if (truthy(" ^ compile_expr ctx condition ^ ")) {";
        compile_expr ctx then_ ^ ";";
        "} else {";
        compile_expr ctx else_ ^ ";";
        "}";
      ]
      |> String.concat "\n"
  | SList (_, _, [ SAtom (_, "let*"); SAtom (_, key); SAtom (_, "nil") ]) ->
      "Object " ^ java_local_name key ^ " = " ^ "null" ^ ";"
  | SList (_, _, [ SAtom (_, "let*"); SAtom (_, key); value ]) ->
      "var " ^ java_local_name key ^ " = " ^ compile_expr ctx value ^ ";"
  | SList (_, _, SAtom (_, "let*") :: SList (_, _, bingins) :: body) ->
      let rec compile_let_binding ctx = function
        | [] -> ([], ctx)
        | SAtom (_, key) :: value :: rest ->
            let line = "var " ^ java_local_name key ^ " = " ^ compile_expr ctx value ^ ";" in
            let rest, ctx = compile_let_binding { ctx with locals = StringSet.add key ctx.locals } rest in
            (line :: rest, ctx)
        | _ -> failwith __LOC__
      in
      let bindings, ctx = compile_let_binding ctx bingins in
      let rec compile_body = function
        | [] -> []
        | [ expr ] -> [ compile_expr ctx expr ]
        | expr :: rest -> (compile_expr ctx expr ^ ";") :: compile_body rest
      in
      bindings @ compile_body body |> String.concat "\n"
  | SList (_, _, [ SAtom (_, "set!"); SAtom (_, name); value ]) -> java_local_name name ^ " = " ^ compile_expr ctx value
  | SList (_, _, SAtom (_, "new") :: SAtom (_, class_name) :: args) ->
      "new " ^ compile_qualified_name ctx class_name ^ "(" ^ String.concat ", " (List.map (compile_expr ctx) args) ^ ")"
  | SList (_, _, SAtom (_, ".") :: instance :: SAtom (_, method_name) :: args) ->
      compile_expr ctx instance ^ "." ^ Symbol_munge.munge method_name ^ "("
      ^ String.concat ", " (List.map (compile_expr ctx) args)
      ^ ")"
  | SList (_, _, SAtom (_, name) :: args) ->
      compile_qualified_name ctx name ^ "(" ^ String.concat ", " (List.map (compile_expr ctx) args) ^ ")"
  | SList (_, _, name :: args) ->
      compile_expr ctx name ^ ".call(" ^ String.concat ", " (List.map (compile_expr ctx) args) ^ ")"
  | SList _ as code -> invalid_sexpr __LOC__ code

let compile_function ctx code name args body =
  let compile_arg = function SAtom (_, name) -> "Object " ^ java_local_name name | _ -> invalid_sexpr __LOC__ code in
  let locals =
    List.fold_left
      (fun locals -> function SAtom (_, name) -> StringSet.add name locals | _ -> invalid_sexpr __LOC__ code)
      StringSet.empty args
  in
  let ctx = { ctx with locals } in
  let rec compile_body = function
    | [] -> [ "return null;" ]
    | [ body ] -> [ "return " ^ compile_expr ctx body ^ ";" ]
    | body :: rest -> (compile_expr ctx body ^ ";") :: compile_body rest
  in
  [
    "static Object " ^ Symbol_munge.munge name ^ "("
    ^ String.concat ", " (List.map compile_arg args)
    ^ ") throws Exception {";
  ]
  @ compile_body body @ [ "}" ]

let compile_statement ctx = function
  | SList (_, _, SAtom (_, "compiler/ns") :: _) -> []
  | SList (_, _, SAtom (_, "compiler/gen-class") :: _) -> []
  | SList (_, _, [ SAtom (_, "def"); SAtom (_, name); SList (_, _, SAtom (_, "fn*") :: SList (_, _, args) :: body) ]) as
    code ->
      compile_function ctx code name args body
  | SList (_, _, [ SAtom (_, "def"); SAtom (_, name); SAtom (_, value) ]) ->
      let value =
        match StringMap.find_opt value ctx.top_fns with
        | Some _ -> compile_atom { ctx with locals = StringSet.empty } value
        | None -> compile_atom { ctx with locals = StringSet.empty } value
      in
      [ "static Object " ^ Symbol_munge.munge name ^ " = " ^ value ^ ";" ]
  | SList (_, _, [ SAtom (_, "def"); SAtom (_, name); value ]) ->
      [
        "static Object " ^ Symbol_munge.munge name ^ ";";
        "static {";
        java_local_name name ^ " = " ^ compile_expr { ctx with locals = StringSet.empty } value ^ ";";
        "}";
      ]
  | sexpr -> invalid_sexpr __LOC__ sexpr

let collect_top_fns sexprs =
  let collect top_fns = function
    | SList (_, _, [ SAtom (_, "def"); SAtom (_, name); SList (_, _, SAtom (_, "fn*") :: SList (_, _, args) :: _) ]) ->
        let arity = List.length args in
        ignore (fn_interface name arity);
        StringMap.add name arity top_fns
    | _ -> top_fns
  in
  List.fold_left collect StringMap.empty sexprs

let parse_gen_class = function
  | SList
      (_, _, [ SAtom (_, "compiler/gen-class"); SAtom (_, class_name); SAtom (_, extends_name); SList (_, _, methods) ])
    ->
      let parse_method = function
        | SList (_, _, [ SAtom (method_meta, method_name); SList (_, _, arg_types); SAtom (_, return_type) ]) ->
            let parse_arg_type = function
              | SAtom (_, name) -> string_value name
              | sexpr -> invalid_sexpr __LOC__ sexpr
            in
            {
              method_name = string_value method_name;
              arg_types = List.map parse_arg_type arg_types;
              return_type = string_value return_type;
              call_super = method_meta.type_annotation = Some "override";
            }
        | sexpr -> invalid_sexpr __LOC__ sexpr
      in
      Some
        {
          class_name = string_value class_name;
          extends_name = string_value extends_name;
          methods = List.map parse_method methods;
        }
  | _ -> None

let helper_class_decl class_name = "public final class " ^ class_name ^ " {"

let gen_class_decl class_name { extends_name; _ } =
  "public static class " ^ class_name ^ " extends " ^ extends_name ^ " {"

let compile_gen_method helper_class_name { method_name; arg_types; return_type; call_super } =
  if return_type <> "void" then failwith "gen-class: only void methods are supported";
  let params = List.mapi (fun index type_name -> type_name ^ " arg" ^ string_of_int index) arg_types in
  let call_args = "this" :: List.mapi (fun index _ -> "arg" ^ string_of_int index) arg_types in
  let super_args = List.mapi (fun index _ -> "arg" ^ string_of_int index) arg_types in
  let super_call = if call_super then [ "super." ^ method_name ^ "(" ^ String.concat ", " super_args ^ ");" ] else [] in
  [ "@Override"; "public " ^ return_type ^ " " ^ method_name ^ "(" ^ String.concat ", " params ^ ") {"; "try {" ]
  @ super_call
  @ [
      helper_class_name ^ "." ^ Symbol_munge.munge ("-" ^ method_name) ^ "(" ^ String.concat ", " call_args ^ ");";
      "} catch (Exception e) {";
      "throw sneaky_throw(e);";
      "}";
      "}";
    ]

let compile_ns (ctx : ctx) gen_class = function
  | SList (_, _, [ SAtom (_, "compiler/ns"); SAtom (_, namespace); SList (_, _, requires); SList (_, _, imports) ]) ->
      let parse_pair = function
        | SList (_, _, [ SAtom (_, left); SAtom (_, right) ]) -> (string_value left, string_value right)
        | sexpr -> invalid_sexpr __LOC__ sexpr
      in
      let namespace = string_value namespace in
      let package, class_name = split_class_name namespace in
      let package =
        match gen_class with
        | Some { class_name; _ } ->
            let gen_package, _ = split_class_name class_name in
            if gen_package <> "" && gen_package <> package && gen_package <> namespace then
              failwith "gen-class: :name package must match namespace package";
            package
        | None -> package
      in
      let import_lines = List.map parse_pair imports |> List.map (fun (_, full_name) -> "import " ^ full_name ^ ";") in
      let requires = List.map parse_pair requires |> List.map (fun (namespace, alias) -> (alias, namespace)) in
      let ctx = { ctx with requires = List.to_seq requires |> StringMap.of_seq } in
      let package_lines = if package = "" then [] else [ "package " ^ package ^ ";"; "" ] in
      ( [ package_lines; [ "import static " ^ runtime_package ^ ".language_runtime.*;" ]; import_lines; [ "" ] ]
        |> List.concat |> String.concat "\n",
        ctx,
        class_name )
  | _ ->
      let package = match gen_class with Some { class_name; _ } -> fst (split_class_name class_name) | None -> "" in
      let package_lines = if package = "" then [] else [ "package " ^ package ^ ";"; "" ] in
      ( (package_lines
        @ [ "import static " ^ runtime_package ^ ".language_runtime.*;" ]
        @ if package = "" then [] else [ "" ])
        |> String.concat "\n",
        ctx,
        "user" )

let compile sexprs =
  Gensym.run (fun () ->
      let lowered = Lowering_expression_to_statement.lower sexprs in
      let gen_class = List.find_map parse_gen_class lowered in
      let top_fns = collect_top_fns lowered in
      let ctx = { top_fns; locals = StringSet.empty; requires = StringMap.empty } in
      let prefix, ctx, helper_class_name = compile_ns ctx gen_class (List.hd lowered) in
      let statements = List.concat_map (compile_statement ctx) lowered in
      match gen_class with
      | None ->
          [ [ prefix; helper_class_decl helper_class_name ]; statements; [ "}" ] ] |> List.flatten |> String.concat "\n"
      | Some gen_class ->
          let gen_class_name = nested_class_name gen_class.class_name in
          let gen_methods = List.concat_map (compile_gen_method helper_class_name) gen_class.methods in
          [
            [ prefix; helper_class_decl helper_class_name ];
            statements;
            [ gen_class_decl gen_class_name gen_class ];
            gen_methods;
            [ "}"; "}" ];
          ]
          |> List.flatten |> String.concat "\n")
