open Frontend
module StringSet = Set.Make (String)

let is_number name = float_of_string_opt name |> Option.is_some
let is_string name = String.length name >= 2 && name.[0] = '"' && name.[String.length name - 1] = '"'
let string_value value = String.sub value 1 (String.length value - 2)
let js_string = Target_syntax.string_literal

let compile_atom name =
  if name = "nil" then "null"
  else if is_number name then name
  else if is_string name then js_string (string_value name)
  else Symbol_munge.munge name

let compile_constructor_name name = Symbol_munge.munge name |> String.map (function '/' -> '.' | ch -> ch)

let invalid_sexpr ((SAtom (meta, _) | SList (meta, _, _)) as code) =
  Printf.sprintf "%s line %d, pos %d" (show_sexpr code) meta.loc.line meta.loc.column |> failwith

let rec compile_expr = function
  | SList (_, _, [ SAtom (_, "compiler/ns"); _; SList (_, _, requires); _ ]) ->
      let parse_pair = function
        | SList (_, _, [ SAtom (_, left); SAtom (_, right) ]) -> (left, right)
        | _ -> failwith "malformed compiler/ns pair"
      in
      List.map
        (fun (namespace, alias) ->
          "import * as " ^ string_value alias ^ " from \"./"
          ^ String.map (fun c -> if c = '.' then '/' else c) (string_value namespace)
          ^ ".js\";")
        (List.map parse_pair requires)
      |> String.concat "\n"
  | SAtom (_, name) -> compile_atom name |> String.map (function '/' -> '.' | ch -> ch)
  | SList (_, _, [ SAtom (_, "quote"); value ]) -> compile_quote value
  | SList (_, _, [ SAtom (_, "cast"); _; value ]) -> compile_expr value
  | SList (_, _, [ SAtom (_, "def"); SAtom (_, name); value ]) ->
      "const " ^ Symbol_munge.munge name ^ " = " ^ compile_expr value
  | SList (_, _, SAtom (_, "fn*") :: SList (_, _, params) :: body) as code -> compile_fn code params body
  | SList (_, _, SAtom (_, "do") :: body) -> compile_body_iife body
  | SList (_, _, SAtom (_, "let*") :: SList (_, _, bindings) :: body) as code -> compile_let code bindings body
  | SList (_, _, [ SAtom (_, "set!"); SAtom (_, name); value ]) ->
      "(" ^ Symbol_munge.munge name ^ " = " ^ compile_expr value ^ ")"
  | SList (_, _, SAtom (_, "new") :: SAtom (_, class_name) :: args) ->
      "new " ^ compile_constructor_name class_name ^ "(" ^ String.concat ", " (List.map compile_expr args) ^ ")"
  | SList (_, _, SAtom (_, ".") :: instance :: SAtom (_, method_name) :: args) ->
      compile_expr instance ^ "." ^ Symbol_munge.munge method_name ^ "("
      ^ String.concat ", " (List.map compile_expr args)
      ^ ")"
  | SList (_, _, fn :: args) -> "(" ^ compile_expr fn ^ ")(" ^ String.concat ", " (List.map compile_expr args) ^ ")"
  | SList _ as code -> invalid_sexpr code

and compile_quote = function
  | SAtom (_, name) -> js_string (if is_string name then string_value name else name)
  | SList (_, _, items) -> "list(" ^ String.concat ", " (List.map compile_quote items) ^ ")"

and compile_fn code params body =
  let compile_param = function SAtom (_, name) -> Symbol_munge.munge name | _ -> invalid_sexpr code in
  "(("
  ^ String.concat ", " (List.map compile_param params)
  ^ ") => {\n"
  ^ String.concat "\n" (compile_returning_body ~empty:[] body)
  ^ "\n})"

and compile_body_iife body =
  "(() => {\n" ^ String.concat "\n" (compile_returning_body ~empty:[ "return null;" ] body) ^ "\n})()"

and compile_let code bindings body =
  let rec compile_bindings = function
    | [] -> []
    | SAtom (_, name) :: value :: rest ->
        ("let " ^ Symbol_munge.munge name ^ " = " ^ compile_expr value ^ ";") :: compile_bindings rest
    | _ -> invalid_sexpr code
  in
  "(() => {\n" ^ String.concat "\n" (compile_bindings bindings @ compile_returning_body ~empty:[] body) ^ "\n})()"

and compile_returning_body ~empty = function
  | [] -> empty
  | [ expr ] -> [ "return " ^ compile_expr expr ^ ";" ]
  | expr :: rest -> compile_statement expr :: compile_returning_body ~empty rest

and compile_statement = function
  | SList (_, _, [ SAtom (_, "let*"); SAtom (_, name); value ]) ->
      "let " ^ Symbol_munge.munge name ^ " = " ^ compile_expr value ^ ";"
  | SList (_, _, [ SAtom (_, "if"); condition; then_; else_ ]) ->
      "if (truthy(" ^ compile_expr condition ^ ")) {\n" ^ compile_statement then_ ^ "\n} else {\n"
      ^ compile_statement else_ ^ "\n}"
  | SList (_, _, [ SAtom (_, "set!"); SAtom (_, name); value ]) ->
      Symbol_munge.munge name ^ " = " ^ compile_expr value ^ ";"
  | expr -> compile_expr expr ^ ";"

let compile sexprs =
  let runtime_import =
    "import { list, vector_QMARK_, concat, hash_map, truthy, print_result, println, eprintln, str, _PLUS_, _MINUS_, \
     _STAR_, _SLASH_, count, get, map, reduce } from \"./language_runtime.js\";"
  in
  let sexprs = Gensym.run (fun () -> Lowering_expression_to_statement.lower sexprs) in
  runtime_import :: List.map (fun sexpr -> compile_expr sexpr ^ ";") sexprs |> String.concat "\n"
