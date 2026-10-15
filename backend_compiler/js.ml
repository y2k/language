open Frontend
module StringSet = Set.Make (String)

type fragment = Source_map.t -> unit

let text value : fragment = fun writer -> Source_map.write writer value
let concat fragments : fragment = fun writer -> List.iter (fun fragment -> fragment writer) fragments

let join separator fragments : fragment =
 fun writer ->
  match fragments with
  | [] -> ()
  | first :: rest ->
      first writer;
      List.iter
        (fun fragment ->
          Source_map.write writer separator;
          fragment writer)
        rest

let at_form code fragment : fragment =
  match code with
  (* V8 may report a call at its callee/receiver atom; retain the call's origin. *)
  | SAtom _ -> fragment
  | SList (meta, _, _) -> fun writer -> Source_map.with_origin writer (Some meta.loc) (fun () -> fragment writer)

let is_number name = float_of_string_opt name |> Option.is_some
let is_string name = String.length name >= 2 && name.[0] = '"' && name.[String.length name - 1] = '"'
let string_value value = String.sub value 1 (String.length value - 2)
let js_string = Target_syntax.string_literal

let output_root_prefix namespace =
  let depth = String.fold_left (fun depth ch -> if ch = '.' then depth + 1 else depth) 0 namespace in
  if depth = 0 then "./" else String.concat "" (List.init depth (fun _ -> "../"))

let compile_atom name =
  if name = "nil" then "null"
  else if is_number name then name
  else if is_string name then js_string (string_value name)
  else Symbol_munge.munge name |> String.map (function '/' -> '.' | ch -> ch)

let compile_constructor_name name = Symbol_munge.munge name |> String.map (function '/' -> '.' | ch -> ch)

let invalid_sexpr ((SAtom (meta, _) | SList (meta, _, _)) as code) =
  Printf.sprintf "%s line %d, pos %d" (show_sexpr code) meta.loc.line meta.loc.column |> failwith

let rec compile_expr code = at_form code (compile_expr_inner code)

and compile_expr_inner = function
  | SList (_, _, [ SAtom (_, "compiler/ns"); SAtom (_, current_namespace); SList (_, _, requires); _ ]) ->
      let parse_pair = function
        | SList (_, _, [ SAtom (_, left); SAtom (_, right) ]) -> (left, right)
        | _ -> failwith "malformed compiler/ns pair"
      in
      let root_prefix = current_namespace |> string_value |> output_root_prefix in
      List.map
        (fun (namespace, alias) ->
          let namespace = string_value namespace in
          (* Quoted content marks a source string require; serialize its decoded value. *)
          if is_string namespace then
            concat
              [
                text "import * as ";
                text (string_value alias |> Symbol_munge.munge);
                text " from ";
                text (js_string (string_value namespace));
                text ";";
              ]
          else
            concat
              [
                text "import * as ";
                text (string_value alias);
                text " from \"";
                text root_prefix;
                text (namespace |> Symbol_munge.munge |> String.map (fun c -> if c = '.' then '/' else c));
                text ".js\";";
              ])
        (List.map parse_pair requires)
      |> join "\n"
  | SAtom (_, name) -> text (compile_atom name)
  | SList (_, _, [ SAtom (_, "quote"); value ]) -> compile_quote value
  | SList (_, _, SAtom (_, "raw-code") :: _) as code -> Raw_code.error code "not allowed in an expression"
  | SList (_, _, SAtom (_, "instance?") :: _) -> failwith "instance? is supported only on the Java target"
  | SList (_, _, [ SAtom (_, "cast"); _; value ]) -> compile_expr value
  | SList (meta, _, [ SAtom (_, "def"); SAtom (_, name); value ]) ->
      concat
        [
          text (if meta.private_ then "" else "export ");
          text "const ";
          text (Symbol_munge.munge name);
          text " = ";
          compile_expr value;
        ]
  | SList (_, _, SAtom (_, "export-default") :: items) ->
      let error () = failwith "export-default: expected one or more key/value pairs" in
      let rec compile_properties = function
        | [] -> []
        | SAtom (_, key) :: value :: rest when is_string key ->
            concat [ text "["; text (compile_atom key); text "]: "; compile_expr value ] :: compile_properties rest
        | _ -> error ()
      in
      if items = [] then error ()
      else concat [ text "export default {"; join ", " (compile_properties items); text "}" ]
  | SList (_, _, SAtom (_, "fn*") :: SList (_, _, params) :: body) as code -> compile_fn code params body
  | SList (_, _, SAtom (_, "do") :: body) -> compile_body_iife body
  | SList (_, _, SAtom (_, "let*") :: SList (_, _, bindings) :: body) as code -> compile_let code bindings body
  | SList (_, _, [ SAtom (_, "set!"); SAtom (_, name); value ]) ->
      concat [ text "("; text (Symbol_munge.munge name); text " = "; compile_expr value; text ")" ]
  | SList (_, _, SAtom (_, "new") :: SAtom (_, class_name) :: args) ->
      concat
        [
          text "new ";
          text (compile_constructor_name class_name);
          text "(";
          join ", " (List.map compile_expr args);
          text ")";
        ]
  | SList (_, _, SAtom (_, ".") :: instance :: SAtom (_, method_name) :: args) ->
      concat
        [
          compile_expr instance;
          text ".";
          text (Symbol_munge.munge method_name);
          text "(";
          join ", " (List.map compile_expr args);
          text ")";
        ]
  | SList (_, _, fn :: args) ->
      concat [ text "("; compile_expr fn; text ")("; join ", " (List.map compile_expr args); text ")" ]
  | SList _ as code -> invalid_sexpr code

and compile_quote = function
  | SAtom (_, name) -> text (js_string (if is_string name then string_value name else name))
  | SList (_, _, items) -> concat [ text "list("; join ", " (List.map compile_quote items); text ")" ]

and compile_fn code params body =
  let compile_param = function SAtom (_, name) -> text (Symbol_munge.munge name) | _ -> invalid_sexpr code in
  concat
    [
      text "((";
      join ", " (List.map compile_param params);
      text ") => {\n";
      join "\n" (compile_returning_body ~empty:[] body);
      text "\n})";
    ]

and compile_body_iife body =
  concat [ text "(() => {\n"; join "\n" (compile_returning_body ~empty:[ text "return null;" ] body); text "\n})()" ]

and compile_let code bindings body =
  let rec compile_bindings = function
    | [] -> []
    | SAtom (_, name) :: value :: rest ->
        concat [ text "let "; text (Symbol_munge.munge name); text " = "; compile_expr value; text ";" ]
        :: compile_bindings rest
    | _ -> invalid_sexpr code
  in
  concat
    [ text "(() => {\n"; join "\n" (compile_bindings bindings @ compile_returning_body ~empty:[] body); text "\n})()" ]

and compile_returning_body ~empty = function
  | [] -> empty
  | [ expr ] -> [ concat [ text "return "; compile_expr expr; text ";" ] ]
  | expr :: rest -> compile_statement expr :: compile_returning_body ~empty rest

and compile_statement code = at_form code (compile_statement_inner code)

and compile_statement_inner = function
  | SList (_, _, SAtom (_, "raw-code") :: _) as code -> text (Raw_code.payload code)
  | SList (_, _, SAtom (_, "let*") :: SList (_, _, []) :: body) ->
      concat [ text "{\n"; join "\n" (List.map compile_statement body); text "\n}" ]
  | SList (_, _, [ SAtom (_, "let*"); SAtom (_, name); value ]) ->
      concat [ text "let "; text (Symbol_munge.munge name); text " = "; compile_expr value; text ";" ]
  | SList (_, _, [ SAtom (_, "if"); condition; then_; else_ ]) ->
      concat
        [
          text "if (truthy(";
          compile_expr condition;
          text ")) {\n";
          compile_statement then_;
          text "\n} else {\n";
          compile_statement else_;
          text "\n}";
        ]
  | SList (_, _, [ SAtom (_, "set!"); SAtom (_, name); value ]) ->
      concat [ text (Symbol_munge.munge name); text " = "; compile_expr value; text ";" ]
  | expr -> concat [ compile_expr expr; text ";" ]

let compile ?source ?source_file sexprs =
  let sexprs = Gensym.run (fun () -> Lowering_expression_to_statement.lower sexprs) in
  let root_prefix =
    List.find_map
      (function
        | SList (_, _, [ SAtom (_, "compiler/ns"); SAtom (_, namespace); _; _ ]) ->
            Some (namespace |> string_value |> output_root_prefix)
        | _ -> None)
      sexprs
    |> Option.value ~default:"./"
  in
  let runtime_import =
    concat
      [
        text
          "import { list, vector_QMARK_, string_QMARK_, concat, conj, hash_map, assoc, merge, truthy, not, \
           print_result, println, eprintln, str, _EQ_, not_EQ_, _PLUS_, _GT_, _LT_, _GT__EQ_, _LT__EQ_, _MINUS_, \
           _STAR_, _SLASH_, count, get, get_in, map, filter, keep, run_BANG_, sort_by, reduce, drop, atom, deref, \
           reset_BANG_, swap_BANG_, re_pattern, re_find, re_find_groups, re_replace } from \"";
        text root_prefix;
        text "language_runtime.js\";";
      ]
  in
  let writer = Source_map.create ?source ?source_file () in
  let output =
    runtime_import
    :: List.map
         (fun ((SAtom (meta, _) | SList (meta, _, _)) as sexpr) ->
           let fragment =
             match sexpr with
             | SList (_, _, SAtom (_, "raw-code") :: _) -> text (Raw_code.payload sexpr)
             | _ -> concat [ compile_expr sexpr; text ";" ]
           in
           fun writer -> Source_map.with_origin writer (Some meta.loc) (fun () -> fragment writer))
         sexprs
    |> join "\n"
  in
  output writer;
  Source_map.finish writer
