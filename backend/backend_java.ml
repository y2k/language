open Core__.Common
open Core__
open Stage__

type compile_opt = { namespace_root : string }

let fix_class_name clazz =
  if String.ends_with ~suffix:".class" clazz then
    String.sub clazz 0 (String.length clazz - 6)
  else clazz

let escape_string_for_java s =
  (* Only escape actual control characters, not existing escape sequences *)
  let buf = Buffer.create (String.length s) in
  String.iter
    (function
      | '\n' -> Buffer.add_string buf "\\n"
      | '\r' -> Buffer.add_string buf "\\r"
      | '\t' -> Buffer.add_string buf "\\t"
      | c -> Buffer.add_char buf c)
    s;
  Buffer.contents buf

let rec compile (ctx : compile_opt) sexp =
  match sexp with
  | SAtom (_, "nil") -> "null"
  | SAtom (_, x) when String.starts_with ~prefix:":" x ->
      "\"" ^ unpack_symbol x ^ "\""
  | SAtom (_, x) when String.starts_with ~prefix:"\"" x ->
      let inner = unpack_string x in
      "\"" ^ escape_string_for_java inner ^ "\""
  | SAtom (_, x) -> x
  (* TODO: handle new namespaces *)
  | SList (_, [ SAtom (_, "def*"); SAtom (_, "__NS__"); _ ]) -> ""
  (* Operators *)
  | SList (_, SAtom (_, op) :: args) when op = "_PLUS_" ->
      List.map (compile ctx) args
      |> String.concat " + " |> Printf.sprintf "(%s)"
  | SList (_, SAtom (_, op) :: args) when op = "_MINUS_" ->
      List.map (compile ctx) args
      |> String.concat " - " |> Printf.sprintf "(%s)"
  | SList (_, SAtom (_, op) :: args)
    when op = "+" || op = "*" || op = "/" || op = "<" || op = "<=" || op = ">"
         || op = ">=" ->
      List.map (compile ctx) args
      |> String.concat (" " ^ op ^ " ")
      |> Printf.sprintf "(%s)"
  | SList (_, [ SAtom (_, "mod"); x; y ]) ->
      Printf.sprintf "(%s %% %s)" (compile ctx x) (compile ctx y)
  (* *)
  | SList (_, [ SAtom (_, "int"); x ]) ->
      compile ctx x |> Printf.sprintf "((int)%s)"
  | SList (_, [ SAtom (_, "double"); x ]) ->
      compile ctx x |> Printf.sprintf "((double)%s)"
  (* *)
  | SList (_, SAtom (_, "do*") :: body) ->
      body |> List.map (compile ctx) |> String.concat ";\n"
  | SList (_, [ SAtom (_, "let*"); SAtom (mt, name) ]) ->
      let type_ =
        if mt.symbol = "private" || mt.symbol = "" then "Object" else mt.symbol
      in
      Printf.sprintf "%s %s" type_ name
  | SList (_, [ SAtom (_, "let*"); SAtom (mt, name); value ]) ->
      let type_ =
        if mt.symbol = "private" || mt.symbol = "" then "var" else mt.symbol
      in
      let cast_type = if type_ = "var" then "" else "(" ^ type_ ^ ")" in
      Printf.sprintf "%s %s=%s%s" type_ name cast_type (compile ctx value)
  | SList (_, [ SAtom (_, "not"); x ]) ->
      Printf.sprintf "(!(%s))" (compile ctx x)
  | SList (_, [ SAtom (_, "set!"); name; value ]) ->
      Printf.sprintf "%s=%s" (compile ctx name) (compile ctx value)
  (* Function definition: def* with fn* -> static method *)
  | SList
      ( _,
        [
          SAtom (m, "def*");
          SAtom (_, name);
          SList (_, [ SAtom (_, "fn*"); SList (_, args); body ]);
        ] ) ->
      let visibility = if m.symbol = "private" then "private" else "public" in
      let args =
        List.map
          (function SAtom (_, x) -> x | x -> failsexp __LOC__ [ x ])
          args
      in
      let sargs =
        args |> List.map (fun a -> "Object " ^ a) |> String.concat ", "
      in
      let body = unwrap_sexp_do body in
      let last_body = body |> List.rev |> List.hd |> compile ctx in
      let body =
        body |> List.rev |> List.tl |> List.rev
        |> List.map (compile ctx)
        |> List.map (fun x -> x ^ ";")
        |> String.concat "\n"
      in
      Printf.sprintf
        "%s static Object %s(%s) throws Exception {\n%s\nreturn %s;\n}"
        visibility name sargs body last_body
  (* Non-function def* -> static field *)
  | SList (_, [ SAtom (m, "def*"); SAtom (_, name); value ]) ->
      let visibility = if m.symbol = "private" then "private" else "public" in
      Printf.sprintf "%s static final Object %s = %s" visibility name
        (compile ctx value)
  (* Anonymous function - still uses lambdas for inline functions *)
  | SList (m, [ SAtom (_, "fn*"); SList (_, args); body ]) -> (
      let args =
        List.map
          (function SAtom (_, x) -> x | x -> failsexp __LOC__ [ x ])
          args
      in
      let body = unwrap_sexp_do body in
      let last_body = body |> List.rev |> List.hd |> compile ctx in
      let body =
        body |> List.rev |> List.tl |> List.rev
        |> List.map (compile ctx)
        |> List.map (fun x -> x ^ ";")
        |> String.concat "\n"
      in
      let sargs = String.concat "," args in
      match m.symbol with
      | "" ->
          Printf.sprintf "y2k.prelude.fn((%s)->{\n%s\nreturn %s;\n})" sargs body
            last_body
      | type_ when String.contains type_ ':' ->
          let type_parts = String.split_on_char ':' type_ in
          Printf.sprintf "(%s)(%s)->{\n%s%s;\n}" (List.hd type_parts) sargs body
            last_body
      | type_ ->
          let type_ =
            type_ |> String.map (fun c -> if c = '$' then '.' else c)
          in
          Printf.sprintf "(%s)(%s)->{\n%sreturn %s;\n}" type_ sargs body
            last_body)
  | SList (_, [ SAtom (_, "quote*"); SAtom (_, value) ]) -> value
  | SList (_, SAtom (_, "__compiler_emit") :: args) ->
      (* For __compiler_emit, strings should be emitted directly without escaping *)
      let compile_emit_arg = function
        | SAtom (_, x) when String.starts_with ~prefix:"\"" x ->
            (* Raw string - emit directly without Java escaping *)
            unpack_string x
        | other -> compile ctx other
      in
      args |> List.map compile_emit_arg |> String.concat "" |> Scanf.unescaped
  | SList (_, [ SAtom (_, "if*"); cond; then_; else_ ]) ->
      let cond =
        compile ctx
          (SList (meta_empty, [ SAtom (meta_empty, "prelude.toBoolean"); cond ]))
      in
      let then_ = compile ctx then_ in
      let else_ = compile ctx else_ in
      Printf.sprintf "if (%s) {\n%s;\n} else {\n%s;\n}" cond then_ else_
  | SList (_, [ SAtom (_, "cast"); SAtom (_, type_); value ]) ->
      let type_ = unpack_string type_ in
      let type_ = fix_class_name type_ in
      let value = compile ctx value in
      Printf.sprintf "((%s)%s)" type_ value
  (* instanceof *)
  | SList (_, [ SAtom (_, "instance_QMARK_"); SAtom (_, type_); instance ]) ->
      let instance = compile ctx instance in
      let type_ = Str.global_replace (Str.regexp "\\.class$") "" type_ in
      Printf.sprintf "(%s instanceof %s)" instance type_
  (* Constructor *)
  | SList (_, SAtom (_, "new") :: SAtom (_, clazz) :: args) ->
      let clazz = fix_class_name clazz in
      let args = List.map (compile ctx) args in
      Printf.sprintf "new %s(%s)" clazz (String.concat "," args)
  (* Interop field access *)
  | SList (_, [ SAtom (_, "."); instance; SAtom (_, field) ])
    when String.starts_with ~prefix:"-" field ->
      let instance = compile ctx instance in
      Printf.sprintf "%s.%s" instance
        (String.sub field 1 (String.length field - 1))
  (* Interop call *)
  | SList (_, SAtom (_, ".") :: instance :: SAtom (_, method_) :: args) ->
      let instance = compile ctx instance in
      let args = List.map (compile ctx) args |> String.concat "," in
      Printf.sprintf "%s.%s(%s)" instance method_ args
  | SList (_, SAtom (_, name) :: _) as x when String.ends_with ~suffix:"*" name
    ->
      failsexp __LOC__ [ x ]
  (* Function call *)
  | SList (_, fn :: args) -> (
      let sargs = List.map (compile ctx) args |> String.concat ",\n" in
      match fn with
      | SAtom (_, name) ->
          if String.contains name '#' then
            (* Cross-namespace call: ns#func -> direct static method call *)
            let name = String.map (fun x -> if x = '#' then '.' else x) name in
            Printf.sprintf "%s(\n%s)" name sargs
          else if String.contains name '.' then
            Printf.sprintf "%s(\n%s)" name sargs
          else if String.contains name '/' then
            let name = String.map (fun x -> if x = '/' then '.' else x) name in
            Printf.sprintf "%s(\n%s)" name sargs
          else if String.starts_with ~prefix:"p__" name then
            (* Local temporary variable - use RT.invoke *)
            (if sargs = "" then "" else ",\n" ^ sargs)
            |> Printf.sprintf "y2k.prelude.invoke(\n%s%s)" name
          else
            (* Assume it's a static method - call directly *)
            Printf.sprintf "%s(\n%s)" name sargs
      | x -> (
          (* For non-atom function expressions, still use RT.invoke *)
          let fn = compile ctx x in
          match sargs with
          | "" -> Printf.sprintf "y2k.prelude.invoke(\n%s)" fn
          | _ -> Printf.sprintf "y2k.prelude.invoke(\n%s,\n%s)" fn sargs))
  | x -> failsexp __LOC__ [ x ]

let rec get_namespace : sexp -> string option = function
  | SList (_, SAtom (_, "do*") :: childs) ->
      childs |> List.find_map get_namespace
  | SList (_, [ SAtom (_, "def*"); SAtom (_, "__namespace"); SAtom (_, name) ])
    ->
      Some (unpack_symbol name)
  | _ -> None

let compute_package opt sexp =
  let pkg =
    get_namespace sexp
    |> Option.value ~default:"user"
    |> String.split_on_char '.' |> List.rev |> List.tl |> List.rev
    |> String.concat "."
  in
  if pkg = "" then opt.namespace_root else opt.namespace_root ^ "." ^ pkg

let do_compile (opt : compile_opt) sexp =
  let pkg = compute_package opt sexp in
  let clazz =
    let ns = get_namespace sexp in
    ns
    |> Option.value ~default:"user"
    |> String.split_on_char '.' |> List.rev |> List.hd
  in
  let body = compile opt sexp in
  Printf.sprintf
    "package %s;\n\n\
     @SuppressWarnings({ \"unchecked\", \"rawtypes\" })\n\
     public class %s {\n\
     %s;\n\
     }"
    pkg clazz body

let get_macro ~builtin_macro node =
  let ctx =
    Backend_eval.eval_ (Backend_eval.create_prelude_context ~builtin_macro) node
    |> fst
  in
  Backend_eval.get_all_functions ctx

let compile ~builtin_macro ~(namespace : string) ~(log : bool)
    ~(filename : string) code =
  Common.NameGenerator.with_scope (fun () ->
      code
      |> Frontend_simplify.do_simplify ~builtin_macro (get_macro ~builtin_macro)
           { log; macro = Lazy.force Prelude.prelude_java_macro; filename }
      |> Stage_escape_names.invoke
      |> log_stage log "Stage_escape_names"
      |> Stage_convert_if_to_statment.invoke
      |> log_stage log "if_to_statement "
      |> Stage_resolve_import.do_resolve
      |> log_stage log "Stage_resolve_import"
      |> Stage_alias_to_class.invoke ~root_namespace:namespace
      |> log_stage log "Stage_alias_to_class"
      |> Stage_fun_args_type.invoke
      |> log_stage log "Stage_fun_args_type"
      |> Stage_flat_do.invoke
      |> log_stage log "Stage_flat_do"
      |> do_compile { namespace_root = namespace })
