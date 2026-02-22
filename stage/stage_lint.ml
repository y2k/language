open Core__.Common
module StringSet = Set.Make (String)

type arity = Fixed of int | Variadic of int

type lint_ctx = {
  defined_vars : StringSet.t;
  fn_arities : arity StringMap.t;
  prelude_fns : StringSet.t;
  filename : string;
}

let is_literal_or_special name =
  String.length name = 0
  || int_of_string_opt name <> None
  || float_of_string_opt name <> None
  || String.starts_with ~prefix:"\"" name
  || String.starts_with ~prefix:":" name
  || name = "true" || name = "false" || name = "nil" || name = "&" || name = "."
  || String.contains name '/' || String.contains name '.'
  || String.ends_with ~suffix:"." name
  || String.starts_with ~prefix:"." name
  || (String.ends_with ~suffix:"*" name && name <> "*")
  || String.starts_with ~prefix:"^" name

let rec extract_arg_names args =
  args
  |> List.filter_map (function
    | SAtom (_, "&") -> None
    | SAtom (_, name) when not (String.starts_with ~prefix:"^" name) ->
        Some name
    | SAtom (_, _) -> None
    | SList (_, items) -> Some (extract_arg_names items |> String.concat ","))

let compute_arity args =
  let has_variadic =
    List.exists (function SAtom (_, "&") -> true | _ -> false) args
  in
  let required_count =
    args
    |> List.filter (function
      | SAtom (_, "&") -> false
      | SAtom (_, name) -> not (String.starts_with ~prefix:"^" name)
      | SList _ -> true)
    |> List.length
  in
  if has_variadic then Variadic (required_count - 1) else Fixed required_count

let add_args_to_ctx ctx args =
  let arg_names =
    args
    |> List.filter_map (function
      | SAtom (_, "&") -> None
      | SAtom (_, name) when not (String.starts_with ~prefix:"^" name) ->
          Some [ name ]
      | SAtom (_, _) -> None
      | SList (_, items) -> Some (extract_arg_names items))
    |> List.flatten
  in
  {
    ctx with
    defined_vars =
      List.fold_left
        (fun acc name -> StringSet.add name acc)
        ctx.defined_vars arg_names;
  }

let lint_error m filename msg =
  Printf.sprintf "Lint error at %s:%d:%d: %s" filename m.line m.pos msg

let check_undefined ctx m name =
  if
    is_literal_or_special name
    || StringSet.mem name ctx.prelude_fns
    || StringSet.mem name ctx.defined_vars
  then ()
  else
    failwith (lint_error m ctx.filename ("Undefined variable '" ^ name ^ "'"))

let check_arity ctx m fn_name args_count =
  if is_literal_or_special fn_name || StringSet.mem fn_name ctx.prelude_fns then
    ()
  else
    match StringMap.find_opt fn_name ctx.fn_arities with
    | Some (Fixed n) when n <> args_count ->
        failwith
          (lint_error m ctx.filename
             (Printf.sprintf
                "Function '%s' expects %d argument(s), but %d provided" fn_name
                n args_count))
    | Some (Variadic min) when args_count < min ->
        failwith
          (lint_error m ctx.filename
             (Printf.sprintf
                "Function '%s' expects at least %d argument(s), but %d provided"
                fn_name min args_count))
    | _ -> ()

let rec lint ctx node =
  match node with
  | SList (_, [ SAtom (_, "quote*"); _ ]) -> ctx
  | SList (_, SAtom (_, "__compiler_emit") :: _) -> ctx
  | SAtom (m, name) ->
      check_undefined ctx m name;
      ctx
  | SList
      ( _,
        [
          SAtom (_, "def*");
          SAtom (_, name);
          SList (_, [ SAtom (_, "fn*"); SList (_, args); body ]);
        ] ) ->
      let arity = compute_arity args in
      let ctx =
        {
          ctx with
          defined_vars = StringSet.add name ctx.defined_vars;
          fn_arities = StringMap.add name arity ctx.fn_arities;
        }
      in
      let fn_ctx = add_args_to_ctx ctx args in
      let _ = lint fn_ctx body in
      ctx
  | SList (_, [ SAtom (_, "def*"); SAtom (_, name); value ]) ->
      let ctx =
        { ctx with defined_vars = StringSet.add name ctx.defined_vars }
      in
      lint ctx value
  | SList (_, [ SAtom (_, "let*"); SAtom (_, name); value ]) ->
      let ctx = lint ctx value in
      { ctx with defined_vars = StringSet.add name ctx.defined_vars }
  | SList (_, [ SAtom (_, "let*"); SAtom (_, name) ]) ->
      { ctx with defined_vars = StringSet.add name ctx.defined_vars }
  | SList (_, [ SAtom (_, "fn*"); SList (_, args); body ]) ->
      let fn_ctx = add_args_to_ctx ctx args in
      let _ = lint fn_ctx body in
      ctx
  | SList (_, SAtom (_, "if*") :: args) -> List.fold_left lint ctx args
  | SList (_, SAtom (_, "do*") :: body) -> List.fold_left lint ctx body
  | SList (_, [ SAtom (_, "set_BANG_"); SAtom (m, name); value ]) ->
      check_undefined ctx m name;
      lint ctx value
  | SList (_, SAtom (_, ".") :: target :: _method :: args) ->
      let ctx = lint ctx target in
      List.fold_left lint ctx args
  | SList (_, [ SAtom (_, "cast"); _type; expr ]) -> lint ctx expr
  | SList (_, SAtom (_, "new") :: _class :: args) ->
      List.fold_left lint ctx args
  | SList (_, [ SAtom (_, "instance?"); _class; expr ]) -> lint ctx expr
  | SList (_, [ SAtom (_, "throw"); expr ]) -> lint ctx expr
  | SList (m, SAtom (_, fn_name) :: args)
    when not (String.ends_with ~suffix:"*" fn_name && fn_name <> "*") ->
      check_arity ctx m fn_name (List.length args);
      List.fold_left lint ctx args
  | SList (_, items) -> List.fold_left lint ctx items

let invoke ~prelude_fns ~filename node =
  if String.ends_with ~suffix:"prelude.clj" filename then node
  else
    let ctx =
      {
        defined_vars = StringSet.empty;
        fn_arities = StringMap.empty;
        prelude_fns = StringSet.of_list prelude_fns;
        filename;
      }
    in
    let _ = lint ctx node in
    node
