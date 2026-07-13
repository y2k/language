open Frontend
include Eval_types

type context = {
  namespaces : (string * env) list ref;
  requires : (string * (string * string) list) list ref;
  current_namespace : string ref;
  env : env;
}

type package_file = { package_source : string }
type _ Effect.t += Read_package_files : string * string -> package_file list Effect.t

let create_context ?(env = []) () = { namespaces = ref []; requires = ref []; current_namespace = ref "user"; env }
let with_env context env = { context with env }
let is_string name = String.length name >= 2 && name.[0] = '"' && name.[String.length name - 1] = '"'
let string_value value = String.sub value 1 (String.length value - 2)
let is_literal name = name = "nil" || name = "true" || name = "false" || Option.is_some (float_of_string_opt name)
let read_package_files package version = Effect.perform (Read_package_files (package, version))
let read_file path = In_channel.with_open_text path In_channel.input_all

let filesystem_package_files package version =
  let dir = Filename.concat (Filename.concat (Sys.getenv "LY2K_PACKAGES_DIR") package) version in
  Sys.readdir dir |> Array.to_seq
  |> Seq.filter (fun name -> Filename.check_suffix name ".clj")
  |> Seq.map (fun name ->
      let path = Filename.concat dir name in
      { package_source = read_file path })
  |> List.of_seq

let with_package_loader (load : string -> string -> package_file list) f =
  Effect.Deep.match_with f ()
    {
      retc = Fun.id;
      exnc = (fun e -> Printexc.raise_with_backtrace e (Printexc.get_raw_backtrace ()));
      effc =
        (fun (type a) (eff : a Effect.t) ->
          match eff with
          | Read_package_files (package, version) ->
              Some (fun (k : (a, _) Effect.Deep.continuation) -> Effect.Deep.continue k (load package version))
          | _ -> None);
    }

let with_filesystem f = with_package_loader filesystem_package_files f

let current_globals context =
  List.assoc_opt !(context.current_namespace) !(context.namespaces) |> Option.value ~default:[]

let current_requires context =
  List.assoc_opt !(context.current_namespace) !(context.requires) |> Option.value ~default:[]

let set_current_globals context globals =
  let namespace = !(context.current_namespace) in
  context.namespaces := (namespace, globals) :: List.remove_assoc namespace !(context.namespaces)

let set_current_requires context requires =
  let namespace = !(context.current_namespace) in
  context.requires := (namespace, requires) :: List.remove_assoc namespace !(context.requires)

let parse_requires = function
  | SList (_, _, requires) ->
      List.map
        (function
          | SList (_, _, [ SAtom (_, namespace); SAtom (_, alias) ]) when is_string namespace && is_string alias ->
              (string_value alias, string_value namespace)
          | _ -> raise (Eval_error "compiler/ns requires must be namespace/alias pairs"))
        requires
  | _ -> raise (Eval_error "compiler/ns requires must be a list")

let set_namespace context namespace requires =
  match namespace with
  | SAtom (_, name) when is_string name ->
      context.current_namespace := string_value name;
      set_current_requires context (parse_requires requires);
      Symbol "nil"
  | _ -> raise (Eval_error "compiler/ns expects string namespace")

let find_in_namespace context namespace name =
  Option.bind (List.assoc_opt namespace !(context.namespaces)) (List.assoc_opt name)

let find_qualified context name =
  match String.split_on_char '/' name with
  | [ qualifier; member ] when qualifier <> "" && member <> "" ->
      let namespace = List.assoc_opt qualifier (current_requires context) |> Option.value ~default:qualifier in
      find_in_namespace context namespace member
  | _ -> None

let find context name =
  if is_string name then Symbol (string_value name)
  else
    match List.assoc_opt name context.env with
    | Some value -> value
    | None -> (
        match List.assoc_opt name (current_globals context) with
        | Some value -> value
        | None -> (
            match List.assoc_opt name Eval_stdlib.env with
            | Some value -> value
            | None -> (
                match find_qualified context name with
                | Some value -> value
                | None when is_literal name -> Symbol name
                | None -> raise (Eval_error ("symbol not found: " ^ name)))))

let define context name value =
  set_current_globals context ((name, value) :: List.remove_assoc name (current_globals context));
  value

let sexpr_location = function
  | SAtom (meta, _) | SList (meta, _, _) -> Printf.sprintf "line %d, column %d" meta.loc.line meta.loc.column

let rec sexpr_text = function
  | SAtom (_, name) -> name
  | SList (_, bracket, items) ->
      let opening, closing = match bracket with Paren -> ("(", ")") | Bracket -> ("[", "]") | Brace -> ("{", "}") in
      opening ^ String.concat " " (List.map sexpr_text items) ^ closing

let value_text = function
  | Symbol name -> Printf.sprintf "symbol %S" name
  | List _ -> "list"
  | HashMap _ -> "hash-map"
  | Closure _ -> "function"

let rec eval ?(context = create_context ()) = function
  | SAtom (_, name) -> find context name
  | SList (_, Paren, [ SAtom (_, "def"); SAtom (_, name); value ]) -> define context name (eval ~context value)
  | SList (_, Paren, [ SAtom (_, "compiler/ns"); namespace; requires; _imports ]) ->
      set_namespace context namespace requires
  | SList (_, Paren, SAtom (_, "compiler/ns") :: _) ->
      raise (Eval_error "compiler/ns expects namespace, requires, imports")
  | SList (_, Paren, [ SAtom (_, "deps"); deps ]) -> load_deps context (eval ~context deps)
  | SList (_, Paren, [ SAtom (_, "quote"); value ]) -> quote value
  | SList (_, Paren, [ SAtom (_, "cast"); _; value ]) -> eval ~context value
  | SList (_, Paren, [ SAtom (_, "if"); condition; then_; else_ ]) ->
      if truthy (eval ~context condition) then eval ~context then_ else eval ~context else_
  | SList (_, Paren, [ SAtom (_, "if"); condition; then_ ]) ->
      if truthy (eval ~context condition) then eval ~context then_ else Symbol "nil"
  | SList (_, Paren, SAtom (_, "if") :: _) ->
      raise (Eval_error "if expects condition, then branch, and optional else branch")
  | SList (_, Paren, SAtom (_, "fn*") :: SList (_, _, params) :: body) ->
      Closure (User (params, body, context.env, !(context.current_namespace)))
  | SList (_, Paren, SAtom (_, "let*") :: SList (_, _, bindings) :: body) ->
      eval_body (with_env context (bind_all context bindings)) body
  | SList (_, Paren, SAtom (_, "do") :: body) -> eval_body context body
  | SList (_, _, items) -> (
      match (items, List.map (eval ~context) items) with
      | _, Closure fn :: args -> apply_closure context fn args
      | _, [] -> List []
      | first :: _, value :: _ ->
          raise
            (Eval_error
               (Printf.sprintf "first list item is not a function at %s: %s evaluated to %s" (sexpr_location first)
                  (sexpr_text first) (value_text value)))
      | _ -> assert false)

and truthy = function Symbol "false" | Symbol "nil" -> false | _ -> true

and quote = function
  | SAtom (_, name) -> Symbol (if is_string name then string_value name else name)
  | SList (_, _, items) -> List (List.map quote items)

and load_deps context = function
  | HashMap deps ->
      List.iter (load_dep context) deps;
      Symbol "nil"
  | _ -> raise (Eval_error "deps expects a hash-map")

and load_dep context = function
  | Symbol package, Symbol version -> List.iter (load_package_file context) (read_package_files package version)
  | _ -> raise (Eval_error "deps expects string package/version pairs")

and load_package_file context file =
  let current_namespace = !(context.current_namespace) in
  match Frontend.parse_and_desugar file.package_source with
  | Ok sexprs ->
      Fun.protect
        ~finally:(fun () -> context.current_namespace := current_namespace)
        (fun () -> ignore (eval_all ~context sexprs))
  | Error message -> raise (Eval_error message)

and bind_all context = function
  | [] -> context.env
  | pattern :: value :: rest ->
      bind_all
        (with_env context
           (bind_pattern ~exact_list_length:false ~allow_hash_map:true context.env pattern (eval ~context value)))
        rest
  | node ->
      raise
        (Eval_error
           (__LOC__ ^ ": let* bindings must be name/value pairs ["
           ^ (node |> List.map show_sexpr |> String.concat "")
           ^ "]"))

and eval_body context = function
  | [] -> List []
  | [ expr ] -> eval ~context expr
  | expr :: rest ->
      ignore (eval ~context expr);
      eval_body context rest

and apply_value context fn args =
  match fn with Closure fn -> apply_closure context fn args | _ -> raise (Eval_error "value is not a function")

and apply_closure context fn args =
  match fn with
  | Native fn -> fn (apply_value context) args
  | User (params, body, closure_env, closure_namespace) -> apply context closure_env closure_namespace params body args

and apply context closure_env closure_namespace params body args =
  if List.length params <> List.length args then raise (Eval_error "wrong number of arguments")
  else
    let current_namespace = !(context.current_namespace) in
    Fun.protect
      ~finally:(fun () -> context.current_namespace := current_namespace)
      (fun () ->
        context.current_namespace := closure_namespace;
        eval_body (with_env context (bind_params closure_env params args)) body)

and bind_params env params args =
  if List.length params <> List.length args then raise (Eval_error "wrong number of destructured values")
  else List.fold_left2 (bind_pattern ~exact_list_length:false ~allow_hash_map:true) env params args

and bind_pattern ~exact_list_length ~allow_hash_map env pattern value =
  match (pattern, value) with
  | SAtom (_, name), value -> (name, value) :: env
  | SList (_, _, SAtom (_, "list") :: patterns), List values ->
      if exact_list_length && List.length patterns <> List.length values then
        raise (Eval_error "wrong number of destructured values")
      else bind_list_pattern ~exact_list_length ~allow_hash_map env patterns values
  | SList (_, _, SAtom (_, "list") :: _), _ -> raise (Eval_error "destructuring pattern expects a list")
  | SList (_, _, SAtom (_, "hash-map") :: patterns), HashMap values when allow_hash_map ->
      bind_hash_map_pattern ~exact_list_length ~allow_hash_map env patterns values
  | SList (_, _, SAtom (_, "hash-map") :: _), _ when allow_hash_map ->
      raise (Eval_error "destructuring pattern expects a hash-map")
  | _ -> raise (Eval_error "function parameter must be a symbol or destructuring list")

and bind_list_pattern ~exact_list_length ~allow_hash_map env patterns values =
  match patterns with
  | [] -> env
  | pattern :: rest ->
      let value, values = match values with value :: rest -> (value, rest) | [] -> (Symbol "nil", []) in
      bind_list_pattern ~exact_list_length ~allow_hash_map
        (bind_pattern ~exact_list_length ~allow_hash_map env pattern value)
        rest values

and bind_hash_map_pattern ~exact_list_length ~allow_hash_map env patterns values =
  match patterns with
  | [] -> env
  | SAtom (_, key) :: pattern :: rest ->
      let key = Symbol (if is_string key then string_value key else key) in
      let value = Option.value (List.assoc_opt key values) ~default:(Symbol "nil") in
      bind_hash_map_pattern ~exact_list_length ~allow_hash_map
        (bind_pattern ~exact_list_length ~allow_hash_map env pattern value)
        rest values
  | _ -> raise (Eval_error "hash-map destructuring pattern must contain key/pattern pairs")

and eval_all ?(context = create_context ()) sexprs = sexprs |> List.map (eval ~context)
