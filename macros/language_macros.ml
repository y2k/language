open Frontend
open Backend_eval.Eval_types

let rec to_sexpr meta = function
  | Syntax source -> source
  | List items -> SList (meta, Paren, List.map (to_sexpr meta) items)
  | Symbol name -> SAtom (meta, name)
  | Nil -> SAtom (meta, "nil")
  | Bool value -> SAtom (meta, string_of_bool value)
  | Int value -> SAtom (meta, string_of_int value)
  | Float value ->
      let text = Backend_eval.Eval_stdlib.float_text value in
      (* Printing may omit the decimal point; code must retain the float category. *)
      SAtom (meta, if Option.is_some (int_of_string_opt text) then text ^ ".0" else text)
  | String value -> SAtom (meta, "\"" ^ value ^ "\"")
  | value -> raise (Eval_error ("macro result is not syntax: " ^ Backend_eval.Eval.value_text value))

(* Evaluates the source with base macros only, so form names inside it stay compile-time functions. *)
let prepare names source =
  let context = Backend_eval.Eval.create_context () in
  let native name fn = ignore (Backend_eval.Eval.define context name (Closure (Native fn))) in
  native "syntax-vector?" (fun _ -> function
    | [ Syntax (SList (_, Bracket, _)) ] -> Bool true
    | [ _ ] -> Bool false
    | _ -> raise (Eval_error "syntax-vector? expects one value"));
  native "syntax-list?" (fun _ -> function
    | [ Syntax (SList (_, Paren, _)) ] -> Bool true
    | [ _ ] -> Bool false
    | _ -> raise (Eval_error "syntax-list? expects one value"));
  native "syntax-children" (fun _ -> function
    | [ Syntax (SList (_, _, items)) ] -> List (List.map (fun item -> Syntax item) items)
    | _ -> raise (Eval_error "syntax-children expects a syntax list"));
  native "syntax-simple-symbol?" (fun _ -> function
    | [ Syntax (SAtom (_, name) as source) ] ->
        Bool (Builtin_macros.is_binding_symbol source && not (String.contains name '/'))
    | [ _ ] -> Bool false
    | _ -> raise (Eval_error "syntax-simple-symbol? expects one value"));
  native "syntax-list-at" (fun _ -> function
    | [ Syntax (SAtom (meta, _) | SList (meta, _, _)); List items ] ->
        let children =
          List.map
            (function Syntax source -> source | _ -> raise (Eval_error "syntax-list-at expects syntax children"))
            items
        in
        Syntax (SList (meta, Paren, children))
    | _ -> raise (Eval_error "syntax-list-at expects source syntax and a list"));
  native "macro-error" (fun _ -> function
    | [ String message ] -> failwith message
    | _ -> raise (Eval_error "macro-error expects a string"));
  let forms =
    match Frontend.parse_and_desugar ~macros:Builtin_macros.builtin_macros source with
    | Ok forms -> forms
    | Error message -> raise (Eval_error message)
  in
  ignore (Backend_eval.Eval.eval_all ~context forms);
  (context, List.map (fun name -> (name, Backend_eval.Eval.find context name)) names)

(* The source is read and prepared on the first call of a named form, separately for each instance. *)
let load names source () =
  let prepared = lazy (prepare names (source ())) in
  let apply meta name clauses =
    let context, expanders = Lazy.force prepared in
    (* Bind at each call, but allocate a fresh name only when the macro asks. *)
    ignore (Backend_eval.Eval.define context "gensym" (Closure (Native (fun _ _ -> Symbol (Gensym.gensym_string meta)))));
    let arguments = List (List.map (fun source -> Syntax source) clauses) in
    Some (to_sexpr meta (Backend_eval.Eval.apply_value context (List.assoc name expanders) [ arguments ]))
  in
  [
    (function
    | SList (meta, Paren, SAtom (_, name) :: clauses) when List.mem name names -> apply meta name clauses
    | _ -> None);
  ]

(* Form names in bootstrap order; the source defines each form under its own name. *)
let create_from names source = load names (fun () -> source)

let prelude_names = [ "cond"; "and"; "or"; "with-open"; "case"; "if-let"; "->"; "->>" ]

let read_prelude () =
  let dir =
    match Sys.getenv_opt "LY2K_PACKAGES_DIR" with
    | Some dir when dir <> "" -> dir
    | _ -> raise (Eval_error "LY2K_PACKAGES_DIR must be set to load builtin macros")
  in
  let path = List.fold_left Filename.concat dir [ "prelude"; "1.0.0"; "macro"; "prelude.clj" ] in
  try In_channel.with_open_text path In_channel.input_all
  with Sys_error message -> raise (Eval_error ("cannot load builtin macros: " ^ message))

let create = load prelude_names read_prelude
