open Frontend
open Eval_types

let rec to_sexpr meta = function
  | Syntax source -> source
  | List items -> SList (meta, Paren, List.map (to_sexpr meta) items)
  | Symbol name -> SAtom (meta, name)
  | Nil -> SAtom (meta, "nil")
  | Bool value -> SAtom (meta, string_of_bool value)
  | Int value -> SAtom (meta, string_of_int value)
  | Float value ->
      let text = Eval_stdlib.float_text value in
      (* Printing may omit the decimal point; code must retain the float category. *)
      SAtom (meta, if Option.is_some (int_of_string_opt text) then text ^ ".0" else text)
  | String value -> SAtom (meta, "\"" ^ value ^ "\"")
  | value -> raise (Eval_error ("macro result is not syntax: " ^ Eval.value_text value))

(* Definitions are (form name, entry point, embedded source), in bootstrap order. *)
let create_from definitions () =
  let context = Eval.create_context () in
  let native name fn = ignore (Eval.define context name (Closure (Native fn))) in
  native "syntax-vector?" (fun _ -> function
    | [ Syntax (SList (_, Bracket, _)) ] -> Bool true
    | [ _ ] -> Bool false
    | _ -> raise (Eval_error "syntax-vector? expects one value"));
  native "syntax-children" (fun _ -> function
    | [ Syntax (SList (_, _, items)) ] -> List (List.map (fun item -> Syntax item) items)
    | _ -> raise (Eval_error "syntax-children expects a syntax list"));
  native "syntax-simple-symbol?" (fun _ -> function
    | [ Syntax (SAtom (_, name) as source) ] ->
        Bool (Frontend.is_binding_symbol source && not (String.contains name '/'))
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
    match
      Frontend.parse_and_desugar
        (String.concat "\n" (List.map (fun (_, _, source) -> source) definitions))
    with
    | Ok forms -> forms
    | Error message -> raise (Eval_error message)
  in
  ignore (Eval.eval_all ~context forms);
  let expanders = List.map (fun (name, entry, _) -> (name, Eval.find context entry)) definitions in
  let apply meta expand clauses =
    (* Bind at each call, but allocate a fresh name only when the macro asks. *)
    ignore (Eval.define context "gensym" (Closure (Native (fun _ _ -> Symbol (Gensym.gensym_string meta)))));
    let arguments = List (List.map (fun source -> Syntax source) clauses) in
    Some (to_sexpr meta (Eval.apply_value context expand [ arguments ]))
  in
  [
    (function
    | SList (meta, Paren, SAtom (_, name) :: clauses) ->
        Option.bind (List.assoc_opt name expanders) (fun expand -> apply meta expand clauses)
    | _ -> None);
  ]

let create () = create_from Builtin_macro_source.definitions ()
