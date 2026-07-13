open Ast

let atom meta value = SAtom (meta, value)
let string_atom meta value = atom meta ("\"" ^ value ^ "\"")

let rec and_expr meta = function
  | [] -> atom meta "true"
  | [ value ] -> value
  | value :: rest ->
      let name = Gensym.gensym meta in
      SList
        ( meta,
          Paren,
          [
            atom meta "let*";
            SList (meta, Paren, [ name; value ]);
            SList (meta, Paren, [ atom meta "if"; name; and_expr meta rest; name ]);
          ] )

let rec or_expr meta = function
  | [] -> atom meta "nil"
  | [ value ] -> value
  | value :: rest ->
      let name = Gensym.gensym meta in
      SList
        ( meta,
          Paren,
          [
            atom meta "let*";
            SList (meta, Paren, [ name; value ]);
            SList (meta, Paren, [ atom meta "if"; name; name; or_expr meta rest ]);
          ] )

let and_macro = function SList (meta, Paren, SAtom (_, "and") :: items) -> Some (and_expr meta items) | _ -> None
let or_macro = function SList (meta, Paren, SAtom (_, "or") :: items) -> Some (or_expr meta items) | _ -> None

let keyword_macro = function
  | SAtom (meta, name) when String.length name > 1 && String.starts_with ~prefix:":" name ->
      Some (atom meta ("\"" ^ String.sub name 1 (String.length name - 1) ^ "\""))
  | _ -> None

let list_macro = function
  | SList (meta, Bracket, items) -> Some (SList (meta, Paren, atom meta "list" :: items))
  | _ -> None

let hash_map_macro = function
  | SList (meta, Brace, items) -> Some (SList (meta, Paren, atom meta "hash-map" :: items))
  | _ -> None

let let_macro = function
  | SList (meta, bracket, SAtom (_, "let") :: SList (bm, _, bindings) :: body) ->
      Some (SList (meta, bracket, atom meta "let*" :: SList (bm, Paren, bindings) :: body))
  | SList (meta, bracket, SAtom (_, "let") :: items) -> Some (SList (meta, bracket, atom meta "let*" :: items))
  | _ -> None

let fn_macro = function
  | SList (meta, bracket, SAtom (_, "fn") :: SList (params_meta, _, params) :: body) ->
      let params, bindings =
        List.fold_right
          (fun param (params, bindings) ->
            match param with
            | SAtom (({ type_annotation = Some type_name; _ } as param_meta), name) ->
                let argument = Gensym.gensym param_meta in
                let cast = SList (param_meta, Paren, [ atom param_meta "cast"; atom param_meta type_name; argument ]) in
                (argument :: params, atom param_meta name :: cast :: bindings)
            | _ -> (param :: params, bindings))
          params ([], [])
      in
      let body =
        match bindings with
        | [] -> body
        | _ -> [ SList (meta, Paren, atom meta "let*" :: SList (params_meta, Paren, bindings) :: body) ]
      in
      Some (SList (meta, bracket, atom meta "fn*" :: SList (params_meta, Paren, params) :: body))
  | _ -> None

let defn_macro = function
  | SList (meta, _, SAtom (_, macro_name) :: SAtom (_, name) :: params :: body)
    when macro_name = "defn" || macro_name = "defn-" ->
      Some
        (SList (meta, Paren, [ atom meta "def"; atom meta name; SList (meta, Paren, atom meta "fn" :: params :: body) ]))
  | _ -> None

let method_call_macro = function
  | SList (meta, bracket, SAtom (method_meta, name) :: instance :: args)
    when String.length name > 1 && String.starts_with ~prefix:"." name ->
      let method_name = String.sub name 1 (String.length name - 1) in
      Some (SList (meta, bracket, atom meta "." :: instance :: atom method_meta method_name :: args))
  | _ -> None

let constructor_macro = function
  | SList (meta, bracket, SAtom (class_meta, name) :: args)
    when String.length name > 1 && String.ends_with ~suffix:"." name ->
      let class_name = String.sub name 0 (String.length name - 1) in
      Some (SList (meta, bracket, atom meta "new" :: atom class_meta class_name :: args))
  | _ -> None

let gen_class_error message = failwith ("gen-class: " ^ message)

let gen_class_macro = function
  | SList (meta, Paren, SAtom (_, "gen-class") :: items) ->
      let rec parse_items class_name extends methods = function
        | [] -> (class_name, extends, methods)
        | SAtom (_, ":name") :: SAtom (_, name) :: rest -> parse_items (Some name) extends methods rest
        | SAtom (_, ":extends") :: SAtom (_, name) :: rest -> parse_items class_name (Some name) methods rest
        | SAtom (_, ":methods") :: SList (_, Bracket, methods) :: rest ->
            parse_items class_name extends (Some methods) rest
        | _ -> gen_class_error "expected :name NAME, :extends NAME and :methods [[name [args] return]]"
      in
      let parse_method = function
        | SList (_, Bracket, [ SAtom (name_meta, name); SList (_, Bracket, args); SAtom (_, return_type) ]) ->
            let parse_arg = function
              | SAtom (_, name) -> string_atom meta name
              | _ -> gen_class_error "method args must be symbols"
            in
            SList
              ( meta,
                Paren,
                [
                  string_atom name_meta name; SList (meta, Paren, List.map parse_arg args); string_atom meta return_type;
                ] )
        | _ -> gen_class_error "methods must be [name [args] return]"
      in
      let name, extends, methods = parse_items None None None items in
      let name = match name with Some name -> name | None -> gen_class_error "missing :name" in
      let extends = match extends with Some extends -> extends | None -> gen_class_error "missing :extends" in
      let methods = match methods with Some methods -> methods | None -> gen_class_error "missing :methods" in
      Some
        (SList
           ( meta,
             Paren,
             [
               atom meta "compiler/gen-class";
               string_atom meta name;
               string_atom meta extends;
               SList (meta, Paren, List.map parse_method methods);
             ] ))
  | _ -> None

let thread_macro name insert = function
  | SList (_, _, SAtom (_, macro_name) :: value :: steps) when macro_name = name ->
      Some
        (List.fold_left
           (fun value -> function
             | SList (meta, bracket, fn :: args) -> SList (meta, bracket, insert fn args value)
             | SAtom (meta, _) as fn -> SList (meta, Paren, insert fn [] value)
             | step -> step)
           value steps)
  | _ -> None

let thread_first_macro = thread_macro "->" (fun fn args value -> fn :: value :: args)
let thread_last_macro = thread_macro "->>" (fun fn args value -> (fn :: args) @ [ value ])

let builtin_macros =
  [
    and_macro;
    or_macro;
    thread_first_macro;
    thread_last_macro;
    defn_macro;
    constructor_macro;
    method_call_macro;
    gen_class_macro;
    fn_macro;
    Macro_ns.apply;
    keyword_macro;
    hash_map_macro;
    let_macro;
    list_macro;
  ]
