open Frontend

let meta_of = function SAtom (meta, _) | SList (meta, _, _) -> meta
let atom meta value = SAtom (meta, value)
let form meta items = SList (meta, Paren, items)
let do_form meta body = form meta (atom meta "do" :: body)
let let_star meta name value = form meta [ atom meta "let*"; name; value ]
let statement_block meta body = form meta (atom meta "let*" :: form meta [] :: body)

let rec unwrap_do sexprs =
  sexprs |> List.concat_map (function SList (_, _, SAtom (_, "do") :: body) -> unwrap_do body | x -> [ x ])

let rec lower sexprs = List.map lower_top_level sexprs

and lower_top_level = function
  | SList (meta, _, [ SAtom (def_meta, "def"); name; value ]) ->
      SList (meta, Paren, [ SAtom (def_meta, "def"); name; lower_expr value ])
  | sexpr -> lower_expr sexpr

and lower_expr expr =
  let bindings, result = lower_value expr in
  match bindings with [] -> result | bindings -> do_form (meta_of expr) (bindings @ [ result ])

and lower_value = function
  | SAtom _ as atom -> ([], atom)
  | SList (_, _, [ SAtom (_, "quote"); _ ]) as quote -> ([], quote)
  | SList (meta, _, SAtom (_, "do") :: body) -> lower_body_value meta body
  | SList (meta, _, SAtom (_, "let*") :: SList (_, _, bindings) :: body) -> lower_let_star_value meta bindings body
  | SList (meta, _, [ SAtom (_, "if"); condition; then_ ]) -> lower_if meta condition then_ (atom meta "nil")
  | SList (meta, _, [ SAtom (_, "if"); condition; then_; else_ ]) -> lower_if meta condition then_ else_
  | SList (meta, _, SAtom (fn_meta, "fn*") :: SList (params_meta, bracket, params) :: body) ->
      let params, bindings =
        List.fold_right
          (fun pattern (params, bindings) ->
            match pattern with
            | SAtom _ -> (pattern :: params, bindings)
            | _ ->
                let parameter = Gensym.gensym (meta_of pattern) in
                (parameter :: params, pattern :: parameter :: bindings))
          params ([], [])
      in
      let body =
        match bindings with
        | [] -> body
        | _ -> [ form meta (atom meta "let*" :: SList (params_meta, bracket, bindings) :: body) ]
      in
      ( [],
        SList
          ( meta,
            Paren,
            SAtom (fn_meta, "fn*") :: SList (params_meta, bracket, params) :: (List.map lower_expr body |> unwrap_do) )
      )
  | SList (meta, bracket, items) ->
      let bindings, items = lower_values items in
      (bindings, SList (meta, bracket, items))

and lower_values = function
  | [] -> ([], [])
  | item :: rest ->
      let item_bindings, item = lower_value item in
      let rest_bindings, rest = lower_values rest in
      (item_bindings @ rest_bindings, item :: rest)

and lower_bindings meta = function
  | pattern :: value :: rest ->
      let value_bindings, value = lower_value value in
      value_bindings @ lower_binding meta pattern value @ lower_bindings meta rest
  | bindings -> bindings

and lower_binding meta pattern value =
  match pattern with
  | SAtom _ -> [ let_star meta pattern value ]
  | SList (pattern_meta, _, SAtom (_, "list") :: patterns) ->
      let temporary = Gensym.gensym pattern_meta in
      let items =
        List.mapi
          (fun index pattern ->
            lower_binding meta pattern
              (form pattern_meta [ atom pattern_meta "get"; temporary; atom pattern_meta (string_of_int index) ]))
          patterns
      in
      let_star meta temporary value :: List.concat items
  | SList (pattern_meta, _, SAtom (_, "hash-map") :: patterns) ->
      let temporary = Gensym.gensym pattern_meta in
      let rec lower_patterns = function
        | [] -> []
        | (SAtom _ as key) :: pattern :: rest ->
            lower_binding meta pattern (form pattern_meta [ atom pattern_meta "get"; temporary; key ])
            @ lower_patterns rest
        | _ -> failwith "hash-map destructuring pattern must contain atom key/pattern pairs"
      in
      let_star meta temporary value :: lower_patterns patterns
  | _ -> failwith "let* binding pattern must be a symbol, list, or hash-map"

and lower_let_star_value meta bindings body =
  let body_bindings, body_value = lower_body_value meta body in
  (lower_bindings meta bindings @ body_bindings, body_value)

and lower_body_value meta = function
  | [] -> ([], atom meta "nil")
  | [ expr ] -> lower_value expr
  | expr :: rest ->
      let rest_bindings, rest_value = lower_body_value meta rest in
      (lower_discard expr @ rest_bindings, rest_value)

and lower_discard expr =
  match expr with
  | SAtom _ | SList (_, _, [ SAtom (_, "quote"); _ ]) | SList (_, _, SAtom (_, "fn*") :: _) ->
      let bindings, value = lower_value expr in
      let discard = Gensym.gensym (meta_of expr) in
      bindings @ [ let_star (meta_of expr) discard value ]
  | SList (_, _, SAtom (_, "do") :: body) -> lower_body_discard body
  | SList (meta, _, SAtom (_, "let*") :: SList (_, _, bindings) :: body) -> lower_let_star_discard meta bindings body
  | SList (meta, _, [ SAtom (_, "if"); condition; then_ ]) -> lower_if_discard meta condition then_ (atom meta "nil")
  | SList (meta, _, [ SAtom (_, "if"); condition; then_; else_ ]) -> lower_if_discard meta condition then_ else_
  | SList (meta, bracket, items) ->
      let bindings, items = lower_values items in
      bindings @ [ SList (meta, bracket, items) ]

and lower_body_discard = function [] -> [] | expr :: rest -> lower_discard expr @ lower_body_discard rest
and lower_let_star_discard meta bindings body = lower_bindings meta bindings @ lower_body_discard body

and lower_if meta condition then_ else_ =
  let condition_bindings, condition = lower_value condition in
  let value = Gensym.gensym meta in
  let assign_branch expr =
    let statements =
      match lower_expr expr with SList (_, _, SAtom (_, "do") :: body) -> unwrap_do body | expr -> [ expr ]
    in
    match List.rev statements with
    | [] -> form meta [ atom meta "set!"; value; atom meta "nil" ]
    | result :: rest -> statement_block meta (List.rev rest @ [ form meta [ atom meta "set!"; value; result ] ])
  in
  let if_value =
    do_form meta
      [
        let_star meta value (atom meta "nil");
        form meta [ atom meta "if"; condition; assign_branch then_; assign_branch else_ ];
        value;
      ]
  in
  (condition_bindings, if_value)

and lower_if_discard meta condition then_ else_ =
  let condition_bindings, condition = lower_value condition in
  let discard_branch expr = statement_block meta (lower_discard expr) in
  condition_bindings @ [ form meta [ atom meta "if"; condition; discard_branch then_; discard_branch else_ ] ]
