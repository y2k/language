open Frontend

let meta_of = function SAtom (meta, _) | SList (meta, _, _) -> meta
let atom meta value = SAtom (meta, value)
let form meta items = SList (meta, Paren, items)
let do_form meta body = form meta (atom meta "do" :: body)
let let_star meta name value = form meta [ atom meta "let*"; name; value ]
let statement_block meta body = form meta (atom meta "let*" :: form meta [] :: body)
let result_name meta = Gensym.gensym { meta with type_annotation = None }

let rec lower_binding meta pattern value =
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

let resolve env = function
  | SAtom (meta, name) as value -> (
      match List.assoc_opt name env with Some (SAtom (_, name)) -> atom meta name | _ -> value)
  | value -> value

let bind ?(fresh = false) ~java env = function
  | SAtom (meta, name) as symbol ->
      let symbol = if fresh || (java && List.mem_assoc name env) then Gensym.gensym meta else symbol in
      (symbol, (name, symbol) :: env)
  | _ -> failwith "expected normalized binding"

let save value =
  let meta = meta_of value in
  let saved = match value with SList (_, _, SAtom (_, "fn*") :: _) -> result_name meta | _ -> Gensym.gensym meta in
  (let_star meta saved value, saved)

let js_member_separator name =
  let index =
    max
      (Option.value ~default:(-1) (String.rindex_opt name '/'))
      (Option.value ~default:(-1) (String.rindex_opt name '.'))
  in
  if index > 0 && index < String.length name - 1 then Some index else None

let rec lower_value ~java env expr =
  match expr with
  | SAtom _ -> ([], resolve env expr)
  | SList (_, _, [ SAtom (_, "quote"); _ ]) -> ([], expr)
  | SList (_, _, SAtom (_, ("compiler/ns" | "compiler/gen-class" | "raw-code")) :: _) -> ([], expr)
  | SList (meta, _, SAtom (_, "try") :: items) ->
      let result = result_name meta in
      let protected = lower_try ~java env meta (Some result) items in
      ([ let_star meta result (atom meta "nil"); protected ], result)
  | SList (meta, _, SAtom (_, "throw") :: args) ->
      let statements, value = lower_value ~java env (Exception_forms.throw_operand args) in
      (statements, form meta [ atom meta "throw"; value ])
  | SList (meta, _, SAtom (_, "do") :: body) -> lower_body_value ~java env meta body
  | SList (meta, _, SAtom (_, "let*") :: SList (_, _, bindings) :: body) ->
      let result = result_name meta in
      let statements =
        lower_bindings ~java env meta bindings (fun env ->
            let statements, value = lower_body_value ~java env meta body in
            statements @ [ form meta [ atom meta "set!"; result; value ] ])
      in
      ([ let_star meta result (atom meta "nil"); statement_block meta statements ], result)
  | SList (meta, _, [ SAtom (_, "if"); condition; then_ ]) -> lower_if ~java env meta condition then_ (atom meta "nil")
  | SList (meta, _, [ SAtom (_, "if"); condition; then_; else_ ]) -> lower_if ~java env meta condition then_ else_
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
      let env, params =
        List.fold_left_map
          (fun env param ->
            let param, env = bind ~java env param in
            (env, param))
          env params
      in
      let body =
        if bindings = [] then body
        else [ form meta (atom meta "let*" :: SList (params_meta, bracket, bindings) :: body) ]
      in
      let body =
        if java && Option.fold ~none:false ~some:(String.starts_with ~prefix:"void:") meta.type_annotation then
          List.concat_map (lower_discard ~java env) body
        else lower_returning_body ~java env meta body
      in
      ([], SList (meta, Paren, SAtom (fn_meta, "fn*") :: SList (params_meta, bracket, params) :: body))
  | SList (meta, bracket, items) ->
      let statements, items = lower_operands ~java env meta items in
      (statements, SList (meta, bracket, items))

and lower_try ~java env meta result items =
  let parsed = Exception_forms.parse items in
  let body env forms =
    let statements =
      match result with
      | None -> List.concat_map (lower_discard ~java env) forms
      | Some result ->
          let statements, value = lower_body_value ~java env meta forms in
          statements @ [ form meta [ atom meta "set!"; result; value ] ]
    in
    statement_block meta statements
  in
  let protected = body env parsed.body in
  let handler =
    match parsed.handler with
    | None -> []
    | Some (binding, forms) ->
        let binding, env = bind ~fresh:true ~java env binding in
        [ form meta [ atom meta "catch"; binding; body env forms ] ]
  in
  let cleanup =
    match parsed.cleanup with
    | None -> []
    | Some forms ->
        [ form meta [ atom meta "finally"; statement_block meta (List.concat_map (lower_discard ~java env) forms) ] ]
  in
  form meta ((atom meta "try" :: protected :: handler) @ cleanup)

and lower_operands ~java env meta = function
  | (SAtom (_, "new") as op) :: class_name :: args when not java ->
      let statements, values = lower_values ~java env (class_name :: args) in
      (statements, op :: values)
  | (SAtom (_, ("new" | "cast" | "instance?")) as op) :: name :: args ->
      let statements, args = lower_values ~java env args in
      (statements, op :: name :: args)
  | (SAtom (_, "set!") as op) :: name :: args ->
      let statements, args = lower_values ~java env args in
      (statements, op :: resolve env name :: args)
  | (SAtom (_, ".") as op) :: receiver :: member :: args ->
      if java then
        let statements, values = lower_values ~java env (receiver :: args) in
        (statements, op :: List.hd values :: member :: List.tl values)
      else lower_js_method env meta receiver member args (fun receiver args -> op :: receiver :: member :: args)
  | (SAtom (_, "export-default") as op) :: items ->
      let rec split = function
        | key :: value :: rest ->
            let keys, values = split rest in
            (key :: keys, value :: values)
        | rest -> (rest, [])
      in
      let rec combine keys values =
        match (keys, values) with
        | key :: keys, value :: values -> key :: value :: combine keys values
        | keys, _ -> keys
      in
      let keys, values = split items in
      let statements, values = lower_values ~java env values in
      (statements, op :: combine keys values)
  | (SAtom (_, name) as op) :: args when java && not (List.mem_assoc name env) ->
      let statements, args = lower_values ~java env args in
      (statements, op :: args)
  | (SAtom (op_meta, name) as op) :: args
    when (not java) && (not (List.mem_assoc name env)) && Option.is_some (js_member_separator name) ->
      let index = Option.get (js_member_separator name) in
      let receiver = atom op_meta (String.sub name 0 index) in
      let member = atom op_meta (String.sub name (index + 1) (String.length name - index - 1)) in
      lower_js_method env meta receiver member args (fun _ args -> op :: args)
  | items -> lower_values ~java env items

and lower_js_method env meta receiver member args rebuild =
  let receiver_statements, receiver = lower_value ~java:false env receiver in
  let arg_statements, args = lower_values ~java:false env args in
  if arg_statements = [] then (receiver_statements, rebuild receiver args)
  else
    let saved_receiver = Gensym.gensym (meta_of receiver) in
    let saved_method = Gensym.gensym meta in
    ( receiver_statements
      @ [
          let_star meta saved_receiver receiver;
          let_star meta saved_method (form meta [ atom meta "compiler/js-member"; saved_receiver; member ]);
        ]
      @ arg_statements,
      atom meta "compiler/js-call" :: saved_method :: saved_receiver :: args )

and lower_values ~java env = function
  | [] -> ([], [])
  | item :: rest -> (
      let item_statements, item = lower_value ~java env item in
      let rest_statements, rest = lower_values ~java env rest in
      match (item, rest_statements) with
      (* Keep null literal typing for Java overload resolution; nil has no effects to preserve. *)
      | SAtom (_, "nil"), _ | _, [] -> (item_statements @ rest_statements, item :: rest)
      | _ ->
          let binding, saved = save item in
          (item_statements @ [ binding ] @ rest_statements, saved :: rest))

and lower_bindings ~java env meta bindings body =
  match bindings with
  | (SAtom _ as pattern) :: value :: rest ->
      let statements, value = lower_value ~java env value in
      let name, env = bind ~java env pattern in
      (* Evaluate before introducing the name, including Java self-reference and JS TDZ. *)
      let initial = result_name (meta_of value) in
      statements
      @ [
          let_star meta initial value;
          statement_block meta (let_star meta name initial :: lower_bindings ~java env meta rest body);
        ]
  | pattern :: value :: rest ->
      let temporary = Gensym.gensym (meta_of pattern) in
      let bindings =
        lower_binding meta pattern temporary
        |> List.concat_map (function
          | SList (_, _, [ SAtom (_, "let*"); name; value ]) -> [ name; value ]
          | _ -> assert false)
      in
      lower_bindings ~java env meta ((temporary :: value :: bindings) @ rest) body
  | [] -> body env
  | _ -> failwith "let* requires binding pairs"

and lower_body_value ~java env meta = function
  | [] -> ([], atom meta "nil")
  | [ expr ] -> lower_value ~java env expr
  | expr :: rest ->
      let statements = lower_discard ~java env expr in
      let rest, value = lower_body_value ~java env meta rest in
      (statements @ rest, value)

and lower_returning_body ~java env meta = function
  | [ SList (meta, _, SAtom (_, "let*") :: SList (_, _, bindings) :: body) ] ->
      [
        statement_block meta
          (lower_bindings ~java env meta bindings (fun env -> lower_returning_body ~java env meta body));
      ]
  | [ SList (meta, _, SAtom (_, "do") :: body) ] -> lower_returning_body ~java env meta body
  | expr :: (_ :: _ as rest) -> lower_discard ~java env expr @ lower_returning_body ~java env meta rest
  | body ->
      let statements, value = lower_body_value ~java env meta body in
      statements @ [ value ]

and lower_if ~java env meta condition then_ else_ =
  let condition_statements, condition = lower_value ~java env condition in
  let value = result_name meta in
  let branch expr =
    let statements, result = lower_value ~java env expr in
    statement_block meta (statements @ [ form meta [ atom meta "set!"; value; result ] ])
  in
  let then_ = branch then_ in
  let else_ = branch else_ in
  ( condition_statements
    @ [ let_star meta value (atom meta "nil"); form meta [ atom meta "if"; condition; then_; else_ ] ],
    value )

and lower_discard ~java env expr =
  let meta = meta_of expr in
  match expr with
  | SList (_, _, SAtom (_, "try") :: items) -> [ lower_try ~java env meta None items ]
  | SList (_, _, SAtom (_, "do") :: body) -> List.concat_map (lower_discard ~java env) body
  | SList (_, _, SAtom (_, "let*") :: SList (_, _, bindings) :: body) ->
      [
        statement_block meta
          (lower_bindings ~java env meta bindings (fun env -> List.concat_map (lower_discard ~java env) body));
      ]
  | SList (_, _, [ SAtom (_, "if"); condition; then_ ]) ->
      lower_if_discard ~java env meta condition then_ (atom meta "nil")
  | SList (_, _, [ SAtom (_, "if"); condition; then_; else_ ]) -> lower_if_discard ~java env meta condition then_ else_
  | _ -> (
      let statements, result = lower_value ~java env expr in
      statements
      @
      match result with
      | SAtom _ | SList (_, _, SAtom (_, ("quote" | "fn*" | "instance?")) :: _) ->
          let binding, _ = save result in
          [ binding ]
      | _ -> [ result ])

and lower_if_discard ~java env meta condition then_ else_ =
  let statements, condition = lower_value ~java env condition in
  let then_ = statement_block meta (lower_discard ~java env then_) in
  let else_ = statement_block meta (lower_discard ~java env else_) in
  statements @ [ form meta [ atom meta "if"; condition; then_; else_ ] ]

let lower ?(java = false) sexprs =
  let lower_expr expr =
    let statements, value = lower_value ~java [] expr in
    match statements with [] -> value | _ -> do_form (meta_of expr) (statements @ [ value ])
  in
  List.map
    (function
      | SList (meta, _, [ (SAtom (_, "def") as op); name; value ]) -> form meta [ op; name; lower_expr value ]
      | expr -> lower_expr expr)
    sexprs
