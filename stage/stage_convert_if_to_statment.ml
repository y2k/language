open Core__.Common

let me = meta_empty
let atom s = SAtom (me, s)
let slist xs = SList (me, xs)

let rec convert_if (node : sexp) : sexp =
  match node with
  | SAtom _ -> node
  | SList (_, SAtom (_, "quote*") :: _) -> node
  | SList (_, [ (SAtom (_, "if*") as if_); cond; then_; else_ ]) ->
      let cond = convert_if cond in
      let then_ = convert_if then_ in
      let else_ = convert_if else_ in
      let var = NameGenerator.get_new_var () in
      let set_ value = slist [ atom "set!"; atom var; value ] in
      slist
        [
          atom "do*";
          slist [ atom "let*"; atom var ];
          slist [ if_; cond; set_ then_; set_ else_ ];
          atom var;
        ]
  | SList (m, (SAtom (_, "fn*") as fn_) :: args :: body) ->
      SList (m, fn_ :: args :: List.map convert_if body)
  | SList (m, args) -> SList (m, List.map convert_if args)

let rec invoke_up_do (node : sexp) : sexp =
  match node with
  | SAtom _ -> node
  | SList (_, SAtom (_, "quote*") :: _) -> node
  | SList (_, [ SAtom (_, "let*"); _ ]) -> node
  | SList (m, (SAtom (_, "do*") as do_) :: body) ->
      let body =
        body |> List.map invoke_up_do
        |> List.concat_map (function
          | SList (_, SAtom (_, "do*") :: xs) -> xs
          | n -> [ n ])
      in
      let body_count = List.length body in
      let body =
        body
        |> List.filteri (fun i x ->
            match (i = body_count - 1, x) with
            | true, _ -> true
            | false, SAtom _ -> false
            | false, _ -> true)
      in
      SList (m, do_ :: body)
  | SList (m, (SAtom (_, "fn*") as fn_) :: args :: body) ->
      SList (m, fn_ :: args :: List.map invoke_up_do body)
  | SList
      ( _,
        [
          (SAtom (_, "if*") as if_);
          (SList (_, SAtom (_, "do*") :: _) as cond);
          then_;
          else_;
        ] ) ->
      let cond_body =
        match invoke_up_do cond with
        | SList (_, SAtom (_, "do*") :: body) -> body
        | n -> [ n ]
      in
      let then_ = invoke_up_do then_ in
      let else_ = invoke_up_do else_ in
      slist
        ([ atom "do*" ]
        @ butlast cond_body
        @ [ slist [ if_; last cond_body; then_; else_ ] ])
  | SList (m, [ (SAtom (_, "if*") as if_); cond; then_; else_ ]) ->
      SList
        (m, [ if_; invoke_up_do cond; invoke_up_do then_; invoke_up_do else_ ])
  | SList
      (m, (SAtom (_, ("___raw_template" | "__compiler_emit")) as head) :: body)
    ->
      SList (m, head :: List.map invoke_up_do body)
  | SList (m, args) -> (
      let args = List.map invoke_up_do args in
      let lets =
        args
        |> List.concat_map (function
          | SList (_, SAtom (_, "do*") :: body) -> butlast body
          | _ -> [])
      in
      let args =
        args
        |> List.map (function
          | SList (_, SAtom (_, "do*") :: body) -> last body
          | n -> n)
      in
      match lets with
      | [] -> SList (m, args)
      | _ -> SList (m, (atom "do*" :: lets) @ [ slist args ]))

let rec invoke_up_do_ level (node : sexp) : sexp =
  if level = 0 then failwith "Recurse too deep";
  let result = invoke_up_do node in
  if result = node then node else invoke_up_do_ (level - 1) result

let invoke (node : sexp) : sexp = convert_if node |> invoke_up_do_ 10
