open Lib__.Common

let rec invoke = function
  | SAtom _ as x -> x
  | SList (_, [ SAtom (_, "quote*"); _ ]) as x -> x
  | SList (m, (SAtom (_, "do*") as do_) :: children) -> (
      let children =
        children |> List.map invoke
        |> List.concat_map (function
             | SList (_, SAtom (_, "do*") :: children) -> children
             | x -> [ x ])
      in
      match children with [ x ] -> x | children -> SList (m, do_ :: children))
  | SList (m, (SAtom (_, "def*") as def_) :: name :: value) ->
      let value = List.map invoke value in
      SList (m, def_ :: name :: value)
  | SList (m, (SAtom (_, "let*") as let_) :: name :: value) ->
      let value = List.map invoke value in
      SList (m, let_ :: name :: value)
  (* if* *)
  | SList (m, [ (SAtom (_, "if*") as if_); cond; then_; else_ ]) ->
      let cond = invoke cond in
      let then_ = invoke then_ in
      let else_ = invoke else_ in
      SList (m, [ if_; cond; then_; else_ ])
  | SList (m, (SAtom (_, "fn*") as fn_) :: args :: body) ->
      let body = List.map invoke body in
      SList (m, fn_ :: args :: body)
  | SList (_, SAtom (_, n) :: _) as x
    when String.ends_with ~suffix:"*" n && n <> "*" ->
      failsexp __LOC__ [ x ]
  | SList (m, f :: args) ->
      let f = invoke f in
      let args = List.map invoke args in
      SList (m, f :: args)
  | x -> failsexp __LOC__ [ x ]
