open Common

let rec invoke (ctx : context) (node : cljexp) : cljexp =
  match node with
  | Atom _ as n -> n
  | RBList (m, [ (Atom (_, "if*") as if_); c; t; e ]) ->
      let c = invoke ctx c in
      let t = invoke ctx t in
      let e = invoke ctx e in
      RBList (m, [ if_; c; t; e ])
  | RBList (m, Atom (_, "fn*") :: args :: body) ->
      let body = List.map (invoke ctx) body in
      RBList (m, Atom (m, "fn*") :: args :: body)
  | RBList (_, [ Atom (_, "quote*"); _ ]) as n -> n
  | RBList (m, (Atom (_, "do*") as do_) :: args) ->
      let args = List.map (invoke ctx) args in
      RBList (m, do_ :: args)
  | RBList (m, [ (Atom (_, "let*") as let_); name; value ]) ->
      let value = invoke ctx value in
      RBList (m, [ let_; name; value ])
  | RBList (m, [ (Atom (_, "def*") as def_); name; value ]) ->
      let value = invoke ctx value in
      RBList (m, [ def_; name; value ])
  | RBList (_, Atom (_, name) :: _) when name <> "*" && String.ends_with ~suffix:"*" name -> failnode __LOC__ [ node ]
  | RBList (_, [ Atom (_, "eval!"); exp ]) ->
      let _, result = ctx.interpreter ctx exp in
      result
  | RBList (m, name :: args) ->
      let name = invoke ctx name in
      let args = List.map (invoke ctx) args in
      RBList (m, name :: args)
  | n -> failnode __LOC__ [ n ]
