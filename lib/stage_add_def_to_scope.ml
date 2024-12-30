open Common

let rec invoke (ctx : context) (node : cljexp) : context * cljexp =
  match node with
  | RBList (_, [ Atom (_, "def*"); Atom (_, name); _ ]) as n ->
      let ctx = { ctx with prelude_scope = StringMap.add name () ctx.prelude_scope } in
      (ctx, n)
  | RBList (m, (Atom (_, "do*") as do_) :: body) ->
      let ctx, body = List.fold_left_map invoke ctx body in
      (ctx, RBList (m, do_ :: body))
  | n -> failnode __LOC__ [ n ]
