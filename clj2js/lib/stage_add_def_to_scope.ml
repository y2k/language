open Common

let rec invoke (ctx : context) (node : cljexp) : context * cljexp =
  match node with
  | RBList [ Atom (_, "def*"); Atom (_, name); _ ] as n ->
      let ctx =
        { ctx with prelude_scope = StringMap.add name () ctx.prelude_scope }
      in
      (ctx, n)
  | RBList ((Atom (_, "do*") as do_) :: body) ->
      let ctx, body = List.fold_left_map invoke ctx body in
      (ctx, RBList (do_ :: body))
  | n -> failnode __LOC__ [ n ]
