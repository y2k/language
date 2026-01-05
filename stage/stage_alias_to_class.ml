open Core__.Common

type ctx = { aliases : (string * string) list }

let compute_name ctx name =
  let parts = String.split_on_char '/' name in
  match List.assoc_opt (List.hd parts) ctx.aliases with
  | Some pkg -> Some pkg
  | _ -> None

let rec transform (ctx : ctx) = function
  | SAtom (m, name) as x when String.contains name '/' ->
      compute_name ctx name
      |> Option.fold ~none:(ctx, x) ~some:(fun name -> (ctx, SAtom (m, name)))
  | SAtom _ as x -> (ctx, x)
  | SList
      ( m,
        SAtom (_, "def*")
        :: SAtom (_, "__ns_aliases")
        :: SList (_, [ SAtom (_, "quote*"); SList (_, items) ])
        :: _ ) ->
      let items =
        items |> List.split_into_pairs
        |> List.map (function
          | SAtom (_, k), SList (_, [ _; SAtom (_, v); _ ]) -> (k, v)
          | k, v -> failsexp __LOC__ [ k; v ])
      in
      let ctx = { aliases = items } in
      (ctx, SList (m, [ SAtom (meta_empty, "do*") ]))
  | SList (m, SAtom (mdo, "do*") :: children) ->
      let ctx, children = List.fold_left_map transform ctx children in
      (ctx, SList (m, SAtom (mdo, "do*") :: children))
  | SList (m, [ SAtom (mfn, "fn*"); args; body ]) ->
      let ctx, body = transform ctx body in
      (ctx, SList (m, [ SAtom (mfn, "fn*"); args; body ]))
  | SList (m, [ SAtom (mdefd, "def*"); name; value ]) ->
      let ctx, value = transform ctx value in
      (ctx, SList (m, [ SAtom (mdefd, "def*"); name; value ]))
  | SList (m, name :: args) ->
      let ctx, name = transform ctx name in
      let ctx, args = List.fold_left_map transform ctx args in
      (ctx, SList (m, name :: args))
  | x -> failsexp __LOC__ [ x ]

let invoke node =
  let ctx = { aliases = [] } in
  transform ctx node |> snd
