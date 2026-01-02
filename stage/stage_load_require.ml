open Core__.Common

type options = { compile : string -> sexp }
type context = { opt : options }

let rec invoke (ctx : context) = function
  | SAtom _ as x -> (ctx, x)
  | SList (m, (SAtom (_, "do*") as do_) :: children) ->
      let ctx, children = List.fold_left_map invoke ctx children in
      (ctx, SList (m, do_ :: children))
  | SList
      ( _,
        SAtom (_, "def*")
        :: SAtom (_, "__ns_aliases")
        :: SList (_, [ SAtom (_, "quote*"); SList (_, items) ])
        :: _ ) ->
      let items =
        items |> List.split_into_pairs
        |> List.map (function
          | _, SList (_, [ _; SAtom (_, path); _ ]) -> ctx.opt.compile path
          | k, v -> failsexp __LOC__ [ k; v ])
      in
      (ctx, SList (meta_empty, SAtom (meta_empty, "do*") :: items))
  | SList (_, SAtom (_, "def*") :: _) as x -> (ctx, x)
  | SList (_, SAtom (_, n) :: _) as x
    when String.ends_with ~suffix:"*" n && n <> "*" ->
      failsexp __LOC__ [ x ]
  | SList (_, _ :: _) as x -> (ctx, x)
  | x -> failsexp __LOC__ [ x ]

let do_invoke compile (node : sexp) : sexp =
  invoke { opt = { compile } } node |> snd
