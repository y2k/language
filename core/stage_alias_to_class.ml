open Lib__.Common

type ctx = { aliases : (string * string) list }

let rec invoke (ctx : ctx) = function
  | SAtom (m, name) as x when String.contains name '/' -> (
      (* *)
      let parts = String.split_on_char '/' name in
      let pkg = List.assoc_opt (List.hd parts) ctx.aliases in
      match pkg with
      | Some pkg ->
          let name = List.nth parts 1 in
          let clazz =
            let xs = String.split_on_char '.' pkg in
            let pkg = xs |> List.rev |> List.tl |> List.rev in
            let cls = xs |> List.rev |> List.hd |> String.capitalize_ascii in
            String.concat "." (pkg @ [ cls ])
          in
          let name = Printf.sprintf "%s#%s" clazz name in
          (ctx, SAtom (m, name))
      | _ -> (ctx, x))
  | SAtom _ as x -> (ctx, x)
  | SList
      ( m,
        SAtom (_, "def*")
        :: SAtom (_, "__ns_aliases")
        :: SList (_, [ SAtom (_, "quote*"); SList (_, items) ])
        :: _ ) ->
      let items =
        items
        (* |> List.map (function SAtom (_, x) -> x | x -> failsexp __LOC__ [ x ]) *)
        |> List.split_into_pairs
        |> List.map (function
             | SAtom (_, k), SList (_, [ SAtom (_, v); _ ]) -> (k, v)
             | k, v -> failsexp __LOC__ [ k; v ])
      in
      let ctx = { aliases = items } in
      (* (ctx, SList (meta_empty, [ SAtom (meta_empty, "do*") ])) *)
      (ctx, SList (m, [ SAtom (meta_empty, "do*") ]))
  | SList (m, SAtom (mdo, "do*") :: children) ->
      let ctx, children = List.fold_left_map invoke ctx children in
      (ctx, SList (m, SAtom (mdo, "do*") :: children))
  | SList (m, [ SAtom (mfn, "fn*"); args; body ]) ->
      let ctx, body = invoke ctx body in
      (ctx, SList (m, [ SAtom (mfn, "fn*"); args; body ]))
  | SList (m, [ SAtom (mdefd, "def*"); name; value ]) ->
      let ctx, value = invoke ctx value in
      (ctx, SList (m, [ SAtom (mdefd, "def*"); name; value ]))
  | SList (m, name :: args) ->
      let ctx, name = invoke ctx name in
      let ctx, args = List.fold_left_map invoke ctx args in
      (ctx, SList (m, name :: args))
  | x -> failsexp __LOC__ [ x ]

let do_invoke node =
  let ctx = { aliases = [] } in
  invoke ctx node |> snd
