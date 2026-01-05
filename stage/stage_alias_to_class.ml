open Core__.Common

type ctx = { root_namespace : string; aliases : (string * string) list }
[@@deriving show]

let compute_name ctx name =
  let parts = String.split_on_char '/' name in
  match (parts, List.assoc_opt (List.hd parts) ctx.aliases) with
  | _ :: fname :: _, Some pkg ->
      Some (ctx.root_namespace ^ "." ^ pkg ^ "#" ^ fname)
  | _ -> None

let rec transform (ctx : ctx) = function
  | SAtom (m, name) as x when String.contains name '/' ->
      compute_name ctx name
      |> Option.fold ~none:(ctx, x) ~some:(fun name -> (ctx, SAtom (m, name)))
  | SAtom _ as x -> (ctx, x)
  | SList (m, SAtom (_, "def*") :: SAtom (_, "__ns_aliases") :: _) ->
      (ctx, SList (m, [ SAtom (meta_empty, "do*") ]))
  | SList
      (m, [ SAtom (_, "def*"); SAtom (_, "__NS__"); SList (_, _ :: _ :: args) ])
    ->
      let pairs = List.split_into_pairs args in
      let ctx =
        {
          ctx with
          aliases =
            pairs
            |> List.map (function
              | SAtom (_, path), SAtom (_, alias) ->
                  (unpack_symbol alias, unpack_symbol path)
              | k, v -> failsexp __LOC__ [ k; v ]);
        }
      in
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
  (* | SList (_, SAtom (_, n) :: _) as x
    when String.ends_with ~suffix:"*" n && n <> "*" ->
      failsexp __LOC__ [ x ] *)
  | SList (m, name :: args) ->
      (* prerr_endline @@ show_sexp2 node ^ " | " ^ show_ctx ctx; *)
      let ctx, name = transform ctx name in
      let ctx, args = List.fold_left_map transform ctx args in
      (ctx, SList (m, name :: args))
  | x -> failsexp __LOC__ [ x ]

let invoke ~root_namespace node =
  let ctx = { root_namespace; aliases = [] } in
  transform ctx node |> snd
