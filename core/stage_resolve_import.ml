open Lib__.Common

type resolve_ctx = {
  links : (string * string) list;
  filename : string;
  root_dir : string;
}

let rec resolve (ctx : resolve_ctx) node =
  match node with
  | SAtom (m, name) -> (
      match ctx.links |> List.assoc_opt name with
      | Some x -> (ctx, SAtom (m, x))
      | None -> (ctx, SAtom (m, name)))
  | SList
      ( _,
        [
          SAtom (_, "def*");
          SAtom (_, name);
          SList (_, [ SAtom (_, "quote*"); SAtom (_, value) ]);
        ] ) ->
      let ctx = { ctx with links = (name, value) :: ctx.links } in
      let node = SList (meta_empty, [ SAtom (meta_empty, "do*") ]) in
      (ctx, node)
  (* | SList (m, [ (SAtom (_, "def*") as def_); SAtom (mn, name); value ]) ->
      let name =
        NamespaceUtils.mangle_from_path ctx.root_dir ctx.filename name
      in
      let _, value = resolve ctx value in
      (ctx, SList (m, [ def_; SAtom (mn, name); value ])) *)
  | SList (m, [ (SAtom (_, "fn*") as fn_); args; body ]) ->
      let _, body = resolve ctx body in
      (ctx, SList (m, [ fn_; args; body ]))
  | SList (m, (SAtom (_, "do*") as do_) :: body) ->
      let ctx, body =
        List.fold_left_map (fun ctx x -> resolve ctx x) ctx body
      in
      let body =
        body
        |> List.filter_map (function
             | SList (_, [ SAtom (_, "do*") ]) -> None
             | x -> Some x)
      in
      (ctx, SList (m, do_ :: body))
  | SList (m, (SAtom (_, "let*") as let_) :: name :: value) ->
      let value = value |> List.map (fun x -> resolve ctx x |> snd) in
      (ctx, SList (m, let_ :: name :: value))
  | SList (m, (SAtom (_, "if*") as if_) :: args) ->
      let args = args |> List.map (fun x -> resolve ctx x |> snd) in
      (ctx, SList (m, if_ :: args))
  | SList (m, fn :: args) ->
      let _, fn = resolve ctx fn in
      let args = List.map (fun x -> resolve ctx x |> snd) args in
      (ctx, SList (m, fn :: args))
  | x -> failsexp __LOC__ [ x ]

let do_resolve filename root_dir node =
  let ctx = { links = []; filename; root_dir } in
  resolve ctx node |> snd
