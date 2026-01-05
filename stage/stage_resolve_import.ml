open Core__.Common

type resolve_ctx = { links : (string * string) list }

let resolve_type_for_node ctx = function
  | SAtom (m, n) when m.symbol <> "" ->
      let className =
        ctx.links |> List.assoc_opt m.symbol |> Option.value ~default:m.symbol
      in
      SAtom ({ m with symbol = className }, n)
  | x -> x

let rec resolve (ctx : resolve_ctx) node =
  (* prerr_endline @@ "[LOG][ResolveImports]:\n" ^ show_sexp node ^ "\n"; *)
  match node with
  | SAtom (m, name) as x when String.contains name '/' -> (
      let parts = String.split_on_char '/' name in
      let clazz = List.hd parts in
      let method_ = List.tl parts |> String.concat "." in
      match List.assoc_opt clazz ctx.links with
      | Some x -> (ctx, SAtom (m, x ^ "." ^ method_))
      | None -> (ctx, x))
  | SAtom (m, name) -> (
      match ctx.links |> List.assoc_opt name with
      | Some x -> (ctx, SAtom (m, x ^ ".class"))
      | None -> (ctx, SAtom (m, name)))
  (* | SList (_, SAtom (_, "def*") :: SAtom (_, "__NS__") :: _) -> (ctx, node) *)
  | SList
      ( _,
        [
          SAtom (_, "def*");
          SAtom (_, name);
          SList (_, [ SAtom (_, "quote*"); SAtom (_, value) ]);
        ] ) ->
      let ctx = { links = (name, value) :: ctx.links } in
      let node = SList (meta_empty, [ SAtom (meta_empty, "do*") ]) in
      (ctx, node)
  | SList (m, [ (SAtom (_, "fn*") as fn_); SList (ma, args); body ]) ->
      let args = List.map (resolve_type_for_node ctx) args in
      let _, body = resolve ctx body in
      (ctx, SList (m, [ fn_; SList (ma, args); body ]))
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
      let name = resolve_type_for_node ctx name in
      let value = value |> List.map (fun x -> resolve ctx x |> snd) in
      (ctx, SList (m, let_ :: name :: value))
  | SList (m, (SAtom (_, "if*") as if_) :: args) ->
      let args = args |> List.map (fun x -> resolve ctx x |> snd) in
      (ctx, SList (m, if_ :: args))
  | SList (m, [ SAtom (_, "__compiler_resolve_type"); SAtom (_, name) ]) ->
      let result =
        let name = unpack_string name in
        ctx.links |> List.assoc_opt name |> Option.value ~default:name
      in
      (ctx, SAtom (m, "\"" ^ result ^ "\""))
  (* Function call *)
  | SList (m, fn :: args) ->
      let _, fn = resolve ctx fn in
      let args = List.map (fun x -> resolve ctx x |> snd) args in
      (ctx, SList (m, fn :: args))
  | x -> failsexp __LOC__ [ x ]

let do_resolve node =
  (* prerr_endline @@ "[LOG][ResolveImports]:\n" ^ show_sexp node; *)
  let ctx = { links = [ ("String", "String") ] } in
  resolve ctx node |> snd
