open Core__.Common
module StringSet = Set.Make (String)

type resolve_ctx = {
  links : string StringMap.t;
  aliases : (string * string) list;
  namespace : string;
  prelude_fns : StringSet.t;
  registered_defs : StringSet.t;
}

let mangle_from_path ns fname = Printf.sprintf "%s.%s" ns fname

let rec resolve (ctx : resolve_ctx) node =
  match node with
  | SAtom (m, name) -> (
      match ctx.links |> StringMap.find_opt name with
      | Some x -> (ctx, SAtom (m, x))
      | None -> (ctx, SAtom (m, name)))
  | SList
      (_, [ SAtom (_, "def*"); SAtom (_, "__namespace"); SAtom (_, namespace) ])
    ->
      let namespace = unpack_symbol namespace in
      ({ ctx with namespace }, SList (meta_empty, [ SAtom (meta_empty, "do*") ]))
  | SList
      ( _,
        SAtom (_, "def*")
        :: SAtom (_, "__ns_aliases")
        :: SList (_, [ SAtom (_, "quote*"); SList (_, items) ])
        :: _ ) as x ->
      let items =
        items |> List.split_into_pairs
        |> List.map (function
          | SAtom (_, k), SList (_, [ SAtom (_, v); _ ]) -> (k, v)
          | k, v -> failsexp __LOC__ [ k; v ])
      in
      let ctx = { ctx with aliases = items } in
      (ctx, x)
  | SList (m, SAtom (_, "def*") :: SAtom (_, "__NS__") :: _) ->
      (ctx, SList (m, [ SAtom (meta_empty, "do*") ]))
  (* | SList
      ( _,
        [
          SAtom (_, "def*");
          SAtom (_, name);
          SList (_, [ SAtom (_, "quote*"); SAtom (_, value) ]);
        ] ) ->
      let ctx = { ctx with links = (name, value) :: ctx.links } in
      let node = SList (meta_empty, [ SAtom (meta_empty, "do*") ]) in
      (ctx, node) *)
  | SList (m, [ (SAtom (_, "def*") as def_); SAtom (mn, name); value ]) ->
      let mng_name = mangle_from_path ctx.namespace name in
      let _, value = resolve ctx value in
      let ctx =
        {
          ctx with
          registered_defs = StringSet.add name ctx.registered_defs;
          links = StringMap.add name mng_name ctx.links;
        }
      in
      (ctx, SList (m, [ def_; SAtom (mn, mng_name); value ]))
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
  | SList (m, (SAtom (_, "let*") as let_) :: (SAtom (_, lname) as name) :: value)
    ->
      let value = value |> List.map (fun x -> resolve ctx x |> snd) in
      let ctx = { ctx with links = StringMap.remove lname ctx.links } in
      (ctx, SList (m, let_ :: name :: value))
  | SList (m, (SAtom (_, "if*") as if_) :: args) ->
      let args = args |> List.map (fun x -> resolve ctx x |> snd) in
      (ctx, SList (m, if_ :: args))
  (* Function call *)
  | SList (m, SAtom (_, fun_name) :: args)
    when not (String.ends_with ~suffix:"*" fun_name) ->
      let _, args = List.fold_left_map (fun ctx x -> resolve ctx x) ctx args in
      let fun_name =
        if
          String.get fun_name 0 < 'a'
          || String.get fun_name 0 > 'z'
          || StringSet.mem fun_name ctx.prelude_fns
          || not (StringSet.mem fun_name ctx.registered_defs)
        then fun_name
        else if String.contains fun_name '/' then
          let alias_name = String.split_on_char '/' fun_name |> List.hd in
          ctx.aliases |> List.assoc_opt alias_name
          |> Option.map (fun x ->
              let fun_name =
                String.split_on_char '/' fun_name
                |> List.tl |> String.concat "/"
              in
              NamespaceUtils.path_to_namespace fun_name x)
          |> Option.value ~default:fun_name
        else mangle_from_path ctx.namespace fun_name
      in
      (ctx, SList (m, SAtom (m, fun_name) :: args))
  | SList (m, fn :: args) ->
      let _, fn = resolve ctx fn in
      let args = List.map (fun x -> resolve ctx x |> snd) args in
      (ctx, SList (m, fn :: args))
  | x -> failsexp __LOC__ [ x ]

let do_resolve functions filename _ node =
  if filename = "prelude.clj" then node
  else
    let ctx =
      {
        links = StringMap.empty;
        aliases = [];
        namespace = "user";
        prelude_fns = StringSet.of_list functions;
        registered_defs = StringSet.empty;
      }
    in
    resolve ctx node |> snd
