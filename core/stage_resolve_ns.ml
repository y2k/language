open Common
module StringSet = Set.Make (String)

type resolve_ctx = {
  links : (string * string) list;
  aliases : (string * string) list;
  filename : string;
  root_dir : string;
  prelude_fns : StringSet.t;
}

let rec resolve (ctx : resolve_ctx) node =
  match node with
  | SAtom (m, name) -> (
      match ctx.links |> List.assoc_opt name with
      | Some x -> (ctx, SAtom (m, x))
      | None -> (ctx, SAtom (m, name)))
  | SList
      ( _,
        SAtom (_, "def*")
        :: SAtom (_, "__ns_aliases")
        :: SList (_, [ SAtom (_, "quote*"); SList (_, items) ])
        :: _ ) as x ->
      let items =
        items
        (* |> List.map (function SAtom (_, x) -> x | x -> failsexp __LOC__ [ x ]) *)
        |> List.split_into_pairs
        |> List.map (function
             | SAtom (_, k), SList (_, [ SAtom (_, v); _ ]) -> (k, v)
             | k, v -> failsexp __LOC__ [ k; v ])
      in
      let ctx = { ctx with aliases = items } in
      (* (ctx, SList (meta_empty, [ SAtom (meta_empty, "do*") ])) *)
      (ctx, x)
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
  | SList (m, [ (SAtom (_, "def*") as def_); SAtom (mn, name); value ]) ->
      let name =
        NamespaceUtils.mangle_from_path ctx.root_dir ctx.filename name
      in
      (* prerr_endline @@ "[ResolveNS:def*] " ^ ctx.filename ^ " | " ^ ctx.root_dir
      ^ " -> " ^ name; *)
      let _, value = resolve ctx value in
      (ctx, SList (m, [ def_; SAtom (mn, name); value ]))
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
  (* Function call *)
  | SList (m, SAtom (_, fun_name) :: args)
    when not (String.ends_with ~suffix:"*" fun_name) ->
      let _, args = List.fold_left_map (fun ctx x -> resolve ctx x) ctx args in
      let fun_name =
        if
          String.get fun_name 0 < 'a'
          || String.get fun_name 0 > 'z'
          || StringSet.mem fun_name ctx.prelude_fns
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
        else NamespaceUtils.mangle_from_path ctx.root_dir ctx.filename fun_name
      in
      (ctx, SList (m, SAtom (m, fun_name) :: args))
  | SList (m, fn :: args) ->
      let _, fn = resolve ctx fn in
      let args = List.map (fun x -> resolve ctx x |> snd) args in
      (ctx, SList (m, fn :: args))
  | x -> failsexp __LOC__ [ x ]

let do_resolve filename root_dir node =
  if filename = "prelude.clj" then node
  else
    let ctx =
      {
        links = [];
        aliases = [];
        filename;
        root_dir;
        prelude_fns =
          StringSet.of_list
            [
              "boolean";
              "count";
              "drop";
              "gensym";
              "get";
              "hash-map";
              "map?";
              "map";
              "reduce";
              "set!";
              "some?";
              "str";
              "vector?";
              "vector";
              "vendor";
            ];
      }
    in
    resolve ctx node |> snd
