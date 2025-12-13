open Core__.Common

type ctx = { aliases : (string * string) list; base_ns : string }
[@@deriving show]

let compute_name ctx name =
  let parts = String.split_on_char '/' name in
  match List.assoc_opt (List.hd parts) ctx.aliases with
  | Some pkg ->
      (* let path =
        Filename.concat
          (Filename.dirname (FileReader.realpath ctx.filename))
          (pkg ^ ".clj")
        |> Files.realpath
      in
      let path =
        let path = Filename.chop_extension path in
        let root = ctx.root |> Files.realpath in
        let n = String.length (root |> FileReader.realpath) + 1 in
        String.sub path n (String.length path - n)
      in
      let clazz = Str.global_replace (Str.regexp "/") "." path in
      let fun_name = List.nth parts 1 in
      Some (Printf.sprintf "%s.%s#%s" ctx.base_ns clazz fun_name) *)
      Some pkg
  | _ -> None

let rec invoke (ctx : ctx) = function
  | SAtom (m, name) as x when String.contains name '/' ->
      (* *)
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
        items
        (* |> trace "__ns_aliases:" (fun xs ->
            xs |> List.map show_sexp2 |> String.concat ", ") *)
        (* |> List.map (function SAtom (_, x) -> x | x -> failsexp __LOC__ [ x ]) *)
        |> List.split_into_pairs
        |> List.map (function
          | SAtom (_, k), SList (_, [ _; SAtom (_, v) ]) -> (k, v)
          | k, v -> failsexp __LOC__ [ k; v ])
      in
      let ctx = { ctx with aliases = items } in
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

let do_invoke base_ns node =
  let ctx = { aliases = []; base_ns } in
  invoke ctx node |> snd
