open Common

let try_unwrap_do = function SList (_, [ SAtom (_, "do*"); body ]) -> body | node -> node

let merge_path base_path mod_path =
  let rec loop xs ps =
    match (xs, ps) with _ :: xs, ".." :: ps -> loop xs ps | xs, "." :: ps -> loop xs ps | xs, ps -> List.rev xs @ ps
  in
  loop (String.split_on_char '/' base_path |> List.rev |> List.tl) (String.split_on_char '/' mod_path)
  |> String.concat "/"

let load_file base_path mod_path =
  let mod_path = unpack_string mod_path ^ ".clj" in
  let path = merge_path base_path mod_path in
  (* prerr_endline @@ "LOG[load_file] " ^ base_path ^ " | " ^ mod_path ^ " | " ^ path; *)
  (path, FileReader.read path)

type execute_config = { path : string; code : string }
type ns_contex = { ns : string; scope : unit StringMap.t; aliases : string StringMap.t }

let rec get_namespace (node : sexp) : string =
  match node with
  | SList (_, SAtom (_, "def*") :: SAtom (_, fn) :: _) when String.contains fn '/' -> failsexp "Deprecated" [ node ]
  | SList (_, SAtom (_, "def*") :: SAtom (_, fn) :: _) ->
      let ns_len = Scanf.sscanf fn "G%i%s" (fun i _ -> i) in
      String.sub fn (1 + String.length (string_of_int ns_len)) ns_len
  | SList (_, SAtom (_, "do*") :: body) -> get_namespace (List.hd body)
  | n -> failsexp __LOC__ [ n ]

let mangle_name (ns : string) (name : string) : string =
  Printf.sprintf "G%i%s%i%s" (String.length ns) ns (String.length name) name

let is_number (x : string) =
  let c = String.get x 0 in
  (c >= '0' && c <= '9') || c = '-'

let is_literal (x : string) =
  let c = String.get x 0 in
  c = ':' || c = '\"' || is_number x || x = "nil"

let resolve_name global_ctx ctx fn_name =
  if String.contains fn_name '/' && fn_name <> "/" then
    let i = String.index fn_name '/' in
    let al_name = String.sub fn_name 0 i in
    let mod_name = ctx.aliases |> StringMap.find al_name in
    let fn_name = String.sub fn_name (i + 1) (String.length fn_name - i - 1) in
    mangle_name mod_name fn_name
  else if StringMap.mem fn_name global_ctx.prelude_scope || StringMap.mem fn_name ctx.scope then fn_name
  else mangle_name ctx.ns fn_name

let invoke (config : config) (execute_code : execute_config -> sexp) (global_ctx : context) (node : sexp) : sexp =
  let rec invoke_with_ctx (ctx : ns_contex) node : ns_contex * sexp =
    let invoke ctx node = invoke_with_ctx ctx node |> snd in
    match node with
    | SAtom (m, name) when not (is_literal name) ->
        let full_name = resolve_name global_ctx ctx name in
        (ctx, SAtom (m, full_name))
    | SList (m, [ (SAtom (_, "if*") as i); cond; then_; else_ ]) ->
        (ctx, SList (m, [ i; invoke ctx cond; invoke ctx then_; invoke ctx else_ ]))
    | SList (m2, [ (SAtom (_, "def*") as def); SAtom (m, name) ]) ->
        (ctx, SList (m2, [ def; SAtom (m, mangle_name ctx.ns name) ]))
    | SList (m2, [ (SAtom (_, "def*") as def); SAtom (m, name); value ]) ->
        (ctx, SList (m2, [ def; SAtom (m, mangle_name ctx.ns name); invoke ctx value ]))
    | SList (m, (SAtom (_, "fn*") as fn_) :: (SList (_, arg_names) as args) :: body) ->
        let scope =
          arg_names |> List.to_seq
          |> Seq.map (function SAtom (_, name) -> (name, ()) | n -> failsexp __LOC__ [ n ])
          |> Fun.flip StringMap.add_seq ctx.scope
        in
        let ctx = { ctx with scope } in
        (ctx, SList (m, fn_ :: args :: List.map (invoke ctx) body))
    | SList
        ( m,
          (SAtom (_, "do*") as do_)
          :: SList (_, [ SAtom (_, "ns"); SList (_, [ _; SList (_, SAtom (_, ns_name) :: ns_body) ]) ])
          :: body ) ->
        let ctx, inlide_modules =
          ns_body
          |> List.fold_left
               (fun (ctx, acc) node ->
                 match node with
                 | SList (_, SAtom (_, ":require") :: reqs) ->
                     let nodes =
                       reqs
                       |> List.fold_left
                            (fun (ctx, acc) req ->
                              match req with
                              | SList (_, [ SAtom (_, ch_ns); _; SAtom (_, aln) ])
                                when (not (String.starts_with ~prefix:"\"" ch_ns)) && config.no_deps ->
                                  let ctx = { ctx with aliases = StringMap.add aln ch_ns ctx.aliases } in
                                  (ctx, acc)
                              | SList (_, [ SAtom (_, mod_path); _; SAtom (_, aln) ])
                                when String.starts_with ~prefix:"\"" mod_path && not config.no_deps ->
                                  let path, code = load_file global_ctx.filename mod_path in
                                  let ch_node = execute_code { path; code } in
                                  let ch_ns = get_namespace ch_node in
                                  let ctx = { ctx with aliases = StringMap.add aln ch_ns ctx.aliases } in
                                  (ctx, acc @ [ ch_node ])
                              | n -> failsexp __LOC__ [ n ])
                            (ctx, acc)
                     in
                     nodes
                 | n -> failsexp __LOC__ [ n ])
               (ctx, [])
        in
        let ctx = { ctx with ns = ns_name } in
        let body = List.map (invoke ctx) body in
        (ctx, SList (m, do_ :: (inlide_modules @ body)) |> try_unwrap_do)
    | SList (m, (SAtom (_, "do*") as do_) :: body) ->
        let ctx, body = List.fold_left_map invoke_with_ctx ctx body in
        (ctx, SList (m, do_ :: body) |> try_unwrap_do)
    | SList (m, [ (SAtom (_, "let*") as let_); (SAtom (_, name) as n); value ]) ->
        let ctx = { ctx with scope = StringMap.add name () ctx.scope } in
        (ctx, SList (m, [ let_; n; invoke ctx value ]))
    | SList (_, SAtom (_, name) :: _) as n when String.ends_with ~suffix:"*" name -> failsexp __LOC__ [ n ]
    (* Function call *)
    | SList (m2, SAtom (m, fn_name) :: args) ->
        let args = List.map (invoke ctx) args in
        let full_name = resolve_name global_ctx ctx fn_name in
        (ctx, SList (m2, SAtom (m, full_name) :: args))
    | node -> (ctx, node)
  in
  invoke_with_ctx { ns = "user"; scope = StringMap.empty; aliases = StringMap.empty } node |> snd
