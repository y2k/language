open Common

let try_unwrap_do = function SList (_, [ SAtom (_, "do*"); body ]) -> body | node -> node

let load_file base_path mod_path =
  let mod_path = unpack_string mod_path ^ ".clj" in
  let merge_path () =
    let rec loop xs ps =
      match (xs, ps) with _ :: xs, ".." :: ps -> loop xs ps | xs, "." :: ps -> loop xs ps | xs, ps -> List.rev xs @ ps
    in
    loop (String.split_on_char '/' base_path |> List.rev |> List.tl) (String.split_on_char '/' mod_path)
    |> String.concat "/"
  in
  let path = merge_path () in
  (* prerr_endline @@ "LOG[load_file] " ^ base_path ^ " | " ^ mod_path ^ " | " ^ path; *)
  (path, FileReader.read path)

type execute_config = { path : string; code : string }
type ns_contex = { ns : string; scope : unit StringMap.t }

let invoke (execute_code : execute_config -> sexp) (global_ctx : context) (node : sexp) : sexp =
  let rec invoke_with_ctx (ctx : ns_contex) node : ns_contex * sexp =
    let invoke ctx node = invoke_with_ctx ctx node |> snd in
    match node with
    | SList (m, [ (SAtom (_, "if*") as i); cond; then_; else_ ]) ->
        (ctx, SList (m, [ i; invoke ctx cond; invoke ctx then_; invoke ctx else_ ]))
    | SList (m2, [ (SAtom (_, "def*") as def); SAtom (m, name) ]) ->
        (ctx, SList (m2, [ def; SAtom (m, ctx.ns ^ "/" ^ name) ]))
    | SList (m2, [ (SAtom (_, "def*") as def); SAtom (m, name); value ]) ->
        (ctx, SList (m2, [ def; SAtom (m, ctx.ns ^ "/" ^ name); invoke ctx value ]))
    | SList (m, (SAtom (_, "fn*") as fn_) :: args :: body) -> (ctx, SList (m, fn_ :: args :: List.map (invoke ctx) body))
    | SList
        ( m,
          (SAtom (_, "do*") as do_)
          :: SList (_, [ SAtom (_, "ns"); SList (_, [ _; SList (_, SAtom (_, ns_name) :: ns_body) ]) ])
          :: body ) ->
        let inlide_modules =
          ns_body
          |> List.fold_left
               (fun acc node ->
                 match node with
                 | SList (_, SAtom (_, ":require") :: reqs) ->
                     reqs
                     |> List.fold_left
                          (fun acc req ->
                            match req with
                            | SList (_, [ SAtom (_, mod_path); _; SAtom (_, _) ]) ->
                                let path, code = load_file global_ctx.filename mod_path in
                                let ch_node = execute_code { path; code } in
                                acc @ [ ch_node ]
                            | n -> failsexp __LOC__ [ n ])
                          acc
                 | n -> failsexp __LOC__ [ n ])
               []
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
        (* prerr_endline @@ "LOG:CALL: " ^ (ctx.scope |> StringMap.bindings |> List.map fst |> String.concat ","); *)
        let args = List.map (invoke ctx) args in
        let prefix =
          if
            StringMap.mem fn_name global_ctx.prelude_scope
            || String.contains fn_name '/' || StringMap.mem fn_name ctx.scope
          then ""
          else ctx.ns ^ "/"
        in
        (ctx, SList (m2, SAtom (m, prefix ^ fn_name) :: args))
    | node -> (ctx, node)
  in
  invoke_with_ctx { ns = "user"; scope = StringMap.empty } node |> snd
