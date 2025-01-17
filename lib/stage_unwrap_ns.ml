open Common

type ns_contex = { ns : string }

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

let invoke (execute_code : execute_config -> sexp) (global_ctx : context) (node : sexp) : sexp =
  let rec invoke (ctx : ns_contex) = function
    | SList (m, (SAtom (_, "let*") as l) :: SList (m2, bindings) :: body) ->
        SList (m, l :: SList (m2, List.map (invoke ctx) bindings) :: List.map (invoke ctx) body)
    | SList (m, [ (SAtom (_, "if*") as i); cond; then_; else_ ]) ->
        SList (m, [ i; invoke ctx cond; invoke ctx then_; invoke ctx else_ ])
    | SList (m2, [ (SAtom (_, "def*") as def); SAtom (m, name) ]) -> SList (m2, [ def; SAtom (m, ctx.ns ^ "/" ^ name) ])
    | SList (m2, [ (SAtom (_, "def*") as def); SAtom (m, name); value ]) ->
        SList (m2, [ def; SAtom (m, ctx.ns ^ "/" ^ name); invoke ctx value ])
    | SList (m, (SAtom (_, "fn*") as fn_) :: args :: body) -> SList (m, fn_ :: args :: List.map (invoke ctx) body)
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
        let ctx = { ns = ns_name } in
        let body = List.map (invoke ctx) body in
        SList (m, do_ :: (inlide_modules @ body)) |> try_unwrap_do
    | SList (m, (SAtom (_, "do*") as do_) :: body) -> SList (m, do_ :: List.map (invoke ctx) body) |> try_unwrap_do
    | SList (m, (SAtom (_, "let*") as let_) :: args) -> SList (m, let_ :: List.map (invoke ctx) args)
    | SList (_, SAtom (_, name) :: _) as n when String.ends_with ~suffix:"*" name -> failsexp __LOC__ [ n ]
    (* Function call *)
    | SList (m2, SAtom (m, fn_name) :: args) ->
        let args = List.map (invoke ctx) args in
        let prefix =
          if StringMap.mem fn_name global_ctx.prelude_scope || String.contains fn_name '/' then "" else ctx.ns ^ "/"
        in
        SList (m2, SAtom (m, prefix ^ fn_name) :: args)
    | node -> node
  in
  invoke { ns = "user" } node
