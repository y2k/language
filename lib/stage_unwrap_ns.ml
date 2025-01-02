open Common

type ns_contex = { ns : string }

let try_unwrap_do = function RBList (_, [ Atom (_, "do*"); body ]) -> body | node -> node

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
  (path, FileReader.read path)

type execute_config = { code : string }

let invoke (execute_code : execute_config -> cljexp) (global_ctx : context) (node : cljexp) : cljexp =
  let rec invoke (ctx : ns_contex) = function
    | RBList (m, (Atom (_, "let*") as l) :: SBList (m2, bindings) :: body) ->
        RBList (m, l :: SBList (m2, List.map (invoke ctx) bindings) :: List.map (invoke ctx) body)
    | RBList (m, [ (Atom (_, "if*") as i); cond; then_; else_ ]) ->
        RBList (m, [ i; invoke ctx cond; invoke ctx then_; invoke ctx else_ ])
    | RBList (m2, [ (Atom (_, "def*") as def); Atom (m, name) ]) -> RBList (m2, [ def; Atom (m, ctx.ns ^ "/" ^ name) ])
    | RBList (m2, [ (Atom (_, "def*") as def); Atom (m, name); value ]) ->
        RBList (m2, [ def; Atom (m, ctx.ns ^ "/" ^ name); invoke ctx value ])
    | RBList (m, (Atom (_, "fn*") as fn_) :: args :: body) -> RBList (m, fn_ :: args :: List.map (invoke ctx) body)
    | RBList
        ( m,
          (Atom (_, "do*") as do_)
          :: RBList (_, [ Atom (_, "ns"); RBList (_, [ _; RBList (_, Atom (_, ns_name) :: ns_body) ]) ])
          :: body ) ->
        let inlide_modules =
          ns_body
          |> List.fold_left
               (fun acc node ->
                 match node with
                 | RBList (_, Atom (_, ":require") :: reqs) ->
                     reqs
                     |> List.fold_left
                          (fun acc req ->
                            match req with
                            | SBList (_, [ Atom (_, mod_path); _; Atom (_, _) ]) ->
                                let _, code = load_file global_ctx.filename mod_path in
                                let ch_node = execute_code { code } in
                                acc @ [ ch_node ]
                            | n -> failnode __LOC__ [ n ])
                          acc
                 | n -> failnode __LOC__ [ n ])
               []
        in
        let ctx = { ns = ns_name } in
        let body = List.map (invoke ctx) body in
        RBList (m, do_ :: (inlide_modules @ body)) |> try_unwrap_do
    | RBList (m, (Atom (_, "do*") as do_) :: body) -> RBList (m, do_ :: List.map (invoke ctx) body) |> try_unwrap_do
    | RBList (m, (Atom (_, "let*") as let_) :: args) -> RBList (m, let_ :: List.map (invoke ctx) args)
    | RBList (_, Atom (_, name) :: _) as n when String.ends_with ~suffix:"*" name -> failnode __LOC__ [ n ]
    (* Function call *)
    | RBList (m2, Atom (m, fn_name) :: args) ->
        let args = List.map (invoke ctx) args in
        let prefix =
          if StringMap.mem fn_name global_ctx.prelude_scope || String.contains fn_name '/' then "" else ctx.ns ^ "/"
        in
        RBList (m2, Atom (m, prefix ^ fn_name) :: args)
    | node -> node
  in
  invoke { ns = "user" } node
