open Common

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
  (path, In_channel.(with_open_bin path input_all))

let rec invoke (execute_code : string -> string -> context * obj) (context : context) = function
  | Atom _ as x -> (context, x)
  | RBList (_, [ Atom (_, "ns"); RBList (_, [ _; RBList (_, _ :: body) ]) ]) ->
      let context =
        body
        |> List.fold_left
             (fun context node ->
               match node with
               | RBList (_, Atom (_, ":require") :: reqs) ->
                   reqs
                   |> List.fold_left
                        (fun context req ->
                          match req with
                          | SBList (_, [ Atom (_, mod_path); _; Atom (_, alias) ]) ->
                              let full_mod_path, code = load_file context.filename mod_path in
                              let ch_ctx, _ = execute_code full_mod_path code in
                              { context with imports = StringMap.add alias ch_ctx context.imports }
                          | n -> failnode __LOC__ [ n ])
                        context
               | n -> failnode __LOC__ [ n ])
             context
      in
      (context, RBList (unknown_location, [ Atom (unknown_location, "do*") ]))
  | RBList (m, (Atom (_, "do*") as do_) :: body) ->
      let context, body = body |> List.fold_left_map (fun ctx node -> invoke execute_code ctx node) context in
      let body = List.concat_map unwrap_do body in
      (context, RBList (m, do_ :: body))
  | RBList (_, _) as x -> (context, x)
  | n -> failnode __LOC__ [ n ]
