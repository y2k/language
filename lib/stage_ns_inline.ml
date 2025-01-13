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
  | SAtom _ as x -> (context, x)
  | SList (_, [ SAtom (_, "ns"); SList (_, [ _; SList (_, _ :: body) ]) ]) ->
      let context =
        body
        |> List.fold_left
             (fun context node ->
               match node with
               | SList (_, SAtom (_, ":require") :: reqs) ->
                   reqs
                   |> List.fold_left
                        (fun context req ->
                          match req with
                          | SList (_, [ SAtom (_, mod_path); _; SAtom (_, alias) ]) ->
                              let full_mod_path, code = load_file context.filename mod_path in
                              let ch_ctx, _ = execute_code full_mod_path code in
                              { context with imports = StringMap.add alias ch_ctx context.imports }
                          | n -> failsexp __LOC__ [ n ])
                        context
               | n -> failsexp __LOC__ [ n ])
             context
      in
      (context, SList (unknown_location, [ SAtom (unknown_location, "do*") ]))
  | SList (m, (SAtom (_, "do*") as do_) :: body) ->
      let context, body = body |> List.fold_left_map (fun ctx node -> invoke execute_code ctx node) context in
      let body = List.concat_map unwrap_sexp_do body in
      (context, SList (m, do_ :: body))
  | SList (_, _) as x -> (context, x)
