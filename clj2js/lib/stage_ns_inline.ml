open Common

module Loader = struct
  type _ Effect.t += Load : string -> string Effect.t

  let with_scope f =
    let open Effect.Deep in
    Effect.Deep.try_with f ()
      {
        effc =
          (fun (type a) (eff : a Effect.t) ->
            match eff with
            | Load path -> Some (fun (k : (a, _) continuation) -> continue k In_channel.(with_open_bin path input_all))
            | _ -> None);
      }
end

let merge_path base_path rel_path =
  prerr_endline @@ "LOG1: " ^ base_path ^ " | " ^ rel_path;
  let rec loop xs ps =
    match (xs, ps) with _ :: xs, ".." :: ps -> loop xs ps | xs, "." :: ps -> loop xs ps | xs, ps -> List.rev xs @ ps
  in
  loop (String.split_on_char '/' base_path |> List.rev |> List.tl) (String.split_on_char '/' rel_path)
  |> String.concat "/"

let load_file path = In_channel.(with_open_bin path input_all)

let rec invoke (execute_code : string -> string -> context * cljexp) (context : context) = function
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
                              let mod_path = unpack_string mod_path ^ ".clj" in
                              let full_mod_path = merge_path context.filename mod_path in
                              let code = load_file full_mod_path in
                              let ch_ctx, _ = execute_code full_mod_path code in
                              { context with imports = StringMap.add alias ch_ctx context.imports }
                          | n -> failnode __LOC__ [ n ])
                        context
               | n -> failnode __LOC__ [ n ])
             context
      in
      (context, RBList (unknown_location, [ Atom (unknown_location, "do*") ]))
  | RBList (m, (Atom (_, "do*") as do_) :: body) ->
      let body = List.concat_map (Fun.compose unwrap_do (Fun.compose snd (invoke execute_code context))) body in
      (context, RBList (m, do_ :: body))
  | RBList (_, _) as x -> (context, x)
  | n -> failnode __LOC__ [ n ]
