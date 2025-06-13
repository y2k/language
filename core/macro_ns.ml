open Lib__.Common

module NamespaceUtils = struct
  let convert_path_to_ns _ filename path =
    prerr_endline @@ "LOG[NS]: base_path: '" ^ "???" ^ "'"
    ^ " | filename: '" ^ filename ^ "'" ^ " | path: '" ^ path ^ "'\n";
    Filename.concat (Filename.dirname filename) (path ^ ".clj")
    (* |> trace __LOC__ Fun.id *)
    |> FileReader.realpath
    (* |> trace __LOC__ Fun.id *)
    |> String.hash
    |> Printf.sprintf "m%i"
    (* |> trace __LOC__ Fun.id *)

  (* let parent_base_path =
      let xs = String.split_on_char '/' base_path in
      let xs = xs |> List.rev |> List.tl |> List.rev in
      String.concat "/" xs
    in
    (* prerr_endline @@ "LOG[NS]: Convert path to ns: (" ^ base_path ^ "|"
    ^ parent_base_path ^ ") '" ^ filename ^ "' -> " ^ path; *)
    let merge_path base_path rel_path =
      let rec loop xs ps =
        match (xs, ps) with
        | _ :: xs, ".." :: ps -> loop xs ps
        | xs, "." :: ps -> loop xs ps
        | xs, ps -> List.rev xs @ ps
      in
      loop
        (String.split_on_char '/' base_path |> List.rev |> List.tl)
        (String.split_on_char '/' rel_path)
      |> String.concat "/"
    in
    let path = merge_path filename path in
    (* prerr_endline @@ "LOG[NS]2: '" ^ path ^ "'"; *)
    let path =
      if String.starts_with ~prefix:base_path path && base_path <> "" then
        let n = String.length base_path + 1 in
        String.sub path n (String.length path - n)
      else if
        String.starts_with ~prefix:parent_base_path path
        && parent_base_path <> ""
      then
        let n = String.length parent_base_path + 1 in
        String.sub path n (String.length path - n)
      else path
    in
    (* prerr_endline @@ "LOG[NS]3: '" ^ path ^ "'"; *)
    let path =
      path
      (* |> Str.global_replace (Str.regexp "\\.") "_" *)
      |> Str.global_replace (Str.regexp "/") "."
    in
    (* prerr_endline @@ "LOG4:RESULT: " ^ path; *)
    path *)
end

type ns_opt = { root_dir : string; filename : string }

let handle_require ctx requires =
  let aliases =
    requires
    |> List.concat_map (function
         | SList (_, [ _; SAtom (_, path); _; SAtom (ma, alias) ]) ->
             let path = unpack_string path in
             (* prerr_endline @@ "LOG[NS]3: " ^ alias ^ " -> " ^ path; *)
             [
               SAtom (meta_empty, alias);
               SList
                 ( meta_empty,
                   [
                     SAtom
                       ( ma,
                         NamespaceUtils.convert_path_to_ns ctx.root_dir
                           ctx.filename path );
                     SAtom (ma, path);
                   ] )
               (* |> trace __LOC__ show_sexp2 *);
             ]
         | x -> failsexp __LOC__ [ x ])
  in
  [
    SList
      ( meta_empty,
        [
          SAtom (meta_empty, "def");
          SAtom (meta_empty, "__ns_aliases");
          SList
            ( meta_empty,
              [ SAtom (meta_empty, "quote"); SList (meta_empty, aliases) ] );
        ] );
  ]

let invoke m (ctx : ns_opt) args =
  let args =
    args
    |> List.concat_map (function
         | SList (_, SAtom (_, ":require") :: requires) ->
             handle_require ctx requires
         | SList (_, SAtom (_, ":import") :: imports) ->
             imports
             |> List.concat_map (function
                  | SList (_, _ :: SAtom (_, pkg) :: classes) ->
                      classes
                      |> List.map (function
                           | SAtom (_, class_name) ->
                               SList
                                 ( meta_empty,
                                   [
                                     SAtom (meta_empty, "def*");
                                     SAtom (meta_empty, class_name);
                                     SList
                                       ( meta_empty,
                                         [
                                           SAtom (meta_empty, "quote*");
                                           SAtom
                                             (meta_empty, pkg ^ "." ^ class_name);
                                         ] );
                                   ] )
                           | x -> failsexp __LOC__ [ x ])
                  | x -> failsexp __LOC__ [ x ])
         | x -> failsexp __LOC__ [ x ])
  in
  SList (m, SAtom (meta_empty, "do") :: args)
