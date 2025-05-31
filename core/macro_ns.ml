open Lib__.Common

module NamespaceUtils = struct
  let convert_path_to_ns _base_path _filename _path =
    prerr_endline @@ "Convert path to ns: (" ^ _base_path ^ ") " ^ _filename
    ^ " -> " ^ _path;
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
    (* prerr_endline @@ "LOG1:_base_path: " ^ _base_path;
    prerr_endline @@ "LOG2:_filename: " ^ _filename;
    prerr_endline @@ "LOG3:_path: " ^ _path; *)
    let path = merge_path _filename _path in
    (* prerr_endline @@ "LOG4:RESULT: " ^ path; *)
    let path =
      let n = String.length _base_path + 1 in
      String.sub path n (String.length path - n)
    in
    let path = Str.global_replace (Str.regexp "/") "." path in
    (* prerr_endline @@ "LOG4:RESULT: " ^ path; *)
    path
end

type ns_opt = { root_dir : string; filename : string }

let invoke m (ctx : ns_opt) args =
  let args =
    args
    |> List.concat_map (function
         | SList (_, SAtom (_, ":require") :: requires) ->
             let aliases =
               requires
               |> List.concat_map (function
                    | SList (_, [ _; SAtom (_, path); _; SAtom (ma, alias) ]) ->
                        [
                          SAtom (meta_empty, alias);
                          SAtom
                            ( ma,
                              NamespaceUtils.convert_path_to_ns ctx.root_dir
                                ctx.filename (unpack_string path) );
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
                         [
                           SAtom (meta_empty, "quote");
                           SList
                             ( meta_empty,
                               SAtom (meta_empty, "hash-map") :: aliases );
                         ] );
                   ] );
             ]
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
