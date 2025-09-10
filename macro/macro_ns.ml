open Core__.Common

module NamespaceUtils = struct
  let convert_path_to_ns _ filename path =
    (* prerr_endline @@ "LOG[NS]: base_path: '" ^ "???" ^ "'" ^ " | filename: '"
    ^ filename ^ "'" ^ " | path: '" ^ path ^ "'\n"; *)
    Filename.concat (Filename.dirname filename) (path ^ ".clj")
    (* |> trace __LOC__ Fun.id *)
    |> FileReader.realpath
    (* |> trace __LOC__ Fun.id *)
    |> String.hash
    |> Printf.sprintf "m%i"
  (* |> trace __LOC__ Fun.id *)
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
         | SList (_, [ SAtom (_, "vector"); _; SAtom (_, ":refer"); _ ]) -> []
         | x -> failsexp __LOC__ [ x ])
  in
  match aliases with
  | [] -> []
  | aliases ->
      [
        SList
          ( meta_empty,
            [
              SAtom (meta_empty, "def");
              SAtom (meta_empty, "__ns_aliases");
              SList
                ( meta_empty,
                  [ SAtom (meta_empty, "quote"); SList (meta_empty, aliases) ]
                );
            ] );
      ]

let invoke m (ctx : ns_opt) args =
  (* trace __LOC__ show_sexp2 (SList (meta_empty, args)) |> ignore; *)
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
(* |> trace __LOC__ show_sexp2 *)

let invoke (ctx : Core__.Frontent_simplify.simplify_ctx) simplify = function
  | SList (m, SAtom (_, "ns") :: _ :: args) ->
      args
      |> invoke m { root_dir = ctx.otp.root_dir; filename = ctx.otp.filename }
      |> simplify |> Option.some
  | _ -> None
