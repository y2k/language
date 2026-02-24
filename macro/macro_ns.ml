open Core__.Common

let module_to_path module_name =
  String.split_on_char '.' module_name |> String.concat Filename.dir_sep

let convert_path_to_ns _filename path = "m" ^ string_of_int (String.hash path)

type ns_ctx = { filename : string; namespace : string }

let expand_require ctx requires =
  let aliases =
    requires
    |> List.concat_map (function
      (* New format: [lib.eff :as e] - path is a symbol with dots *)
      | SList (_, [ _; SAtom (_, path); SAtom (_, ":as"); SAtom (ma, alias) ])
        when not (String.starts_with ~prefix:"\"" path) ->
          [
            SAtom (meta_empty, alias);
            SList
              ( meta_empty,
                [
                  SAtom (ma, convert_path_to_ns ctx.filename path);
                  SAtom (ma, path);
                ] );
          ]
      (* Old format: ["./lib/eff" :as e] - path is a quoted string *)
      | SList (_, [ _; SAtom (_, path); SAtom (_, ":as"); SAtom (ma, alias) ])
        ->
          let path2 = unpack_string path in
          [
            SAtom (meta_empty, alias);
            SList
              ( meta_empty,
                [
                  SAtom (ma, convert_path_to_ns ctx.filename path2);
                  SAtom (ma, path2);
                  SAtom (ma, path);
                ] );
          ]
      | SList (_, [ SAtom (_, "vector"); _; SAtom (_, ":refer"); _ ]) -> []
      | x -> failsexp __LOC__ [ x ])
  in
  match aliases with
  | [] -> []
  | _ ->
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

let expand_import imports =
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
                          SAtom (meta_empty, pkg ^ "." ^ class_name);
                        ] );
                  ] )
          | x -> failsexp __LOC__ [ x ])
    | x -> failsexp __LOC__ [ x ])

let expand_require2 requires =
  requires
  |> List.concat_map (function
    | SList (_, [ _; SAtom (mp, path); SAtom (_, ":as"); SAtom (ma, alias) ]) ->
        [
          SAtom
            ( mp,
              if String.starts_with ~prefix:"\"" path then path else ":" ^ path
            );
          SAtom (ma, ":" ^ alias);
        ]
    | x -> failsexp __LOC__ [ x ])

let compute_ns_requires args =
  args
  |> List.concat_map (function
    | SList (_, SAtom (_, ":require") :: requires) -> expand_require2 requires
    | SList (_, SAtom (_, ":import") :: _imports) -> []
    | x -> failsexp __LOC__ [ x ])

let expand_ns m ctx args =
  let expanded_args =
    args
    |> List.concat_map (function
      | SList (_, SAtom (_, ":require") :: requires) ->
          expand_require ctx requires
      | SList (_, SAtom (_, ":import") :: imports) -> expand_import imports
      | x -> failsexp __LOC__ [ x ])
  in
  let ns_def =
    [
      SList
        ( meta_empty,
          [
            SAtom (meta_empty, "def*");
            SAtom (meta_empty, "__namespace");
            SAtom (meta_empty, ":" ^ ctx.namespace);
          ] );
    ]
  in
  let _ns2_def =
    SList
      ( meta_empty,
        [
          SAtom (meta_empty, "def");
          SAtom (meta_empty, "__NS__");
          SList
            ( meta_empty,
              SAtom (meta_empty, "__BUILD_NS__")
              :: SAtom (meta_empty, "\"" ^ ctx.namespace ^ "\"")
              :: compute_ns_requires args );
        ] )
  in
  SList (m, SAtom (meta_empty, "do") :: _ns2_def :: (ns_def @ expanded_args))

let invoke (ctx : Core__.Frontend_simplify.simplify_ctx) simplify = function
  | SList (m, SAtom (_, "ns") :: SAtom (_, namespace) :: args) ->
      expand_ns m { filename = ctx.otp.filename; namespace } args
      |> simplify |> Option.some
  | _ -> None
