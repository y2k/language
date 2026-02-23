open Core__.Common

type deps = (string * string) list

type options = {
  compile :
    deps:deps -> current_dir:string -> module_name:string -> string -> sexp;
  deps : deps;
  current_dir : string;
}

type context = { opt : options }

let module_to_path module_name =
  String.split_on_char '.' module_name |> String.concat Filename.dir_sep

let get_root_module module_name =
  match String.split_on_char '.' module_name with
  | root :: _ -> root
  | [] -> module_name

let resolve_module_path ~deps ~current_dir module_name =
  let relative_path = module_to_path module_name in
  let local_path = Filename.concat current_dir (relative_path ^ ".clj") in
  if FileReader.file_exists local_path then FileReader.realpath local_path
  else
    let root_module = get_root_module module_name in
    match List.assoc_opt root_module deps with
    | Some version ->
        let packages_dir =
          match Sys.getenv_opt "LY2K_PACKAGES_DIR" with
          | Some dir -> dir
          | None -> failwith "LY2K_PACKAGES_DIR environment variable is not set"
        in
        let pkg_path =
          Printf.sprintf "%s/%s/%s/%s.clj" packages_dir root_module version
            relative_path
        in
        if FileReader.file_exists pkg_path then pkg_path
        else
          failwith
            (Printf.sprintf "Module not found in package: %s at %s" module_name
               pkg_path)
    | None ->
        failwith
          (Printf.sprintf "Module not found: %s (checked: %s)" module_name
             local_path)

let extract_deps node =
  let rec find_deps = function
    | SAtom _ -> []
    | SList (_, SAtom (_, "do*") :: children) ->
        List.concat_map find_deps children
    | SList
        ( _,
          SAtom (_, "def*")
          :: SAtom (_, "__deps")
          :: SList (_, [ SAtom (_, "quote*"); SList (_, items) ])
          :: _ ) ->
        items
        |> List.map (function
          | SList (_, [ SAtom (_, name); SAtom (_, version) ]) -> (name, version)
          | x -> failsexp __LOC__ [ x ])
    | SList _ -> []
  in
  find_deps node

let rec invoke (ctx : context) = function
  | SAtom _ as x -> (ctx, x)
  | SList (m, (SAtom (_, "do*") as do_) :: children) ->
      let ctx, children = List.fold_left_map invoke ctx children in
      (ctx, SList (m, do_ :: children))
  | SList
      ( _,
        SAtom (_, "def*")
        :: SAtom (_, "__ns_aliases")
        :: SList (_, [ SAtom (_, "quote*"); SList (_, items) ])
        :: _ ) ->
      let items =
        items |> List.split_into_pairs
        |> List.map (function
          (* New format: 2 elements - [ns_id, module_path] *)
          | _, SList (_, [ _; SAtom (_, module_name) ]) ->
              let resolved_path =
                resolve_module_path ~deps:ctx.opt.deps
                  ~current_dir:ctx.opt.current_dir module_name
              in
              let new_current_dir = Filename.dirname resolved_path in
              ctx.opt.compile ~deps:ctx.opt.deps ~current_dir:new_current_dir
                ~module_name resolved_path
          (* Old format: 3 elements - [ns_id, path_unquoted, path_quoted] *)
          | _, SList (_, [ _; SAtom (_, path); _ ]) ->
              let resolved_path =
                resolve_module_path ~deps:ctx.opt.deps
                  ~current_dir:ctx.opt.current_dir path
              in
              let new_current_dir = Filename.dirname resolved_path in
              ctx.opt.compile ~deps:ctx.opt.deps ~current_dir:new_current_dir
                ~module_name:path resolved_path
          | k, v -> failsexp __LOC__ [ k; v ])
      in
      (ctx, SList (meta_empty, SAtom (meta_empty, "do*") :: items))
  | SList (_, SAtom (_, "def*") :: _) as x -> (ctx, x)
  | SList (_, SAtom (_, n) :: _) as x
    when String.ends_with ~suffix:"*" n && n <> "*" ->
      failsexp __LOC__ [ x ]
  | SList (_, _ :: _) as x -> (ctx, x)
  | x -> failsexp __LOC__ [ x ]

let do_invoke ~deps ~current_dir compile (node : sexp) : sexp =
  invoke { opt = { compile; deps; current_dir } } node |> snd
