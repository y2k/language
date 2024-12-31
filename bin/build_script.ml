module Atomic = struct
  include Atomic

  let rec swap a f =
    let old_v = Atomic.get a in
    if Atomic.compare_and_set a old_v (f old_v) then () else swap a f
end

let reduce_files dir acc f =
  let rec reduce_files2 dir prefix acc f =
    let files = Sys.readdir dir in
    Array.fold_left
      (fun acc file ->
        let full_path = Filename.concat dir file in
        if Sys.is_directory full_path then
          (* Рекурсивно обходим папку *)
          reduce_files2 full_path (Filename.concat prefix file) acc f
        else (* Применяем функцию к файлу *)
          f acc (Filename.concat prefix file))
      acc files
  in
  reduce_files2 dir "" acc f

let filter_source_file path =
  let contains template =
    try
      Str.search_forward (Str.regexp_string template) path 0 |> ignore;
      true
    with Not_found -> false
  in
  contains ".clj"
  && (not (contains "/."))
  && (not (contains "vendor/packages/"))
  && (not (contains "vendor/prelude/"))
  && (not @@ String.starts_with ~prefix:"." path)

let change_extension path ext = Filename.chop_extension path ^ ext

let convert_clj_filename_to_java path =
  match Filename.dirname path with "." -> path | dir -> Filename.basename path |> Filename.concat dir

let get_lib_pkg_name lib file =
  let parts = String.split_on_char '/' lib in
  let a = List.find_index (( = ) "packages") parts |> Option.get in
  let start_pkg = List.nth parts (a + 1) in

  match String.rindex_opt file '/' with
  | None -> start_pkg
  | Some b ->
      let end_pkg = String.sub file 0 b in
      start_pkg ^ "." ^ end_pkg

let make_copy_libs lang target =
  List.concat_map (fun lib ->
      let path_parts = String.split_on_char '/' lib in
      let lib_name = List.nth path_parts (List.length path_parts - 3) in
      let target_lib_path = target ^ "/" ^ lib_name in
      reduce_files lib [] (fun acc file -> file :: acc)
      |> List.concat_map (fun file ->
             (* prerr_endline @@ "LOG: " ^ lib ^ " | " ^ file; *)
             let target_file = Filename.concat target_lib_path (change_extension file ("." ^ lang)) in
             let target_file = Filename.concat (Sys.getcwd ()) target_file in
             [
               Printf.sprintf "mkdir -p %s" (Filename.dirname target_file);
               Printf.sprintf "clj2js %s %s $FULL_PRELUDE_PATH %s > %s" lang (Filename.concat lib file)
                 (get_lib_pkg_name lib file) target_file;
             ]))

let compute_package_name source_path file =
  (* prerr_endline @@ "LOG: " ^ source_path ^ " | " ^ file; *)
  match String.rindex_opt source_path '/' with
  | None -> source_path
  | Some a -> (
      let a = a + 1 in
      let start_pkg = String.sub source_path a (String.length source_path - a) in
      match String.rindex_opt file '/' with
      | None -> start_pkg
      | Some b ->
          let end_pkg = String.sub file 0 b in
          start_pkg ^ "." ^ end_pkg)

let make_build_script_ lang source_path target_path libs =
  (* print_endline @@ path ^ " | " ^ target_path; *)
  let result = reduce_files source_path [] (fun acc file -> if filter_source_file file then file :: acc else acc) in
  List.concat
    [
      make_copy_libs lang target_path libs;
      [ "" ];
      result
      |> List.map (fun file ->
             let target_file =
               Filename.concat
                 (Filename.concat target_path (Filename.basename source_path))
                 (change_extension (convert_clj_filename_to_java file) ("." ^ lang))
             in
             let target_file = Filename.concat (Sys.getcwd ()) target_file in
             (* *)
             let pkg_name = compute_package_name source_path file in
             String.concat "\n"
               [
                 Printf.sprintf "mkdir -p %s" (Filename.dirname target_file);
                 Printf.sprintf "clj2js %s %s $FULL_PRELUDE_PATH %s > %s" lang
                   (Filename.concat (Filename.concat (Sys.getcwd ()) source_path) file)
                   pkg_name target_file;
               ]);
    ]
  |> List.fold_left (Printf.sprintf "%s\n%s")
       (Printf.sprintf
          "#!/bin/bash\n\
           set -e\n\
           set -u\n\
           set -o pipefail\n\n\
           export OCAMLRUNPARAM=b\n\
           export FULL_PRELUDE_PATH=$(realpath $PRELUDE_%s)\n"
          (String.uppercase_ascii lang))
  |> print_endline

let make_build_script () =
  let pathes = Atomic.make [] in
  let target_path = ref "" in
  let libs = ref [] in
  let lang = ref "java" in
  Arg.current := 1;
  Arg.parse
    [
      ("-path", Arg.String (fun x -> Atomic.swap pathes (fun l -> x :: l)), "Path to clj files root");
      ("-lib", Arg.String (fun x -> libs := x :: !libs), "Dependency path");
      ("-target", Arg.Set_string target_path, "Target path");
      ("-lang", Arg.Set_string lang, "Language (default: java, options: java, js)");
    ]
    ignore "";
  List.iter (fun path -> make_build_script_ !lang path !target_path !libs) (Atomic.get pathes)
