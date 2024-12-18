(* /Users/igor/Projects/interpreter *)

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
  match Filename.dirname path with
  | "." -> String.capitalize_ascii path
  | dir ->
      Filename.basename path |> String.capitalize_ascii |> Filename.concat dir

let make_copy_libs target libs =
  libs
  |> List.concat_map (fun lib ->
         let path_parts = String.split_on_char '/' lib in
         let lib_name = List.nth path_parts (List.length path_parts - 3) in
         let target_lib_path = target ^ "/" ^ lib_name in
         let complie_commands =
           reduce_files lib [] (fun acc file -> file :: acc)
           |> List.map (fun file ->
                  Printf.sprintf "clj2js java %s $FULL_PRELUDE_JAVA > %s"
                    (Filename.concat lib file)
                    (Filename.concat target_lib_path
                       (change_extension file ".java")))
         in
         [
           Printf.sprintf "mkdir -p %s" target_lib_path;
           (* Printf.sprintf "cp -r %s %s" lib target_lib_path; *)
         ]
         @ complie_commands)

let make_build_script_ source_path target_path libs =
  (* print_endline @@ path ^ " | " ^ target_path; *)
  let result =
    reduce_files source_path [] (fun acc file ->
        if filter_source_file file then file :: acc else acc)
  in
  List.concat
    [
      make_copy_libs target_path libs;
      [ "" ];
      result
      |> List.map (fun file ->
             let target_file =
               Filename.concat
                 (Filename.concat target_path (Filename.basename source_path))
                 (change_extension (convert_clj_filename_to_java file) ".java")
             in
             (* *)
             let create_dir =
               Printf.sprintf "mkdir -p %s" (Filename.dirname target_file)
             in
             Printf.sprintf "%s\nclj2js java %s $FULL_PRELUDE_JAVA > %s"
               create_dir
               (Filename.concat source_path file)
               target_file);
    ]
  |> List.fold_left (Printf.sprintf "%s\n%s")
       (Printf.sprintf
          "#!/bin/bash\n\
           set -e\n\
           set -u\n\
           set -o pipefail\n\n\
           export FULL_PRELUDE_JAVA=$(realpath $PRELUDE_JAVA)/prelude.clj\n")
  |> print_endline

let make_build_script () =
  let path = ref "" in
  let target_path = ref "" in
  let libs = ref [] in
  Arg.parse
    [
      ("-path", Arg.Set_string path, "Path to clj files root");
      ("-lib", Arg.String (fun x -> libs := x :: !libs), "Dependency path");
      ("-target", Arg.Set_string target_path, "Target path");
    ]
    ignore "";
  make_build_script_ !path !target_path !libs
