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

let make_build_script path target_path =
  let result =
    reduce_files path [] (fun acc file ->
        if filter_source_file file then file :: acc else acc)
  in
  result
  |> List.map (fun file ->
         let target_file =
           Filename.concat target_path
             (change_extension (convert_clj_filename_to_java file) ".java")
         in
         (* *)
         let create_dir =
           Printf.sprintf "mkdir -p %s" (Filename.dirname target_file)
         in
         Printf.sprintf "%s\nclj2js java %s $FULL_PRELUDE_JAVA > %s" create_dir
           (Filename.concat path file)
           target_file)
  |> List.fold_left (Printf.sprintf "%s\n%s")
       (Printf.sprintf
          "#!/bin/bash\n\
           set -e\n\
           set -u\n\
           set -o pipefail\n\n\
           export FULL_PRELUDE_JAVA=$(realpath $PRELUDE_JAVA)\n")
  |> print_endline
