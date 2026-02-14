(* Prelude module - dynamically loads prelude files from $LY2K_PACKAGES_DIR *)

let get_packages_dir () =
  match Sys.getenv_opt "LY2K_PACKAGES_DIR" with
  | Some dir -> dir
  | None -> failwith "LY2K_PACKAGES_DIR environment variable is not set"

let read_file path =
  try In_channel.(with_open_bin path input_all)
  with Sys_error msg ->
    failwith (Printf.sprintf "Failed to read prelude file %s: %s" path msg)

let prelude_base_path () = Filename.concat (get_packages_dir ()) "prelude/1.0.0"

(* Java *)
let prelude_java_macro =
  lazy
    (let base = prelude_base_path () in
     read_file (Filename.concat base "java/prelude.macro"))

let java_runtime =
  lazy
    (let base = prelude_base_path () in
     read_file (Filename.concat base "java/RT.java"))

let java_runtime2 =
  lazy
    (let base = prelude_base_path () in
     read_file (Filename.concat base "java/prelude_java.java"))

(* JS *)
let prelude_js_macro =
  lazy
    (let base = prelude_base_path () in
     read_file (Filename.concat base "js/prelude.macro"))

let js_runtime =
  lazy
    (let base = prelude_base_path () in
     read_file (Filename.concat base "js/rt.js"))

(* Eval *)
let prelude_eval_macro =
  lazy
    (let base = prelude_base_path () in
     read_file (Filename.concat base "eval/prelude.macro"))

let prelude_eval =
  lazy
    (let base = prelude_base_path () in
     read_file (Filename.concat base "eval/prelude.clj"))
