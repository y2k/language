open Lib__.Common
module Clj2js = Lib
module Bs = Build_script

let read_code_file filename = if filename = "prelude" then "" else In_channel.(with_open_bin filename input_all)

let compile_file filename target root_ns =
  prerr_endline @@ "Compile: " ^ target ^ " | " ^ filename;
  NameGenerator.with_scope (fun _ ->
      let compiler =
        match target with
        | "js" -> Clj2js.main_js_with_strict false filename
        | "java" -> Clj2js.main_java root_ns false filename
        | "bytecode" -> Clj2js.main_bytecode false filename
        | "repl" -> Clj2js.main_interpreter false filename
        | t -> failwith @@ "Invalid target " ^ t
      in
      filename |> read_code_file |> FileReader.with_scope compiler |> print_endline)

let compile_file_old () =
  let target = Sys.argv.(1) in
  let filename = Sys.argv.(2) in
  let root_ns = match Sys.argv with [| _; _; _; _; x |] -> x | _ -> "" in
  compile_file filename target root_ns

let main () =
  let target = ref "" in
  let src = ref "" in
  let command = ref "" in
  let root_ns = ref "" in
  Arg.parse
    [
      ("-target", Arg.Set_string target, "Target: js, java, repl, bytecode");
      ("-src", Arg.Set_string src, "Source file");
      ("-root_ns", Arg.Set_string root_ns, "Root namespace");
      ("-lang", Arg.String ignore, "Deprecated");
      ("-lib", Arg.String ignore, "Deprecated");
      ("-path", Arg.String ignore, "Deprecated");
    ]
    (( := ) command) "clj2js";
  match !command with
  | "compile" -> compile_file !src !target !root_ns
  | "make_build_script" -> Bs.make_build_script ()
  | _ -> compile_file_old ()

let () = main ()
