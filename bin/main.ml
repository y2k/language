open Lib__.Common
module Clj2js = Lib

let read_code_file filename = if filename = "prelude" then "" else In_channel.(with_open_bin filename input_all)

let compile_file filename target root_ns =
  prerr_endline @@ "Compile: [" ^ Sys.getcwd () ^ "] " ^ target ^ " | " ^ filename;
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
  | "gen" -> print_endline @@ Lib__.Preludes.java_runtime
  | "compile" -> compile_file !src !target !root_ns
  | n -> failwith ("Invalid command " ^ n ^ " (" ^ Sys.getcwd () ^ ")")

let () = main ()
