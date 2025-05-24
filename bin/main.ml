open Lib__.Common
module Clj2js = Lib

let read_code_file filename =
  if filename = "prelude" then ""
  else if filename = "@stdin" then In_channel.input_all stdin
  else In_channel.(with_open_bin filename input_all)

let compile_file filename target root_ns no_lint virtual_src =
  prerr_endline @@ "Compile: [" ^ Sys.getcwd () ^ "] " ^ target ^ " | " ^ filename;
  NameGenerator.with_scope (fun _ ->
      let compiler =
        match target with
        | "js" -> Clj2js.main_js_with_strict false filename
        | "java" -> Clj2js.main_java root_ns false filename
        | "bytecode" ->
            Clj2js.main_bytecode
              { config_default with no_lint; virtual_src }
              (if virtual_src <> "" then virtual_src else filename)
        | "bytecode_repl" ->
            Clj2js.main_bytecode
              { config_default with no_lint; virtual_src; no_deps = true }
              (if virtual_src <> "" then virtual_src else filename)
        | "repl" -> Clj2js.main_interpreter false filename
        | t -> failwith @@ "Invalid target " ^ t
      in
      filename |> read_code_file |> FileReader.with_scope compiler |> print_endline)

let get_namespace filename : string =
  let module F = Lib__.Frontend_parser in
  let code = In_channel.(with_open_bin filename input_all) in
  match F.string_to_cjexp code with
  | (RBList (_, Atom (_, "ns") :: _) as ns) :: _ -> show_sexp ns
  | n -> failnode __LOC__ n

let _main () =
  let target = ref "" in
  let src = ref "" in
  let command = ref "" in
  let root_ns = ref "" in
  let no_lint = ref false in
  let virtual_src = ref "" in
  let client_host = ref "" in
  Arg.parse
    [
      ("-target", Arg.Set_string target, "Target: js, java, repl, bytecode");
      ("-src", Arg.Set_string src, "Source file (use @stdin for standard input)");
      ("-root_ns", Arg.Set_string root_ns, "Root namespace");
      ("-no_lint", Arg.Bool (( := ) no_lint), "Disable linting");
      ("-virtual_src", Arg.Set_string virtual_src, "Virtual source");
      ("-host", Arg.Set_string client_host, "Client host");
      ("-lang", Arg.String ignore, "Deprecated");
      ("-lib", Arg.String ignore, "Deprecated");
      ("-path", Arg.String ignore, "Deprecated");
    ]
    (( := ) command) "clj2js";
  match !command with
  | "nrepl" -> Nrepl.start !client_host
  | "get_namespace" -> print_endline @@ get_namespace !src
  | "gen" -> print_endline @@ Lib__.Preludes.java_runtime
  | "compile" -> compile_file !src !target !root_ns !no_lint !virtual_src
  | n -> failwith ("Invalid command '" ^ n ^ "' (" ^ Sys.getcwd () ^ ")")

let () = Main2.main ()
