open Core__.Common
open Backend__

type config = {
  command : string;
  target : string;
  src : string;
  namespace : string;
  log : bool;
  prelude_path : string;
  output : string;
}

let parse_args () =
  let command = ref "" in
  let target = ref "" in
  let src = ref "" in
  let namespace = ref "" in
  let log = ref false in
  let prelude_path = ref "" in
  let output = ref "" in
  let speclist =
    [
      ("-target", Arg.Set_string target, "Target: js, java_v2, eval, sexp");
      ("-src", Arg.Set_string src, "Source file");
      ("-namespace", Arg.Set_string namespace, "Namespace");
      ("-log", Arg.Bool (( := ) log), "Show log");
      ("-prelude_path", Arg.Set_string prelude_path, "Prelude path");
      ("-output", Arg.Set_string output, "Output file path");
    ]
  in
  let usage = Sys.argv.(0) ^ " [options]" in
  if Array.length Sys.argv = 1 then (
    Arg.usage speclist usage;
    exit 0);
  (try Arg.parse_argv Sys.argv speclist (( := ) command) usage
   with Arg.Bad msg | Arg.Help msg ->
     prerr_endline msg;
     exit 1);
  {
    command = !command;
    target = !target;
    src = !src;
    namespace = !namespace;
    log = !log;
    prelude_path = !prelude_path;
    output = !output;
  }

let read_source src = In_channel.(with_open_bin src input_all)
let with_file_scope f = FileReader.with_scope (fun _ -> f ()) ()

let compile_source cfg =
  let code = read_source cfg.src in
  match cfg.target with
  | "sexp_legacy" ->
      let result =
        Backend_sexp.invoke ~builtin_macro:Macro.invoke ~log:cfg.log code
      in
      print_endline result
  | "sexp" ->
      if cfg.output = "" then
        failwith "Output directory required for sexp target";
      let output_dir = Filename.dirname cfg.output in
      Backend_sexp2.save_to_directory ~builtin_macro:Macro.invoke ~log:cfg.log
        code ~filename:cfg.src ~directory:output_dir;
      print_endline "STUB"
  | "java" | "java_v2" ->
      let result =
        Backend_java.compile ~builtin_macro:Macro__.Macro_v2.invoke
          ~namespace:cfg.namespace ~log:cfg.log ~filename:cfg.src code
      in
      print_endline result
  | "js" ->
      let result =
        Backend_js.compile ~builtin_macro:Macro.invoke ~log:cfg.log code
          ~filename:cfg.src ~prelude_path:cfg.prelude_path
      in
      print_endline result
  | "eval" | "repl" ->
      let result =
        Backend_eval.invoke ~builtin_macro:Macro.invoke cfg.log cfg.src code
      in
      print_endline result
  | t -> failwith @@ "Invalid target: " ^ t

let () =
  let cfg = parse_args () in
  match cfg.command with _ -> with_file_scope (fun () -> compile_source cfg)
