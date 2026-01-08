open Core__.Common
open Core__
open Backend__

type config = {
  command : string;
  target : string;
  src : string;
  namespace : string;
  log : bool;
  prelude_path : string;
}

let parse_args () =
  let command = ref "" in
  let target = ref "" in
  let src = ref "" in
  let namespace = ref "" in
  let log = ref false in
  let prelude_path = ref "" in
  let speclist =
    [
      ("-target", Arg.Set_string target, "Target: js, java, java_v2, eval, sexp");
      ("-src", Arg.Set_string src, "Source file");
      ("-namespace", Arg.Set_string namespace, "Namespace");
      ("-log", Arg.Bool (( := ) log), "Show log");
      ("-prelude_path", Arg.Set_string prelude_path, "Prelude path");
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
  }

let read_source src = In_channel.(with_open_bin src input_all)
let with_file_scope f = FileReader.with_scope (fun _ -> f ()) ()

let compile_source cfg =
  let code = read_source cfg.src in
  let result =
    match cfg.target with
    | "sexp_legacy" ->
        Backend_sexp.invoke ~builtin_macro:Macro.invoke ~log:cfg.log code
    | "sexp" ->
        Backend_sexp2.invoke_to_line ~builtin_macro:Macro.invoke ~log:cfg.log
          code ~filename:cfg.src
    | "java" ->
        Backend_java.compile ~builtin_macro:Macro.invoke
          ~namespace:cfg.namespace cfg.log cfg.src code
    | "java_v2" ->
        Backend_java_v2.compile ~builtin_macro:Macro.invoke
          ~namespace:cfg.namespace ~log:cfg.log ~filename:cfg.src code
    | "js" ->
        Backend_js.compile ~builtin_macro:Macro.invoke ~log:cfg.log code
          ~filename:cfg.src ~prelude_path:cfg.prelude_path
    | "eval" | "repl" ->
        Backend_eval.invoke ~builtin_macro:Macro.invoke cfg.log cfg.src code
    | t -> failwith @@ "Invalid target: " ^ t
  in
  print_endline result

let run_generate target =
  match target with
  | "java" -> print_endline Prelude.java_runtime
  | "java_prelude" -> print_endline Prelude.java_runtime2
  | "java_prelude_v2" -> print_endline Prelude.java_runtime2_v2
  | "js" -> print_endline Prelude.js_runtime
  | t -> failwith @@ "Invalid generate target: " ^ t

let () =
  let cfg = parse_args () in
  match cfg.command with
  | "generate" -> run_generate cfg.target
  | _ -> with_file_scope (fun () -> compile_source cfg)
