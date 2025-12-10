open Core__.Common
open Core__

let () =
  let command = ref "" in
  let target = ref "" in
  let src = ref "" in
  let namespace = ref "" in
  let log = ref false in
  let prelude_path = ref "" in
  let speclist =
    [
      ("-target", Arg.Set_string target, "Target: js, java, eval, bytecode");
      ("-src", Arg.Set_string src, "Source file (use :stdin for standard input)");
      ("-namespace", Arg.Set_string namespace, "Namespace");
      ("-log", Arg.Bool (( := ) log), "Show log");
      ("-prelude_path", Arg.Set_string prelude_path, "Prelude path");
    ]
  in
  (try Arg.parse_argv Sys.argv speclist (( := ) command) "ly2k"
   with Arg.Bad msg | Arg.Help msg ->
     print_string msg;
     exit 1);
  match !command with
  | "generate" -> (
      match !target with
      | "java" -> print_endline @@ Prelude.java_runtime
      | "java_prelude" -> print_endline @@ Prelude.java_runtime2
      | _ -> failwith @@ "Invalid target " ^ !target)
  | _ -> (
      let code _ = In_channel.(with_open_bin !src input_all) in
      match !target with
      | "sexp_legacy" ->
          FileReader.with_scope
            (fun _ ->
              Backend_sexp.invoke ~builtin_macro:Macro.invoke ~log:!log
                (code ())
              |> print_endline)
            ()
      | "sexp" ->
          FileReader.with_scope
            (fun _ ->
              Backend_sexp2.invoke_to_line ~builtin_macro:Macro.invoke ~log:!log
                (code ()) ~filename:!src
              |> print_endline)
            ()
      | "java" ->
          FileReader.with_scope
            (fun _ ->
              Backend_java.compile ~builtin_macro:Macro.invoke
                ~namespace:!namespace !log !src (code ())
              |> print_endline)
            ()
      | "js" ->
          FileReader.with_scope
            (fun _ ->
              Backend_js.compile ~builtin_macro:Macro.invoke ~log:!log (code ())
                ~filename:!src ~prelude_path:!prelude_path
              |> print_endline)
            ()
      | "eval" | "repl" ->
          FileReader.with_scope
            (fun () ->
              Backend_eval.invoke ~builtin_macro:Macro.invoke !log !src
                (code ())
              |> print_endline)
            ()
      | t -> failwith @@ "Invalid target " ^ t)
