open Core__.Common
open Core__

let () =
  let command = ref "" in
  let target = ref "" in
  let src = ref "" in
  let capture_stdin = ref false in
  let namespace = ref "" in
  let root_dir = ref "" in
  let log = ref false in
  Arg.parse
    [
      ("-target", Arg.Set_string target, "Target: js, java, eval, bytecode");
      ("-src", Arg.Set_string src, "Source file (use :stdin for standard input)");
      ("-capture_stdin", Arg.Bool (( := ) capture_stdin), "Capture stdin");
      ("-namespace", Arg.Set_string namespace, "Namespace");
      ("-root", Arg.Set_string root_dir, "Root directory");
      ("-log", Arg.Bool (( := ) log), "Show log");
    ]
    (( := ) command) "ly2k";
  match !command with
  | "generate" -> (
      match !target with
      | "java" -> print_endline @@ Prelude.java_runtime
      | _ -> failwith @@ "Invalid target " ^ !target)
  | _ -> (
      let code _ = In_channel.(with_open_bin !src input_all) in
      match !target with
      | "sexp" ->
          FileReader.with_scope
            (fun _ -> Backend_sexp.invoke ~log:!log (code ()) |> print_endline)
            ()
      | "java" ->
          FileReader.with_scope
            (fun _ ->
              Backend_java.compile !namespace !log !src !root_dir (code ())
              |> print_endline)
            ()
      | "js" ->
          FileReader.with_scope
            (fun _ ->
              Backend_js.compile ~log:!log (code ()) ~filename:!src
              |> print_endline)
            ()
      | "eval" | "repl" ->
          FileReader.with_scope
            (fun () ->
              let input =
                if !capture_stdin then In_channel.(with_open_bin !src input_all)
                else ""
              in
              Backend_eval.eval2 !log !src input (code ()) |> print_endline)
            ()
      | t -> failwith @@ "Invalid target " ^ t)
