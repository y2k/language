module Clj2js = Lib
open Lib__.Common

let read_code_file filename =
  if filename = "prelude" then ""
  else In_channel.(with_open_bin filename input_all)

let () =
  let target = Sys.argv.(1) in
  let filename = Sys.argv.(2) in
  NameGenerator.with_scope (fun _ ->
      let compiler =
        match target with
        | "js" ->
            let prelude_macros = read_code_file Sys.argv.(3) in
            fun code ->
              Lib__Linter.run_resolve
                (fun name ->
                  let path =
                    Filename.concat (Filename.dirname filename) (name ^ ".clj")
                  in
                  (* prerr_endline @@ Sys.getenv "PWD" ^ " | " ^ filename; *)
                  In_channel.(with_open_bin path input_all))
                (fun _ ->
                  Clj2js.main_js_with_strict false filename prelude_macros code)
        | "java" ->
            let prelude = read_code_file Sys.argv.(3) in
            Clj2js.main_java false filename prelude
        | "repl" ->
            let prelude = read_code_file Sys.argv.(3) in
            Clj2js.main_interpreter filename prelude
        | t -> failwith @@ "Invalid target " ^ t
      in
      filename |> read_code_file |> compiler |> print_endline)
