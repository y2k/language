module Clj2js = Lib

let read_code_file filename =
  if filename = "prelude" then ""
  else In_channel.with_open_bin filename In_channel.input_all

let () =
  let target = Sys.argv.(1) in
  let filename = Sys.argv.(2) in
  let compiler =
    match target with
    | "json" -> Clj2js.main_json filename
    | "js" -> fun code -> Clj2js.main_js filename code |> snd
    | "sh" ->
        fun str ->
          let shebang = "#!/usr/bin/env clj2sh\n" in
          let str =
            if String.starts_with ~prefix:shebang str then
              String.sub str (String.length shebang)
                (String.length str - String.length shebang)
            else str
          in
          "set -o xtrace\nset -e\n\n" ^ (Clj2js.main_sh filename) str
    | "kt" -> Clj2js.main_kt filename
    | "java" ->
        let prelude = read_code_file Sys.argv.(3) in
        Clj2js.main_java filename prelude
    | t -> failwith @@ "Invalid target " ^ t
  in
  filename |> read_code_file |> compiler |> print_endline
