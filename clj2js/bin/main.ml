let read_text_file filename =
  let channel = open_in filename in
  let size = in_channel_length channel in
  let content = really_input_string channel size in
  close_in channel;
  content

let () =
  if Array.length Sys.argv <= 2 then
    (match Sys.argv.(1) with
    | "prelude" -> (Clj2js.prelude |> Clj2js.main "prelude", "")
    | filename ->
        ( filename |> read_text_file |> Clj2js.main filename,
          Clj2js.prelude_imports ))
    |> (fun ((_, code), imports) ->
         Printf.sprintf "\"use strict\";\n%s\n%s" imports code)
    |> print_endline
  else
    let target = Sys.argv.(1) in
    let filename = Sys.argv.(2) in
    filename |> read_text_file
    |> (match target with
       | "json" -> Clj2js.main_json filename
       | "js" -> fun code -> Clj2js.main filename code |> snd
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
       | t -> failwith @@ "Invalid target " ^ t)
    |> print_endline
