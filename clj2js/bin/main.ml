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
    |> (fun (code, imports) ->
         Printf.sprintf "\"use strict\";\n%s\n%s" imports code)
    |> print_endline
  else
    let target = Sys.argv.(0) in
    let filename = Sys.argv.(2) in
    filename |> read_text_file
    |> (if target = "js" then Clj2js.main filename else Clj2js.main_kt filename)
    |> print_endline
