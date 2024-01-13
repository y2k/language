let read_text_file filename =
  let channel = open_in filename in
  let size = in_channel_length channel in
  let content = really_input_string channel size in
  close_in channel;
  content

let () =
  let filename = Sys.argv.(1) in
  filename |> read_text_file |> Clj2js.main filename
  |> Printf.sprintf "\"use strict\";\n%s"
  |> print_endline
