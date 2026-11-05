let error message = failwith ("unpack-fex: " ^ message)

let decode text =
  let length = String.length text in
  let invalid () = error "invalid Base64 payload" in
  let digit = function
    | 'A' .. 'Z' as c -> Char.code c - Char.code 'A'
    | 'a' .. 'z' as c -> Char.code c - Char.code 'a' + 26
    | '0' .. '9' as c -> Char.code c - Char.code '0' + 52
    | '+' -> 62
    | '/' -> 63
    | _ -> invalid ()
  in
  if length mod 4 <> 0 then invalid ();
  let output = Buffer.create (length / 4 * 3) in
  let byte n = Buffer.add_char output (Char.chr (n land 255)) in
  for i = 0 to (length / 4) - 1 do
    let p = i * 4 in
    let a = digit text.[p] and b = digit text.[p + 1] in
    byte ((a lsl 2) lor (b lsr 4));
    if text.[p + 2] = '=' then (if p + 4 <> length || text.[p + 3] <> '=' || b land 15 <> 0 then invalid ())
    else
      let c = digit text.[p + 2] in
      byte ((b lsl 4) lor (c lsr 2));
      if text.[p + 3] = '=' then (if p + 4 <> length || c land 3 <> 0 then invalid ())
      else byte ((c lsl 6) lor digit text.[p + 3])
  done;
  Buffer.contents output

let line input =
  match input_line input with
  | text ->
      let length = String.length text in
      Some (if length > 0 && text.[length - 1] = '\r' then String.sub text 0 (length - 1) else text)
  | exception End_of_file -> None

let run ~output_dir input =
  try
    if not (Sys.file_exists output_dir) then Unix.mkdir output_dir 0o755;
    let rec pairs () =
      match line input with
      | None -> ()
      | Some key ->
          if
            key = "" || key = "." || key = ".."
            || not
                 (String.for_all
                    (function 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '-' | '.' | '%' | '!' -> true | _ -> false)
                    key)
          then error "unsafe filename";
          let payload = match line input with Some text -> decode text | None -> error "EOF after key" in
          let path = Filename.concat output_dir key in
          let unchanged =
            match Unix.lstat path with
            | stat when stat.Unix.st_kind <> Unix.S_REG -> error "output file is not a regular file"
            | _ -> In_channel.with_open_bin path In_channel.input_all = payload
            | exception Unix.Unix_error (Unix.ENOENT, _, _) -> false
          in
          if not unchanged then Out_channel.with_open_bin path (fun output -> output_string output payload);
          print_endline key;
          pairs ()
    in
    pairs ();
    Ok ()
  with
  | Failure message | Sys_error message -> Error message
  | Unix.Unix_error (code, operation, path) ->
      Error (Printf.sprintf "%s: %s: %s" operation path (Unix.error_message code))
