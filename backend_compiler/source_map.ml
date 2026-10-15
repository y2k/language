type mapping = {
  source : string;
  source_file : string;
  line_starts : int array;
  mutable line : int;
  mutable column : int;
  mutable after_cr : bool;
  mutable origin : (int * int) option;
  mutable last_origin : (int * int) option option;
  mutable segments : (int * int * (int * int) option) list;
}

type t = { buffer : Buffer.t; mapping : mapping option }

let create ?source ?(source_file = "user.clj") () =
  let mapping =
    Option.map
      (fun source ->
        let starts = ref [ 0 ] in
        String.iteri (fun i c -> if c = '\n' then starts := (i + 1) :: !starts) source;
        {
          source;
          source_file;
          line_starts = Array.of_list (List.rev !starts);
          line = 0;
          column = 0;
          after_cr = false;
          origin = None;
          last_origin = None;
          segments = [];
        })
      source
  in
  { buffer = Buffer.create 256; mapping }

let utf16_width uchar = if Uchar.to_int uchar > 0xffff then 2 else 1

let source_position mapping (loc : Frontend.loc) =
  let start = mapping.line_starts.(loc.line - 1) in
  let stop = start + loc.column - 1 in
  let rec count offset column =
    if offset >= stop then column
    else
      let decoded = String.get_utf_8_uchar mapping.source offset in
      count (offset + Uchar.utf_decode_length decoded) (column + utf16_width (Uchar.utf_decode_uchar decoded))
  in
  (loc.line - 1, count start 0)

let with_origin writer origin emit =
  match writer.mapping with
  | None -> emit ()
  | Some mapping ->
      let previous = mapping.origin in
      mapping.origin <- Option.map (source_position mapping) origin;
      Fun.protect ~finally:(fun () -> mapping.origin <- previous) emit

let write writer text =
  Buffer.add_string writer.buffer text;
  match writer.mapping with
  | None -> ()
  | Some mapping ->
      let newline () =
        mapping.line <- mapping.line + 1;
        mapping.column <- 0;
        mapping.last_origin <- None
      in
      let rec scan offset =
        if offset < String.length text then (
          let decoded = String.get_utf_8_uchar text offset in
          let uchar = Uchar.utf_decode_uchar decoded in
          let code = Uchar.to_int uchar in
          (match code with
          | 0x0a -> if not mapping.after_cr then newline ()
          | 0x0d | 0x2028 | 0x2029 -> newline ()
          | _ ->
              if mapping.last_origin <> Some mapping.origin then (
                mapping.segments <- (mapping.line, mapping.column, mapping.origin) :: mapping.segments;
                mapping.last_origin <- Some mapping.origin);
              mapping.column <- mapping.column + utf16_width uchar);
          mapping.after_cr <- code = 0x0d;
          scan (offset + Uchar.utf_decode_length decoded))
      in
      scan 0

let alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"

let vlq buffer value =
  let rec emit value =
    let digit = value land 31 in
    let rest = value lsr 5 in
    Buffer.add_char buffer alphabet.[digit lor if rest = 0 then 0 else 32];
    if rest <> 0 then emit rest
  in
  emit (if value < 0 then (-value lsl 1) lor 1 else value lsl 1)

let mappings mapping =
  let output = Buffer.create 128 in
  let line = ref 0 and column = ref 0 and source_line = ref 0 and source_column = ref 0 in
  let first = ref true in
  List.iter
    (fun (next_line, next_column, origin) ->
      while !line < next_line do
        Buffer.add_char output ';';
        incr line;
        column := 0;
        first := true
      done;
      if not !first then Buffer.add_char output ',';
      first := false;
      vlq output (next_column - !column);
      column := next_column;
      match origin with
      | None -> ()
      | Some (next_source_line, next_source_column) ->
          vlq output 0;
          vlq output (next_source_line - !source_line);
          vlq output (next_source_column - !source_column);
          source_line := next_source_line;
          source_column := next_source_column)
    (List.rev mapping.segments);
  (* Node needs a separator after a final unmapped segment to recognize it. *)
  Buffer.add_char output ';';
  Buffer.contents output

let base64 text =
  let output = Buffer.create ((String.length text + 2) / 3 * 4) in
  let byte i = if i < String.length text then Char.code text.[i] else 0 in
  let rec emit i =
    if i < String.length text then (
      let a = byte i and b = byte (i + 1) and c = byte (i + 2) in
      Buffer.add_char output alphabet.[a lsr 2];
      Buffer.add_char output alphabet.[((a land 3) lsl 4) lor (b lsr 4)];
      Buffer.add_char output (if i + 1 < String.length text then alphabet.[((b land 15) lsl 2) lor (c lsr 6)] else '=');
      Buffer.add_char output (if i + 2 < String.length text then alphabet.[c land 63] else '=');
      emit (i + 3))
  in
  emit 0;
  Buffer.contents output

let source_url path =
  let output = Buffer.create (String.length path) in
  String.iter
    (function
      | '#' -> Buffer.add_string output "%23"
      | '?' -> Buffer.add_string output "%3F"
      | '%' -> Buffer.add_string output "%25"
      | c -> Buffer.add_char output c)
    path;
  Buffer.contents output

let finish writer =
  let code = Buffer.contents writer.buffer in
  match writer.mapping with
  | None -> code
  | Some mapping ->
      let json =
        Printf.sprintf {|{"version":3,"sources":[%s],"sourcesContent":[%s],"names":[],"mappings":%s}|}
          (Target_syntax.string_literal (source_url mapping.source_file))
          (Target_syntax.string_literal mapping.source)
          (Target_syntax.string_literal (mappings mapping))
      in
      code ^ "\n//# sourceMappingURL=data:application/json;base64," ^ base64 json
