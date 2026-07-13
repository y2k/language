let string_literal value =
  let output = Buffer.create (String.length value + 2) in
  Buffer.add_char output '"';
  String.iter
    (function
      | '"' -> Buffer.add_string output "\\\""
      | '\\' -> Buffer.add_string output "\\\\"
      | '\n' -> Buffer.add_string output "\\n"
      | '\r' -> Buffer.add_string output "\\r"
      | '\t' -> Buffer.add_string output "\\t"
      | c when Char.code c < 0x20 -> Buffer.add_string output (Printf.sprintf "\\u%04x" (Char.code c))
      | c -> Buffer.add_char output c)
    value;
  Buffer.add_char output '"';
  Buffer.contents output
