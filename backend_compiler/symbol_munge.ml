let replacement = function
  | '-' | ' ' -> Some "_"
  | ':' -> Some "_COLON_"
  | '+' -> Some "_PLUS_"
  | '>' -> Some "_GT_"
  | '<' -> Some "_LT_"
  | '=' -> Some "_EQ_"
  | '~' -> Some "_TILDE_"
  | '!' -> Some "_BANG_"
  | '@' -> Some "_CIRCA_"
  | '#' -> Some "_SHARP_"
  | '\'' -> Some "_SINGLEQUOTE_"
  | '"' -> Some "_DOUBLEQUOTE_"
  | '%' -> Some "_PERCENT_"
  | '^' -> Some "_CARET_"
  | '&' -> Some "_AMPERSAND_"
  | '*' -> Some "_STAR_"
  | '|' -> Some "_BAR_"
  | '{' -> Some "_LBRACE_"
  | '}' -> Some "_RBRACE_"
  | '[' -> Some "_LBRACK_"
  | ']' -> Some "_RBRACK_"
  | '\\' -> Some "_BSLASH_"
  | '?' -> Some "_QMARK_"
  | _ -> None

let munge name =
  if name = "-" then "_MINUS_"
  else if name = "/" then "_SLASH_"
  else
    let output = Buffer.create (String.length name) in
    String.iter
      (fun char ->
        match replacement char with Some value -> Buffer.add_string output value | None -> Buffer.add_char output char)
      name;
    Buffer.contents output
