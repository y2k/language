open Ast

let position_at input offset =
  let rec loop index line column =
    if index >= offset then { line; column }
    else match input.[index] with '\n' -> loop (index + 1) (line + 1) 1 | _ -> loop (index + 1) line (column + 1)
  in
  loop 0 1 1

let make_meta input start_offset = { loc = position_at input start_offset; type_annotation = None }

let parse_string input =
  let open Angstrom in
  let whitespace = function ' ' | '\n' | '\r' | '\t' -> true | _ -> false in
  let delimiter = function '(' | ')' | '[' | ']' | '{' | '}' | '"' | '\'' | ';' -> true | c -> whitespace c in
  let whitespace_run = satisfy whitespace *> skip_while whitespace in
  let comment = char ';' *> skip_while (fun c -> c <> '\n') in
  let ignored = skip_many (whitespace_run <|> comment) in
  let with_loc parser =
    pos >>= fun start_offset ->
    parser >>| fun value -> (make_meta input start_offset, value)
  in
  let symbol = with_loc (take_while1 (fun c -> not (delimiter c))) >>| fun (meta, value) -> SAtom (meta, value) in
  let quoted_string =
    let string_char =
      char '\\' *> any_char >>| (fun c -> Printf.sprintf "\\%c" c) <|> (satisfy (fun c -> c <> '"') >>| String.make 1)
    in
    with_loc (char '"' *> many string_char <* char '"') >>| fun (meta, parts) ->
    SAtom (meta, "\"" ^ String.concat "" parts ^ "\"")
  in
  let sexpr =
    fix (fun sexpr ->
        let delimited_list opening closing bracket =
          with_loc (char opening *> ignored *> many (sexpr <* ignored) <* char closing) >>| fun (meta, items) ->
          SList (meta, bracket, items)
        in
        let quoted =
          with_loc (char '\'' *> ignored *> sexpr) >>| fun (meta, item) ->
          SList (meta, Paren, [ SAtom (meta, "quote"); item ])
        in
        let list =
          choice [ delimited_list '(' ')' Paren; delimited_list '[' ']' Bracket; delimited_list '{' '}' Brace ]
        in
        let annotated =
          char '^' *> take_while1 (fun c -> not (delimiter c)) <* ignored >>= fun type_annotation ->
          sexpr >>| function
          | SAtom (meta, value) -> SAtom ({ meta with type_annotation = Some type_annotation }, value)
          | SList (meta, bracket, items) -> SList ({ meta with type_annotation = Some type_annotation }, bracket, items)
        in
        ignored *> (list <|> quoted <|> quoted_string <|> annotated <|> symbol))
  in
  Angstrom.parse_string ~consume:Consume.All (ignored *> many (sexpr <* ignored)) input
