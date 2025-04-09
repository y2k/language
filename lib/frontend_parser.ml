open Angstrom
open Common
module A = Angstrom

let pnode find_line_and_pos =
  let make_meta pos =
    let line, pos = find_line_and_pos pos in
    { line; pos; symbol = "" }
  in
  let pcomment = (A.string ";" <|> A.string "#!") *> A.take_while (( <> ) '\n') <* A.many (A.char '\n') in
  let pspace = A.many (A.char ' ' <|> A.char '\n' <|> (pcomment >>| fun _ -> ' ')) in
  let patom =
    A.both A.pos (A.take_while1 (function ' ' | '\n' | '[' | ']' | '(' | ')' | '{' | '}' -> false | _ -> true))
    >>| fun (pos, x) -> (make_meta pos, x)
  in
  let patom_ = patom >>| fun (m, a) -> Atom (m, a) in
  let ptext_ =
    A.char '"'
    *> ( A.many1 (A.char '\\' *> A.char '"' >>| Fun.const "\\\"" <|> (A.satisfy (( <> ) '"') >>| String.make 1))
       >>| fun x -> String.concat "" x )
    <* A.char '"'
  in
  let pmeta =
    A.char '^'
    *> (ptext_
       <|> A.take_while1 (fun x ->
               (x >= 'A' && x <= 'z') || x = '.' || x = '(' || x = ')' || x = '-' || x = '>' || x = ':' || x = '?'))
  in
  let patom_meta = A.map2 (pmeta <* pspace) patom ~f:(fun m (a, x) -> Atom ({ a with symbol = m }, x)) in
  let ptext = ptext_ >>| fun x -> Atom (unknown_location, "\"" ^ x ^ "\"") in
  let make_list start end_ pnode make =
    let plist =
      A.char start *> (A.both (A.many (pnode <* pspace)) A.pos >>| fun (xs, pos) -> (make_meta pos, xs)) <* A.char end_
    in
    A.map2 (pmeta <* pspace) plist ~f:(fun sym (m, xs) -> ({ m with symbol = sym }, xs)) <|> plist >>| fun (m, v) ->
    make m v
  in
  A.many1
    (A.fix (fun pnode ->
         pspace
         *> (ptext <|> patom_meta
            <|> make_list '(' ')' pnode (fun m v -> RBList (m, v))
            <|> make_list '{' '}' pnode (fun m v -> CBList (m, v))
            <|> make_list '[' ']' pnode (fun m v -> SBList (m, v))
            <|> patom_)
         <* pspace))

let find_line_and_pos str index =
  let length = String.length str in
  let rec aux i line pos =
    if i >= length || i >= index then (line, pos)
    else if String.get str i = '\n' then aux (i + 1) (line + 1) 1
    else aux (i + 1) line (pos + 1)
  in
  aux 0 1 1

let string_to_cjexp code =
  code
  |> A.parse_string ~consume:All (pnode (find_line_and_pos code))
  |> Result.fold ~ok:Fun.id ~error:(fun error -> failwith ("Parse SEXP error: " ^ error))
