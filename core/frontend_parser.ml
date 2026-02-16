open Common
open Angstrom
module A = Angstrom

let is_meta_char = function
  | 'A' .. 'z' | '.' | '(' | ')' | '-' | '>' | ':' | '?' | '$' -> true
  | _ -> false

let is_atom_char = function
  | ' ' | '\n' | '[' | ']' | '(' | ')' | '{' | '}' -> false
  | _ -> true

let pnode find_line_and_pos =
  let make_meta pos =
    let line, pos = find_line_and_pos pos in
    { line; pos; symbol = "" }
  in
  let pcomment =
    (A.string ";" <|> A.string "#!") *> A.take_while (( <> ) '\n')
    <* A.many (A.char '\n')
  in
  let pspace =
    A.many (A.char ' ' <|> A.char '\n' <|> (pcomment >>| Fun.const ' '))
  in
  let patom =
    A.both A.pos (A.take_while1 is_atom_char) >>| fun (pos, x) ->
    (make_meta pos, x)
  in
  let ptext_ =
    let pescape c result = A.char '\\' *> A.char c >>| Fun.const result in
    A.char '"'
    *> (A.many1
          (pescape '"' "\\\"" <|> pescape '\\' "\\\\"
          <|> (A.satisfy (( <> ) '"') >>| String.make 1))
       >>| String.concat "")
    <* A.char '"'
  in
  let pmeta = A.char '^' *> (ptext_ <|> A.take_while1 is_meta_char) in
  let with_meta parser =
    A.map2 (pmeta <* pspace) parser ~f:(fun sym (m, x) ->
        ({ m with symbol = sym }, x))
    <|> parser
  in
  let patom_meta = with_meta patom >>| fun (m, x) -> Atom (m, x) in
  let ptext = ptext_ >>| fun x -> Atom (unknown_location, "\"" ^ x ^ "\"") in
  let make_list start end_ pnode make =
    let plist =
      A.char start
      *> ( A.both (A.many (pnode <* pspace)) A.pos >>| fun (xs, pos) ->
           (make_meta pos, xs) )
      <* A.char end_
    in
    with_meta plist >>| fun (m, xs) -> make m xs
  in
  A.many1
    (A.fix (fun pnode ->
         pspace
         *> (ptext <|> patom_meta
            <|> make_list '(' ')' pnode (fun m v -> RBList (m, v))
            <|> make_list '{' '}' pnode (fun m v -> CBList (m, v))
            <|> make_list '[' ']' pnode (fun m v -> SBList (m, v)))
         <* pspace))

let find_line_and_pos str index =
  let length = String.length str in
  let rec aux i line pos =
    if i >= length || i >= index then (line, pos)
    else if String.get str i = '\n' then aux (i + 1) (line + 1) 1
    else aux (i + 1) line (pos + 1)
  in
  aux 0 1 1

let string_to_cljexp code =
  code
  |> A.parse_string ~consume:All (pnode (find_line_and_pos code))
  |> Result.fold ~ok:Fun.id ~error:(fun e ->
      failwith ("Parse SEXP error: " ^ e))

let invoke (node : cljexp) : sexp =
  let rec to_sexp ~replace_col node =
    let conv = to_sexp ~replace_col in
    let conv_all = List.map conv in
    match node with
    | RBList (m, [ Atom (lm, "let*"); Atom (nm, name) ]) ->
        SList (m, [ SAtom (lm, "let*"); SAtom (nm, name) ])
    | RBList (m, [ Atom (lm, "let*"); Atom (nm, name); v ]) ->
        SList (m, [ SAtom (lm, "let*"); SAtom (nm, name); conv v ])
    | RBList (m, Atom (lm, "let*") :: SBList (bm, bindings) :: body) ->
        SList
          ( m,
            SAtom (lm, "let*") :: SList (bm, conv_all bindings) :: conv_all body
          )
    | RBList (m, (Atom (_, "fn*") as fn) :: SBList (_, args) :: body) ->
        SList
          ( m,
            conv fn :: SList (unknown_location, conv_all args) :: conv_all body
          )
    | RBList (m, [ (Atom (_, "quote*") as q); xs ]) ->
        SList (m, [ conv q; to_sexp ~replace_col:false xs ])
    | RBList (m, (Atom (_, ("def*" | "if*" | "do*")) as head) :: xs) ->
        SList (m, conv head :: conv_all xs)
    | RBList (_, Atom (_, name) :: _)
      when String.ends_with ~suffix:"*" name && name <> "*" ->
        failnode __LOC__ [ node ]
    | RBList (m, xs) -> SList (m, conv_all xs)
    | SBList (m, xs) when replace_col ->
        SList (m, SAtom (unknown_location, "vector") :: conv_all xs)
    | CBList (m, xs) when replace_col ->
        SList (m, SAtom (unknown_location, "hash-map") :: conv_all xs)
    | SBList (m, xs) | CBList (m, xs) -> SList (m, conv_all xs)
    | Atom (m, x) -> SAtom (m, x)
  in
  to_sexp ~replace_col:true node

let parse_text code =
  if code = "" then SAtom (meta_empty, "nil")
  else
    string_to_cljexp code
    |> ( function
    | [ x ] -> x
    | xs -> RBList (meta_empty, Atom (meta_empty, "do") :: xs) )
    |> invoke
