open Common
open Angstrom
module A = Angstrom

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
    A.many (A.char ' ' <|> A.char '\n' <|> (pcomment >>| fun _ -> ' '))
  in
  let patom =
    A.both A.pos
      (A.take_while1 (function
        | ' ' | '\n' | '[' | ']' | '(' | ')' | '{' | '}' -> false
        | _ -> true))
    >>| fun (pos, x) -> (make_meta pos, x)
  in
  let patom_ = patom >>| fun (m, a) -> Atom (m, a) in
  let ptext_ =
    A.char '"'
    *> ( A.many1
           (A.char '\\' *> A.char '"'
           >>| Fun.const "\\\""
           <|> (A.char '\\' *> A.char '\\' >>| Fun.const "\\\\")
           <|> (A.satisfy (( <> ) '"') >>| String.make 1))
       >>| fun x -> String.concat "" x )
    <* A.char '"'
  in
  let pmeta =
    A.char '^'
    *> (ptext_
       <|> A.take_while1 (fun x ->
           (x >= 'A' && x <= 'z')
           || x = '.' || x = '(' || x = ')' || x = '-' || x = '>' || x = ':'
           || x = '?' || x = '$'))
  in
  let patom_meta =
    A.map2 (pmeta <* pspace) patom ~f:(fun m (a, x) ->
        Atom ({ a with symbol = m }, x))
  in
  let ptext = ptext_ >>| fun x -> Atom (unknown_location, "\"" ^ x ^ "\"") in
  let make_list start end_ pnode make =
    let plist =
      A.char start
      *> ( A.both (A.many (pnode <* pspace)) A.pos >>| fun (xs, pos) ->
           (make_meta pos, xs) )
      <* A.char end_
    in
    A.map2 (pmeta <* pspace) plist ~f:(fun sym (m, xs) ->
        ({ m with symbol = sym }, xs))
    <|> plist
    >>| fun (m, v) -> make m v
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
  |> Result.fold ~ok:Fun.id ~error:(fun error ->
      failwith ("Parse SEXP error: " ^ error))

let invoke (node : cljexp) : sexp =
  let rec to_sexp ~replace_col node =
    let conv = to_sexp ~replace_col in
    let conv_all = List.map conv in
    match node with
    (* let* with simple binding: (let* name) or (let* name value) *)
    | RBList (m, [ Atom (lm, "let*"); Atom (nm, name) ]) ->
        SList (m, [ SAtom (lm, "let*"); SAtom (nm, name) ])
    | RBList (m, [ Atom (lm, "let*"); Atom (nm, name); v ]) ->
        SList (m, [ SAtom (lm, "let*"); SAtom (nm, name); conv v ])
    (* let* with bindings vector *)
    | RBList (m, Atom (lm, "let*") :: SBList (bm, bindings) :: body) ->
        SList
          ( m,
            SAtom (lm, "let*") :: SList (bm, conv_all bindings) :: conv_all body
          )
    (* fn* *)
    | RBList (m, (Atom (_, "fn*") as fn) :: SBList (_, args) :: body) ->
        SList
          ( m,
            conv fn :: SList (unknown_location, conv_all args) :: conv_all body
          )
    (* quote* - disable collection replacement for quoted content *)
    | RBList (m, [ (Atom (_, "quote*") as q); xs ]) ->
        SList (m, [ conv q; to_sexp ~replace_col:false xs ])
    (* Standard forms: def*, if*, do* *)
    | RBList (m, (Atom (_, ("def*" | "if*" | "do*")) as head) :: xs) ->
        SList (m, conv head :: conv_all xs)
    (* Reject unknown starred forms *)
    | RBList (_, Atom (_, name) :: _)
      when String.ends_with ~suffix:"*" name && name <> "*" ->
        failnode __LOC__ [ node ]
    (* General function application *)
    | RBList (m, xs) -> SList (m, conv_all xs)
    (* Collections *)
    | SBList (m, xs) when replace_col ->
        SList (m, SAtom (unknown_location, "vector") :: conv_all xs)
    | CBList (m, xs) when replace_col ->
        SList (m, SAtom (unknown_location, "hash-map") :: conv_all xs)
    | SBList (m, xs) | CBList (m, xs) -> SList (m, conv_all xs)
    (* Atom *)
    | Atom (m, x) -> SAtom (m, x)
  in
  to_sexp ~replace_col:true node

let parse_text code =
  if code = "" then SAtom (meta_empty, "nil")
  else
    string_to_cjexp code
    |> ( function
    | [ x ] -> x
    | xs -> RBList (meta_empty, Atom (meta_empty, "do") :: xs) )
    |> invoke
