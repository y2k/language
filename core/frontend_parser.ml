open Common
open Angstrom

(* ============================================================================
   Character Predicates
   ============================================================================ *)

let is_meta_char = function
  | 'A' .. 'z' | '.' | '(' | ')' | '-' | '>' | ':' | '?' | '$' -> true
  | _ -> false

let is_atom_char = function
  | ' ' | '\n' | '[' | ']' | '(' | ')' | '{' | '}' -> false
  | _ -> true

(* ============================================================================
   Source Position Tracking
   ============================================================================ *)

let compute_line_and_pos source index =
  let len = String.length source in
  let rec loop i line col =
    if i >= len || i >= index then (line, col)
    else if source.[i] = '\n' then loop (i + 1) (line + 1) 1
    else loop (i + 1) line (col + 1)
  in
  loop 0 1 1

(* ============================================================================
   Parser Combinators
   ============================================================================ *)

let make_parser source =
  let make_meta offset =
    let line, pos = compute_line_and_pos source offset in
    { line; pos; symbol = "" }
  in

  let comment =
    (string ";" <|> string "#!") *> take_while (( <> ) '\n') <* many (char '\n')
  in
  let whitespace =
    many (char ' ' <|> char '\n' <|> (comment >>| fun _ -> ' '))
  in

  let string_content =
    let escape c replacement = char '\\' *> char c >>| fun _ -> replacement in
    char '"'
    *> (many1
          (escape '"' "\\\"" <|> escape '\\' "\\\\"
          <|> (satisfy (( <> ) '"') >>| String.make 1))
       >>| String.concat "")
    <* char '"'
  in

  let metadata = char '^' *> (string_content <|> take_while1 is_meta_char) in

  let atom =
    both pos (take_while1 is_atom_char) >>| fun (offset, text) ->
    (make_meta offset, text)
  in

  let with_optional_meta parser =
    map2 (metadata <* whitespace) parser ~f:(fun sym (m, x) ->
        ({ m with symbol = sym }, x))
    <|> parser
  in

  let atom_with_meta = with_optional_meta atom >>| fun (m, x) -> Atom (m, x) in

  let string_literal =
    string_content >>| fun x -> Atom (unknown_location, "\"" ^ x ^ "\"")
  in

  let make_bracketed_list start_char end_char make_node node_parser =
    let inner =
      char start_char
      *> ( both (many (node_parser <* whitespace)) pos >>| fun (xs, offset) ->
           (make_meta offset, xs) )
      <* char end_char
    in
    with_optional_meta inner >>| fun (m, xs) -> make_node m xs
  in

  many1
    (fix (fun node_parser ->
         whitespace
         *> (string_literal <|> atom_with_meta
            <|> make_bracketed_list '(' ')'
                  (fun m xs -> RBList (m, xs))
                  node_parser
            <|> make_bracketed_list '{' '}'
                  (fun m xs -> CBList (m, xs))
                  node_parser
            <|> make_bracketed_list '[' ']'
                  (fun m xs -> SBList (m, xs))
                  node_parser)
         <* whitespace))

(* ============================================================================
   Parse Entry Point
   ============================================================================ *)

let string_to_cljexp code =
  match parse_string ~consume:All (make_parser code) code with
  | Ok result -> result
  | Error msg -> failwith ("Parse SEXP error: " ^ msg)

(* ============================================================================
   cljexp -> sexp Transformation
   ============================================================================ *)

let invoke (node : cljexp) : sexp =
  let rec transform ~(convert_collections : bool) (node : cljexp) : sexp =
    let go n = transform ~convert_collections:true n in
    let go_all ns = List.map go ns in
    match node with
    | Atom (m, x) -> SAtom (m, x)
    | RBList (m, [ Atom (lm, "let*"); Atom (nm, name) ]) ->
        SList (m, [ SAtom (lm, "let*"); SAtom (nm, name) ])
    | RBList (m, [ Atom (lm, "let*"); Atom (nm, name); value ]) ->
        SList (m, [ SAtom (lm, "let*"); SAtom (nm, name); go value ])
    | RBList (m, Atom (lm, "let*") :: SBList (bm, bindings) :: body) ->
        SList
          (m, SAtom (lm, "let*") :: SList (bm, go_all bindings) :: go_all body)
    | RBList (m, (Atom (_, "fn*") as fn) :: SBList (_, args) :: body) ->
        SList (m, go fn :: SList (unknown_location, go_all args) :: go_all body)
    | RBList (m, [ (Atom (_, "quote*") as q); xs ]) ->
        SList (m, [ go q; transform ~convert_collections:false xs ])
    | RBList (m, (Atom (_, ("def*" | "if*" | "do*")) as head) :: xs) ->
        SList (m, go head :: go_all xs)
    | RBList (_, Atom (_, name) :: _)
      when String.ends_with ~suffix:"*" name && name <> "*" ->
        failnode __LOC__ [ node ]
    | RBList (m, xs) -> SList (m, go_all xs)
    | SBList (m, xs) when convert_collections ->
        SList (m, SAtom (unknown_location, "vector") :: go_all xs)
    | CBList (m, xs) when convert_collections ->
        SList (m, SAtom (unknown_location, "hash-map") :: go_all xs)
    | SBList (m, xs) | CBList (m, xs) -> SList (m, go_all xs)
  in
  transform ~convert_collections:true node

(* ============================================================================
   Main API
   ============================================================================ *)

let parse_text code =
  if code = "" then SAtom (meta_empty, "nil")
  else
    let nodes = string_to_cljexp code in
    let cljexp =
      match nodes with
      | [ single ] -> single
      | multiple -> RBList (meta_empty, Atom (meta_empty, "do") :: multiple)
    in
    invoke cljexp
