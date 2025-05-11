open Lib__.Common
module P = Lib__.Frontend_parser
module NB = Lib__.Stage_normalize_bracket

let parse_text code =
  if code = "" then SAtom (meta_empty, "nil")
  else
    P.string_to_cjexp code
    |> ( function
    | [ x ] -> x
    | xs -> RBList (meta_empty, Atom (meta_empty, "do") :: xs) )
    |> NB.invoke
