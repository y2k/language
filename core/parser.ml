open Lib__.Common

let parse_text code =
  let module P = Lib__.Frontend_parser in
  let module NB = Lib__.Stage_normalize_bracket in
  P.string_to_cjexp code
  |> ( function [ x ] -> x | xs -> RBList (meta_empty, Atom (meta_empty, "do*") :: xs) )
  |> NB.invoke
