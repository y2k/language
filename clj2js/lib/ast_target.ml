module A = Angstrom
open Core

let rec cljexp_to_json = function
  | Atom (_, x) -> `String x
  | RBList xs -> `List (List.map cljexp_to_json xs)
  | SBList xs -> `List (List.map cljexp_to_json xs)
  | CBList xs -> `List (List.map cljexp_to_json xs)

let main filename code =
  Core.parse_and_simplify filename code
  |> snd |> cljexp_to_json |> Yojson.Safe.to_string
