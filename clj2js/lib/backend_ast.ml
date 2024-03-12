module A = Angstrom
open Frontend

let rec cljexp_to_json = function
  | Atom (_, x) when String.starts_with ~prefix:":" x ->
      `String ("\"" ^ String.sub x 1 (String.length x - 1) ^ "\"")
  | Atom (_, x) -> `String x
  | RBList (Atom (_, "module") :: xs) ->
      cljexp_to_json (xs |> List.rev |> List.hd)
  | RBList xs -> `List (List.map cljexp_to_json xs)
  | SBList xs -> `List (List.map cljexp_to_json xs)
  | CBList xs -> `List (List.map cljexp_to_json xs)

let main filename code =
  Frontend.parse_and_simplify 0 filename code
  |> snd
  (* |> fun x -> print_endline (show_cljexp x); x *)
  |> cljexp_to_json |> Yojson.Safe.to_string
