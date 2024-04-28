module A = Angstrom
open Frontend

let rec cljexp_to_json : _ -> Yojson.Safe.t = function
  | Atom (_, x) when String.starts_with ~prefix:":" x ->
      `String ("\"" ^ String.sub x 1 (String.length x - 1) ^ "\"")
  | Atom (_, x) -> `String x
  | RBList (Atom (_, "module") :: xs) ->
      let rec loop : cljexp list -> cljexp = function
        | RBList [ Atom (l, "def"); name; value ] :: tail ->
            RBList [ Atom (l, "let*"); SBList [ name; value ]; loop tail ]
        | RBList [ Atom (_, "comment") ] :: tail -> loop tail
        | [ x ] -> x
        | [] -> CBList []
        | x -> fail_node x
      in
      xs |> loop |> cljexp_to_json
  | RBList xs -> `List (List.map cljexp_to_json xs)
  | SBList xs -> `List (List.map cljexp_to_json xs)
  | CBList xs -> `List (List.map cljexp_to_json xs)

let main filename code =
  Frontend.parse_and_simplify StringMap.empty 0 filename code
  |> snd
  (* |> fun x -> print_endline (show_cljexp x); x *)
  |> cljexp_to_json
  |> Yojson.Safe.to_string
