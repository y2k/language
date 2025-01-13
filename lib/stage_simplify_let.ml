open Common

let rec invoke (node : sexp) : sexp =
  let rec convert_bindings bindings =
    match bindings with
    | [] -> []
    | (SAtom (m, _) as k) :: v :: tail -> SList (m, [ SAtom (m, "let*"); k; invoke v ]) :: convert_bindings tail
    | n -> failsexp __LOC__ n
  in
  match node with
  | SList (mr, SAtom (m, "let*") :: SList (_, bindings) :: body) ->
      SList (mr, SAtom (m, "do*") :: (convert_bindings bindings @ List.map invoke body))
  | SList (m, xs) -> SList (m, List.map invoke xs)
  | SAtom _ as x -> x
