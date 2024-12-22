open Common

let rec invoke (node : cljexp) : cljexp =
  let rec convert_bindings bindings =
    match bindings with
    | [] -> []
    | (Atom (m, _) as k) :: v :: tail -> RBList (m, [ Atom (m, "let*"); k; invoke v ]) :: convert_bindings tail
    | n -> failnode __LOC__ n
  in
  match node with
  | RBList (mr, Atom (m, "let*") :: SBList (_, bindings) :: body) ->
      RBList (mr, Atom (m, "do*") :: (convert_bindings bindings @ List.map invoke body))
  | RBList (m, xs) -> RBList (m, List.map invoke xs)
  | SBList (m, xs) -> SBList (m, List.map invoke xs)
  | CBList (m, xs) -> CBList (m, List.map invoke xs)
  | Atom _ as x -> x
