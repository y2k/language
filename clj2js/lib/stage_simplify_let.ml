open Common

let rec invoke (node : cljexp) : cljexp =
  let rec convert_bindings bindings =
    match bindings with
    | [] -> []
    | (Atom (m, _) as k) :: v :: tail ->
        RBList [ Atom (m, "let*"); k; invoke v ] :: convert_bindings tail
    | n -> failnode __LOC__ n
  in
  match node with
  | RBList (Atom (m, "let*") :: SBList bindings :: body) ->
      RBList
        (Atom (m, "do*") :: (convert_bindings bindings @ List.map invoke body))
  | RBList xs -> RBList (List.map invoke xs)
  | SBList xs -> SBList (List.map invoke xs)
  | CBList xs -> CBList (List.map invoke xs)
  | Atom _ as x -> x
