open Common

let rec invoke (node : cljexp) : cljexp =
  match node with
  (* | RBList ((Atom (_, "let*") as l) :: SBList bindings :: body) ->
      RBList (l :: RBList (List.map invoke bindings) :: List.map invoke body) *)
  | RBList [ (Atom (_, "let*") as l); k; v ] -> RBList [ l; k; v ]
  | RBList ((Atom (_, "fn*") as fn) :: SBList args :: body) ->
      RBList (fn :: RBList args :: List.map invoke body)
  | RBList (name :: xs) -> (
      match name with
      | Atom (_, name) when String.ends_with ~suffix:"*" name ->
          failnode __LOC__ [ node ]
      | _ -> RBList (name :: List.map invoke xs))
  | SBList xs -> RBList (Atom (unknown_location, "vector") :: List.map invoke xs)
  | CBList xs ->
      RBList (Atom (unknown_location, "hash-map") :: List.map invoke xs)
  | Atom _ as x -> x
  | n -> failnode __LOC__ [ n ]
