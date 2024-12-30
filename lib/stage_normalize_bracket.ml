open Common

let rec invoke (node : cljexp) : cljexp =
  (* print_endline @@ "== NORM[] == " ^ debug_show_cljexp [ node ]; *)
  match node with
  | RBList (m, [ (Atom (_, "let*") as l); k; v ]) -> RBList (m, [ l; k; invoke v ])
  | RBList (m, (Atom (_, "fn*") as fn) :: SBList (_, args) :: body) ->
      RBList (m, fn :: RBList (unknown_location, args) :: List.map invoke body)
  | RBList (m, (Atom (_, "def*") as name) :: xs) -> RBList (m, name :: List.map invoke xs)
  | RBList (m, [ (Atom (_, "if*") as i); c; t; e ]) -> RBList (m, [ i; invoke c; invoke t; invoke e ])
  | RBList (_, [ Atom (_, "quote*"); _ ]) as node -> node
  | RBList (m, (Atom (_, "do*") as name) :: xs) -> RBList (m, name :: List.map invoke xs)
  | RBList (m, name :: xs) -> (
      match name with
      | Atom (_, name) when String.ends_with ~suffix:"*" name && name <> "*" -> failnode __LOC__ [ node ]
      | _ -> RBList (m, invoke name :: List.map invoke xs))
  | SBList (m, xs) -> RBList (m, Atom (unknown_location, "vector") :: List.map invoke xs)
  | CBList (m, xs) -> RBList (m, Atom (unknown_location, "hash-map") :: List.map invoke xs)
  | Atom _ as x -> x
  | n -> failnode __LOC__ [ n ]
