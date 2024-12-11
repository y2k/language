open Common

let rec invoke (node : cljexp) : cljexp =
  (* print_endline @@ "== NORM[] == " ^ debug_show_cljexp [ node ]; *)
  match node with
  | RBList [ (Atom (_, "let*") as l); k; v ] -> RBList [ l; k; invoke v ]
  (* | RBList [ (Atom (_, "if*") as l); cond; then_; else_ ] ->
      RBList [ l; invoke cond; invoke then_; invoke else_ ] *)
  (* | RBList ((Atom (_, "bind*") as name) :: xs) ->
      RBList (name :: List.map invoke xs)
  | RBList ((Atom (_, "bind-update*") as name) :: xs) ->
      RBList (name :: List.map invoke xs) *)
  | RBList ((Atom (_, "fn*") as fn) :: SBList args :: body) ->
      RBList (fn :: RBList args :: List.map invoke body)
  | RBList ((Atom (_, "def*") as name) :: xs) ->
      RBList (name :: List.map invoke xs)
  | RBList [ (Atom (_, "if*") as i); c; t; e ] ->
      RBList [ i; invoke c; invoke t; invoke e ]
  | RBList [ Atom (_, "quote*"); _ ] as node -> node
  | RBList ((Atom (_, "do*") as name) :: xs) ->
      RBList (name :: List.map invoke xs)
  | RBList (name :: xs) -> (
      match name with
      | Atom (_, name) when String.ends_with ~suffix:"*" name && name <> "*" ->
          failnode __LOC__ [ node ]
      | _ -> RBList (invoke name :: List.map invoke xs))
  | SBList xs -> RBList (Atom (unknown_location, "vector") :: List.map invoke xs)
  | CBList xs ->
      RBList (Atom (unknown_location, "hash-map") :: List.map invoke xs)
  | Atom _ as x -> x
  | n -> failnode __LOC__ [ n ]
