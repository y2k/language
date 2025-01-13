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

let invoke_sexp (node : cljexp) : sexp =
  let rec invoke_sexp2 (replace_col : bool) (node : cljexp) : sexp =
    let invoke_sexp = invoke_sexp2 replace_col in
    (* print_endline @@ "== NORM[] == " ^ debug_show_cljexp [ node ]; *)
    match node with
    | RBList (m, [ (Atom (_, "let*") as l); (Atom _ as k) ]) -> SList (m, [ invoke_sexp l; invoke_sexp k ])
    | RBList (m, [ (Atom (_, "let*") as l); (Atom _ as k); v ]) ->
        SList (m, [ invoke_sexp l; invoke_sexp k; invoke_sexp v ])
    | RBList (m, (Atom (_, "fn*") as fn) :: RBList (_, args) :: body) ->
        SList (m, invoke_sexp fn :: SList (unknown_location, List.map invoke_sexp args) :: List.map invoke_sexp body)
    (* FIXME: delete dup *)
    | RBList (m, (Atom (_, "fn*") as fn) :: SBList (_, args) :: body) ->
        SList (m, invoke_sexp fn :: SList (unknown_location, List.map invoke_sexp args) :: List.map invoke_sexp body)
    | RBList (m, (Atom (_, "def*") as name) :: xs) -> SList (m, invoke_sexp name :: List.map invoke_sexp xs)
    | RBList (m, [ (Atom (_, "if*") as i); c; t; e ]) ->
        SList (m, [ invoke_sexp i; invoke_sexp c; invoke_sexp t; invoke_sexp e ])
    | RBList (m, [ (Atom (_, "quote*") as q); xs ]) -> SList (m, [ invoke_sexp q; invoke_sexp2 false xs ])
    | RBList (m, (Atom (_, "do*") as name) :: xs) -> SList (m, invoke_sexp name :: List.map invoke_sexp xs)
    | RBList (m, name :: xs) -> (
        match name with
        | Atom (_, name) when String.ends_with ~suffix:"*" name && name <> "*" -> failnode __LOC__ [ node ]
        | _ -> SList (m, invoke_sexp name :: List.map invoke_sexp xs))
    | SBList (m, xs) ->
        if replace_col then SList (m, SAtom (unknown_location, "vector") :: List.map invoke_sexp xs)
        else SList (m, List.map invoke_sexp xs)
    | CBList (m, xs) ->
        if replace_col then SList (m, SAtom (unknown_location, "hash-map") :: List.map invoke_sexp xs)
        else SList (m, List.map invoke_sexp xs)
    | Atom (m, x) -> SAtom (m, x)
    | n -> failnode __LOC__ [ n ]
  in
  invoke_sexp2 true node
