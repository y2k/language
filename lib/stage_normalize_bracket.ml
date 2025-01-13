open Common

let invoke (node : cljexp) : sexp =
  let rec invoke_sexp2 (replace_col : bool) (node : cljexp) : sexp =
    let invoke_sexp = invoke_sexp2 replace_col in
    (* print_endline @@ "== NORM[] == " ^ debug_show_cljexp [ node ]; *)
    match node with
    | RBList (m, Atom (lm, "let*") :: SBList (bm, bindings) :: body) ->
        SList (m, SAtom (lm, "let*") :: SList (bm, List.map invoke_sexp bindings) :: List.map invoke_sexp body)
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
