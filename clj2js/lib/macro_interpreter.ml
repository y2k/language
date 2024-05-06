open Common

let compute_args (arg_names : cljexp list) (arg_values : cljexp list) :
    cljexp StringMap.t =
  let rec compute_args' acc arg_names arg_values =
    match (arg_names, arg_values) with
    | [ Atom (_, "&"); Atom (_, name) ], vt ->
        StringMap.add name (RBList vt) acc
    | Atom (_, name) :: nt, v :: vt ->
        compute_args' (StringMap.add name v acc) nt vt
    | [], [] -> acc
    | a, b -> failnode __LOC__ (List.concat [ a; b ])
  in
  compute_args' StringMap.empty arg_names arg_values

let rec unpack_to_map = function
  | [] -> []
  | Atom (_, k) :: v :: tail -> (k, v) :: unpack_to_map tail
  | n -> failnode __LOC__ n

let run (ctx : context) (macro : cljexp) (_macro_args : cljexp list) : cljexp =
  let macro_arg_names, macro_body =
    match macro with
    | RBList (_ :: _ :: SBList macro_arg_names :: body) ->
        (macro_arg_names, body)
    | n -> failnode __LOC__ [ n ]
  in

  let args = compute_args macro_arg_names _macro_args |> StringMap.to_seq in
  let local_scope = ctx.scope |> StringMap.add_seq args in

  RBList (Atom (unknown_location, "let*") :: SBList [] :: macro_body)
  |> ctx.interpreter { ctx with scope = local_scope }
  |> snd
