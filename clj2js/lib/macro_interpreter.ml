open Common

let compute_args (arg_names : cljexp list) (arg_values : cljexp list) : cljexp StringMap.t =
  let rec compute_args' acc arg_names arg_values =
    match (arg_names, arg_values) with
    | [ Atom (_, "&"); Atom (_, name) ], vt -> StringMap.add name (RBList (unknown_location, vt)) acc
    | Atom (_, name) :: nt, v :: vt -> compute_args' (StringMap.add name v acc) nt vt
    | [], [] -> acc
    | a, b ->
        failnode __LOC__
          [
            RBList (unknown_location, a);
            RBList (unknown_location, b);
            RBList (unknown_location, arg_names);
            RBList (unknown_location, arg_values);
          ]
  in
  compute_args' StringMap.empty arg_names arg_values

let rec unpack_to_map = function
  | [] -> []
  | Atom (_, k) :: v :: tail -> (k, v) :: unpack_to_map tail
  | n -> failnode __LOC__ n

let run (ctx : context) (macro : cljexp) (macro_args : cljexp list) : cljexp =
  let macro_arg_names, macro_body =
    match macro with
    | RBList (_, _ :: _ :: SBList (_, macro_arg_names) :: body) -> (macro_arg_names, body)
    | n -> failnode __LOC__ [ n ]
  in
  let args = compute_args macro_arg_names macro_args |> StringMap.map (fun x -> (x, ctx)) |> StringMap.to_seq in
  let local_scope = StringMap.add_seq args ctx.scope in
  RBList (unknown_location, Atom (unknown_location, "let*") :: SBList (unknown_location, []) :: macro_body)
  |> ctx.interpreter { ctx with scope = local_scope }
  |> snd
