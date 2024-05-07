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
    | a, b ->
        failnode __LOC__
          [ RBList a; RBList b; RBList arg_names; RBList arg_values ]
  in
  compute_args' StringMap.empty arg_names arg_values

let rec unpack_to_map = function
  | [] -> []
  | Atom (_, k) :: v :: tail -> (k, v) :: unpack_to_map tail
  | n -> failnode __LOC__ n

let run (ctx : context) (macro : cljexp) (macro_args : cljexp list) : cljexp =
  let macro_arg_names, macro_body =
    match macro with
    | RBList (_ :: _ :: SBList macro_arg_names :: body) ->
        (macro_arg_names, body)
    | n -> failnode __LOC__ [ n ]
  in

  (* (match macro_arg_names with
     | [ _; Atom (_, "&"); _ ] ->
      failnode __LOC__ macro_args
     | _ -> ()); *)
  (* macro_args |> List.map show_sexp
     |> List.reduce_opt (Printf.sprintf "%s\n%s")
     |> Option.value ~default:"<EMPTY>"
     |> Printf.sprintf "[LOG1] %s:\n======\n%s\n======" __LOC__
     |> prerr_endline; *)
  (* prerr_endline @@ "[LOG2] " ^ show_context ctx.scope; *)
  (* ctx.scope |> StringMap.to_list
  |> List.map (fun (k, _) -> k)
  |> List.reduce_opt (Printf.sprintf "%s %s")
  |> Option.value ~default:"" |> Printf.sprintf "[LOG3] %s" |> print_endline; *)

  let args =
    compute_args macro_arg_names macro_args
    |> StringMap.map (fun x -> (x, ctx))
    |> StringMap.to_seq
  in
  let local_scope = StringMap.add_seq args ctx.scope in

  RBList (Atom (unknown_location, "let*") :: SBList [] :: macro_body)
  |> ctx.interpreter { ctx with scope = local_scope }
  |> snd
