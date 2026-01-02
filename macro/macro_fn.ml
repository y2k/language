open Core__.Common

let mk_get m v key = SList (m, [ SAtom (meta_empty, "get"); v; key ])

let rec expand_destructure simplify = function
  | [] -> []
  | ((SAtom _, _) as kv) :: tail -> kv :: expand_destructure simplify tail
  | (SList (m, SAtom (_, "vector") :: xs), (SAtom _ as v)) :: tail ->
      let bindings =
        xs
        |> List.mapi (fun i a ->
            (a, simplify (mk_get m v (SAtom (meta_empty, string_of_int i)))))
      in
      expand_destructure simplify (bindings @ tail)
  | ((SList (_, SAtom (_, "vector") :: _) as pat), fn_v) :: tail ->
      let tmp = SAtom (meta_empty, NameGenerator.get_new_var ()) in
      (tmp, fn_v) :: expand_destructure simplify ((pat, tmp) :: tail)
  | (SList (m, SAtom (_, "hash-map") :: xs), (SAtom _ as v)) :: tail ->
      let rec expand_map_pairs = function
        | [] -> []
        | SAtom (_, ":as") :: alias :: rest ->
            (alias, v) :: expand_map_pairs rest
        | var :: SAtom (_, key) :: rest ->
            (var, simplify (mk_get m v (SAtom (meta_empty, key))))
            :: expand_map_pairs rest
        | xs -> failsexp __LOC__ xs
      in
      expand_destructure simplify (expand_map_pairs xs @ tail)
  | ((SList (_, SAtom (_, "hash-map") :: _) as pat), fn_v) :: tail ->
      let tmp = SAtom (meta_empty, NameGenerator.get_new_var ()) in
      (tmp, fn_v) :: expand_destructure simplify ((pat, tmp) :: tail)
  | xs ->
      failsexp __LOC__
        (xs |> List.map (fun (a, b) -> SList (meta_empty, [ a; b ])))

let desugar_fn_arguments simplify args =
  let args2, destructure_bindings =
    args
    |> List.fold_left
         (fun (args, bindings) arg ->
           match arg with
           | SAtom (m, "_") ->
               (args @ [ SAtom (m, NameGenerator.get_new_var ()) ], bindings)
           | SAtom _ -> (args @ [ arg ], bindings)
           | pattern ->
               let tmp = SAtom (meta_empty, NameGenerator.get_new_var ()) in
               (args @ [ tmp ], bindings @ [ (pattern, tmp) ]))
         ([], [])
  in
  let let_bindings =
    expand_destructure simplify destructure_bindings
    |> List.concat_map (fun (a, b) -> [ a; b ])
  in
  (args2, let_bindings)

let wrap_body simplify body =
  let body = List.map simplify body in
  match body with
  | [ x ] -> [ x ]
  | xs -> [ SList (meta_empty, SAtom (meta_empty, "do*") :: xs) ]

let invoke simplify = function
  | SList (mfn, SAtom (l, "fn") :: SList (_, _ :: args) :: body) ->
      let new_args, let_bindings = desugar_fn_arguments simplify args in
      let fn_atom = SAtom (l, "fn*") in
      let args_list = SList (meta_empty, new_args) in
      let expanded_body = wrap_body simplify body in
      let result =
        match let_bindings with
        | [] -> SList (mfn, fn_atom :: args_list :: expanded_body)
        | _ ->
            let let_expr =
              simplify
                (SList
                   ( meta_empty,
                     SAtom (meta_empty, "let")
                     :: SList
                          ( meta_empty,
                            SAtom (meta_empty, "vector") :: let_bindings )
                     :: expanded_body ))
            in
            SList (mfn, [ fn_atom; args_list; let_expr ])
      in
      Some result
  | _ -> None
