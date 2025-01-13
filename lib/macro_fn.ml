open Common

let desugar_fn_arguments expand_core_macro2 args =
  let args2, let_args2 =
    args
    |> List.fold_left
         (fun (args, lets) a ->
           match a with
           | Atom _ as x -> (args @ [ x ], lets)
           | node ->
               let virt_arg = Atom (unknown_location, NameGenerator.get_new_var ()) in
               let kv = (node, virt_arg) in
               (args @ [ virt_arg ], lets @ [ kv ]))
         ([], [])
  in

  let rec loop let_args =
    match let_args with
    | [] -> []
    | ((Atom _, _) as x) :: t -> x :: loop t
    (*
       [a b c] xs
       >>>
       a (get 0 xs)
       b (get 1 xs)
       c (get 2 xs)

       [[a b] c] xs
       >>>
       [a b] (get 0 xs)
       c (get 1 xs)
       >>>
       t1 (get 0 xs)
       [a b] t1           <-------
       c (get 1 xs)
       >>>
       t1 (get 0 xs)
       a (get 0 t1)
       b (get 1 t1)
       c (get 1 xs)
    *)
    | (SBList (m, xs), (Atom _ as v)) :: t ->
        let new_nodes =
          xs
          |> List.mapi (fun i a ->
                 ( a,
                   expand_core_macro2
                     (RBList (m, [ Atom (unknown_location, "get"); v; Atom (unknown_location, string_of_int i) ])) ))
        in
        loop (new_nodes @ t)
    | ((SBList _ as a), fn_v) :: t ->
        let virt_arg = Atom (unknown_location, NameGenerator.get_new_var ()) in
        (virt_arg, fn_v) :: loop ((a, virt_arg) :: t)
    (*
       {a :b c :d}
       >>>
       a (get e :b)
       c (get e :d)

       {{a :b c :d} :d e :f} xs
       >>>
       {a :b c :d} (get xs :d)
       e (get xs :f)
       >>>
    *)
    | (CBList (m, xs), (Atom _ as v)) :: t ->
        let rec loop2 = function
          | [] -> []
          | av :: Atom (_, k) :: t ->
              (av, expand_core_macro2 (RBList (m, [ Atom (unknown_location, "get"); v; Atom (unknown_location, k) ])))
              :: loop2 t
          | xs -> failnode __LOC__ xs
        in
        let new_nodes = loop2 xs in
        loop (new_nodes @ t)
    | ((CBList _ as a), fn_v) :: t ->
        let virt_arg = Atom (unknown_location, NameGenerator.get_new_var ()) in
        (virt_arg, fn_v) :: loop ((a, virt_arg) :: t)
    | xs -> failnode __LOC__ (xs |> List.concat_map (fun (a, b) -> [ a; b ]))
  in
  (args2, loop let_args2 |> List.concat_map (fun (a, b) -> [ a; b ]))

let invoke desugar_and_register expand_core_macro2 context (node : cljexp) =
  match node with
  | RBList (mfn, Atom (l, "fn") :: args_node :: body) ->
      let args =
        match args_node with
        | SBList (_, args) -> args
        | RBList (_, args) -> args
        | n -> failnode __LOC__ [n]
      in
      let result =
        match desugar_fn_arguments expand_core_macro2 args with
        | new_args, [] -> RBList (mfn, [ Atom (l, "fn*"); SBList (unknown_location, new_args) ])
        | new_args, let_args ->
            RBList
              ( mfn,
                [
                  Atom (l, "fn*");
                  SBList (unknown_location, new_args);
                  RBList (unknown_location, [ Atom (unknown_location, "let*"); SBList (unknown_location, let_args) ]);
                ] )
      in
      let expand_body _ _ body = List.map (fun x -> desugar_and_register context x |> snd) body in
      let fn =
        match result with
        | RBList (m, [ fa; SBList (m2, args) ]) -> RBList (m, fa :: SBList (m2, args) :: expand_body args [] body)
        | RBList (m, [ fa; SBList (m2, args); RBList (m3, [ (Atom (_, "let*") as l); SBList (m4, let_args) ]) ]) ->
            RBList
              (m, [ fa; SBList (m2, args); RBList (m3, l :: SBList (m4, let_args) :: expand_body args let_args body) ])
        | n -> failnode __LOC__ [ n ]
      in
      (context, fn)
  | n -> failnode __LOC__ [ n ]
