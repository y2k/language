open Lib__.Common

let desugar_fn_arguments expand_core_macro2 args =
  let args2, let_args2 =
    args
    |> List.fold_left
         (fun (args, lets) a ->
           match a with
           | SAtom (m, "_") ->
               (args @ [ SAtom (m, NameGenerator.get_new_var ()) ], lets)
           | SAtom _ as x -> (args @ [ x ], lets)
           | node ->
               let virt_arg =
                 SAtom (unknown_location, NameGenerator.get_new_var ())
               in
               let kv = (node, virt_arg) in
               (args @ [ virt_arg ], lets @ [ kv ]))
         ([], [])
  in
  let rec loop let_args =
    match let_args with
    | [] -> []
    | ((SAtom _, _) as x) :: tail -> x :: loop tail
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
    | (SList (m, SAtom (_, "vector") :: xs), (SAtom _ as v)) :: tail ->
        let new_nodes =
          xs
          |> List.mapi (fun i a ->
                 ( a,
                   expand_core_macro2
                     (SList
                        ( m,
                          [
                            SAtom (unknown_location, "get");
                            v;
                            SAtom (unknown_location, string_of_int i);
                          ] )) ))
        in
        loop (new_nodes @ tail)
    | ((SList (_, SAtom (_, "vector") :: _) as a), fn_v) :: t ->
        let virt_arg = SAtom (unknown_location, NameGenerator.get_new_var ()) in
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
    | (SList (m, SAtom (_, "hash-map") :: xs), (SAtom _ as v)) :: tail ->
        let rec loop2 = function
          | [] -> []
          | SAtom (_, ":as") :: lv :: t -> (lv, v) :: loop2 t
          | av :: SAtom (_, k) :: t ->
              ( av,
                expand_core_macro2
                  (SList
                     ( m,
                       [
                         SAtom (unknown_location, "get");
                         v;
                         SAtom (unknown_location, k);
                       ] )) )
              :: loop2 t
          | xs -> failsexp __LOC__ xs
        in
        let new_nodes = loop2 xs in
        loop (new_nodes @ tail)
    | ((SList (_, SAtom (_, "hash-map") :: _) as hm), fn_v) :: tail ->
        let virt_arg = SAtom (unknown_location, NameGenerator.get_new_var ()) in
        (virt_arg, fn_v) :: loop ((hm, virt_arg) :: tail)
    | xs ->
        failsexp __LOC__
          (xs |> List.map (fun (a, b) -> SList (meta_empty, [ a; b ])))
  in
  (args2, loop let_args2 |> List.concat_map (fun (a, b) -> [ a; b ]))

let invoke simplify_node (node : sexp) =
  match node with
  | SList (mfn, SAtom (l, "fn") :: SList (_, _ :: args) :: body) -> (
      let result =
        match desugar_fn_arguments simplify_node args with
        | new_args, [] ->
            SList (mfn, [ SAtom (l, "fn*"); SList (unknown_location, new_args) ])
        | new_args, let_args ->
            SList
              ( mfn,
                [
                  SAtom (l, "fn*");
                  SList (unknown_location, new_args);
                  SList
                    ( unknown_location,
                      [
                        SAtom (unknown_location, "let*");
                        SList (unknown_location, let_args);
                      ] );
                ] )
      in
      let expand_body body =
        (* prerr_endline @@ "LOG1: " ^ show_sexp2 (SList (meta_empty, body)); *)
        let body = List.map simplify_node body in
        (* prerr_endline @@ "LOG2: " ^ show_sexp2 (SList (meta_empty, body)); *)
        match body with
        | [ x ] -> [ x ]
        | xs -> [ SList (meta_empty, SAtom (meta_empty, "do*") :: xs) ]
      in
      match result with
      | SList (m, [ fa; SList (m2, args) ]) ->
          (* prerr_endline @@ "LOG: " ^ show_sexp2 (SList (meta_empty, body)); *)
          SList (m, fa :: SList (m2, args) :: expand_body body)
      | SList
          ( m,
            [
              fa;
              SList (m2, args);
              SList (m3, [ SAtom (ml, "let*"); SList (m4, let_args) ]);
            ] ) ->
          SList
            ( m,
              [
                fa;
                SList (m2, args);
                simplify_node
                  (SList
                     ( m3,
                       SAtom (ml, "let")
                       :: SList (m4, SAtom (meta_empty, "vector") :: let_args)
                       :: expand_body body ));
              ] )
      | n -> failsexp __LOC__ [ n ])
  | n -> failsexp __LOC__ [ n ]
