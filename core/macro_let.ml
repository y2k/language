open Common

let invoke desugar_and_register _context (node : sexp) : sexp =
  let expand_core_macro2 = desugar_and_register in
  match node with
  | SList (m2, SAtom (_, "let") :: SList (_, _ :: vals) :: body) -> (
      let unpack_let_args args =
        let rec loop = function
          | [] -> []
          | SAtom (m, "_") :: v :: tail ->
              SAtom (m, NameGenerator.get_new_var ())
              :: expand_core_macro2 v :: loop tail
          | (SAtom _ as k) :: v :: tail ->
              k :: expand_core_macro2 v :: loop tail
          | SList (m, _ :: xs) :: v :: tail ->
              let temp_val = NameGenerator.get_new_var () in
              let a = [ SAtom (meta_empty, temp_val); expand_core_macro2 v ] in
              let b =
                xs
                |> List.fold_left
                     (fun (i, acc) x ->
                       let binding =
                         match x with
                         | SAtom (_, "_") -> []
                         | x ->
                             [
                               x;
                               expand_core_macro2
                                 (SList
                                    ( m,
                                      [
                                        SAtom (meta_empty, "get");
                                        SAtom (meta_empty, temp_val);
                                        SAtom (meta_empty, string_of_int i);
                                      ] ));
                             ]
                       in
                       (i + 1, acc @ binding))
                     (0, a)
                |> snd
              in
              b @ loop tail
          | xs -> failsexp __LOC__ xs
        in
        SList (m2, [ SAtom (meta_empty, "let*"); SList (meta_empty, loop args) ])
      in
      match unpack_let_args vals with
      | SList (_, _l :: SList (_, args) :: let_body) ->
          let body = List.map desugar_and_register body in
          let lets =
            List.split_into_pairs args
            |> List.map (fun (k, v) ->
                   let v = expand_core_macro2 v in
                   SList (meta_empty, [ SAtom (meta_empty, "let"); k; v ]))
          in
          (* SList (m3, (l :: SList (m4, args) :: let_body) @ body) *)
          SList (m2, SAtom (meta_empty, "do") :: (lets @ let_body @ body))
          |> desugar_and_register
      | n -> failsexp __LOC__ [ n ])
  | node -> failsexp __LOC__ [ node ]
