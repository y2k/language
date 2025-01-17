open Common

let invoke desugar_and_register context (node : cljexp) =
  let expand_core_macro2 x = desugar_and_register context x |> snd in
  match node with
  | RBList (m2, Atom (_, "let") :: SBList (_, vals) :: body) ->
      let unpack_let_args args =
        let rec loop = function
          | [] -> []
          | Atom (m, "_") :: v :: tail -> Atom (m, NameGenerator.get_new_var ()) :: expand_core_macro2 v :: loop tail
          | (Atom _ as k) :: v :: tail -> k :: expand_core_macro2 v :: loop tail
          | SBList (m, xs) :: v :: tail ->
              let temp_val = NameGenerator.get_new_var () in
              let a = [ Atom (meta_empty, temp_val); expand_core_macro2 v ] in
              let b =
                xs
                |> List.fold_left
                     (fun (i, acc) x ->
                       let binding =
                         match x with
                         | Atom (_, "_") -> []
                         | x ->
                             [
                               x;
                               expand_core_macro2
                                 (RBList
                                    ( m,
                                      [
                                        Atom (meta_empty, "get");
                                        Atom (meta_empty, temp_val);
                                        Atom (meta_empty, string_of_int i);
                                      ] ));
                             ]
                       in
                       (i + 1, acc @ binding))
                     (0, a)
                |> snd
              in
              b @ loop tail
          | xs -> failnode __LOC__ xs
        in
        RBList (m2, [ Atom (meta_empty, "let*"); SBList (meta_empty, loop args) ])
      in
      let unpacked_let =
        match unpack_let_args vals with
        | RBList (m3, l :: SBList (m4, args) :: let_body) ->
            let body = List.map (fun x -> desugar_and_register context x |> snd) body in
            RBList (m3, (l :: SBList (m4, args) :: let_body) @ body)
        | n -> failnode __LOC__ [ n ]
      in
      (context, unpacked_let)
  | node -> failnode __LOC__ [ node ]
