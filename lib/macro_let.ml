open Common

let invoke desugar_and_register expand_core_macro2 context (node : cljexp) =
  match node with
  | RBList (m2, Atom (_, "let") :: SBList (_, vals) :: body) ->
      let unpack_let_args args =
        let rec loop = function
          | [] -> []
          | (Atom _ as k) :: v :: tail -> k :: expand_core_macro2 v :: loop tail
          | SBList (m, xs) :: v :: tail ->
              let temp_val = NameGenerator.get_new_var () in
              let a = [ Atom (unknown_location, temp_val); expand_core_macro2 v ] in
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
                                        Atom (unknown_location, "get");
                                        Atom (unknown_location, temp_val);
                                        Atom (unknown_location, string_of_int i);
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
        RBList (m2, [ Atom (unknown_location, "let*"); SBList (unknown_location, loop args) ])
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
