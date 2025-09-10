open Core__.Common

let invoke simplify = function
  | SList (m, SAtom (l, "case") :: target :: body) as node ->
      let var = NameGenerator.get_new_var () in
      let rec loop = function
        | cond :: then_ :: body ->
            SList
              ( meta_empty,
                [
                  SAtom (meta_empty, "if");
                  SList
                    ( meta_empty,
                      [ SAtom (meta_empty, "="); SAtom (meta_empty, var); cond ]
                    );
                  then_;
                  loop body;
                ] )
        | [ x ] -> x
        | _ -> failsexp __LOC__ [ node ]
      in
      SList
        ( m,
          [
            SAtom (l, "let");
            SList
              ( meta_empty,
                [
                  SAtom (meta_empty, "vector"); SAtom (meta_empty, var); target;
                ] );
            loop body;
          ] )
      |> simplify |> Option.some
  | _ -> None
