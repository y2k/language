open Core__.Common

let rec expand_bindings compile m bindings then_ else_ =
  match bindings with
  | SAtom (l, name) :: value :: tail ->
      let name = if name = "_" then NameGenerator.get_new_var () else name in
      SList
        ( m,
          [
            SAtom (l, "let");
            SList
              ( meta_empty,
                [
                  SAtom (meta_empty, "vector"); SAtom (meta_empty, name); value;
                ] );
            SList
              ( meta_empty,
                [
                  SAtom (meta_empty, "if");
                  SAtom (meta_empty, name);
                  expand_bindings compile m tail then_ else_;
                  else_;
                ] );
          ] )
  | [] -> then_
  | xs -> failsexp __LOC__ xs

let invoke simplify = function
  | SList
      ( m,
        [
          SAtom (_, "if-let");
          SList (_, SAtom (_, "vector") :: bindings);
          then_;
          else_;
        ] ) ->
      expand_bindings simplify m bindings then_ else_ |> simplify |> Option.some
  | _ -> None
