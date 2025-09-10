open Core__.Common

let invoke compile m bindings then_ else_ _node =
  let rec loop = function
    | SAtom (l, name) :: value :: tail ->
        let name = if name = "_" then NameGenerator.get_new_var () else name in
        SList
          ( m,
            [
              SAtom (l, "let");
              SList
                ( meta_empty,
                  [
                    SAtom (meta_empty, "vector");
                    SAtom (meta_empty, name);
                    value;
                  ] );
              SList
                ( meta_empty,
                  [
                    SAtom (meta_empty, "if");
                    SAtom (meta_empty, name);
                    loop tail;
                    else_;
                  ] );
            ] )
    | [] -> then_
    | x -> failsexp __LOC__ x
  in
  loop bindings |> compile

let invoke simplify = function
  | SList
      ( m,
        [
          SAtom (_, "if-let");
          SList (_, SAtom (_, "vector") :: bindings);
          then_;
          else_;
        ] ) as sexp ->
      invoke simplify m bindings then_ else_ sexp |> Option.some
  | _ -> None
