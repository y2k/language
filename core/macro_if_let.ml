open Common

let invoke compile m bindings then_ else_ node =
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
    | _ ->
        failwith @@ "if-let has wrong signature [" ^ show_sexp2 node ^ "] "
        ^ __LOC__
  in
  loop bindings |> compile
