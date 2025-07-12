open Common

let rec handle_args args body =
  match args with
  | [] -> ([], body)
  | (SAtom (m, _) as x) :: tail ->
      if m.symbol <> "" then
        let nv = NameGenerator.get_new_var () in
        let new_arg = SAtom (meta_empty, nv) in
        let body =
          SList
            ( meta_empty,
              [
                SAtom (meta_empty, "do*");
                SList
                  ( meta_empty,
                    [
                      SAtom (meta_empty, "let*");
                      x;
                      SList
                        ( meta_empty,
                          [
                            SAtom (meta_empty, "cast");
                            SAtom (meta_empty, m.symbol);
                            new_arg;
                          ] );
                    ] );
                body;
              ] )
        in
        let args, body = handle_args tail body in
        (new_arg :: args, body)
      else
        let args, body = handle_args tail body in
        (x :: args, body)
  | x -> failsexp __LOC__ x

let rec invoke = function
  | SAtom _ as x -> x
  | SList (m, [ (SAtom (_, "fn*") as fn_); SList (ma, args); body ]) ->
      let args, body = handle_args args body in
      let body = invoke body in
      SList (m, [ fn_; SList (ma, args); invoke body ])
  | SList (m, (SAtom (_, "do*") as do_) :: body) ->
      let body = List.map invoke body in
      SList (m, do_ :: body)
  | SList (m, (SAtom (_, "def*") as def_) :: name :: value) ->
      let value = List.map invoke value in
      SList (m, def_ :: name :: value)
  | SList (m, (SAtom (_, "let*") as let_) :: name :: value) ->
      let value = List.map invoke value in
      SList (m, let_ :: name :: value)
  | SList (m, (SAtom (_, "if*") as if_) :: cond :: then_ :: else_) ->
      let cond = invoke cond in
      let then_ = invoke then_ in
      let else_ = List.map invoke else_ in
      SList (m, if_ :: cond :: then_ :: else_)
  | SList (_, SAtom (_, name) :: _) as x
    when String.ends_with ~suffix:"*" name && name <> "*" ->
      failsexp __LOC__ [ x ]
  | SList (m, name :: args) ->
      let args = List.map invoke args in
      SList (m, name :: args)
  | x -> failsexp __LOC__ [ x ]
