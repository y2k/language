open Common

let rec invoke (node : sexp) : sexp =
  (* print_endline @@ "LOG[0.1]:: " ^ debug_show_cljexp [ node ]; *)
  match node with
  | SAtom _ as n -> n
  | SList (_, SAtom (_, "quote*") :: _) as n -> n
  | SList (_, [ (SAtom (_, "if*") as if_); cond; then_; else_ ]) ->
      let cond = invoke cond in
      let then_ = invoke then_ in
      let else_ = invoke else_ in
      let var = NameGenerator.get_new_var () in
      SList
        ( unknown_location,
          [
            SAtom (unknown_location, "do*");
            SList (unknown_location, [ SAtom (unknown_location, "let*"); SAtom (unknown_location, var) ]);
            SList
              ( unknown_location,
                [
                  if_;
                  cond;
                  SList (unknown_location, [ SAtom (unknown_location, "set!"); SAtom (unknown_location, var); then_ ]);
                  SList (unknown_location, [ SAtom (unknown_location, "set!"); SAtom (unknown_location, var); else_ ]);
                ] );
            SAtom (unknown_location, var);
          ] )
  | SList (m, (SAtom (_, "fn*") as fn_) :: args :: body) ->
      let body = List.map invoke body in
      SList (m, fn_ :: args :: body)
  | SList (m, fname :: args) ->
      let args = List.map invoke args in
      SList (m, fname :: args)
  | n -> failsexp __LOC__ [ n ]

let rec invoke_up_do (node : sexp) : sexp =
  (* print_endline @@ "LOG[2.1]:: " ^ debug_show_cljexp [ node ]; *)
  match node with
  | SAtom _ as n -> n
  | SList (_, SAtom (_, "quote*") :: _) as n -> n
  | SList (_, [ SAtom (_, "let*"); _ ]) as n -> n
  | SList (m, (SAtom (_, "do*") as do_) :: body) ->
      let body =
        body |> List.map invoke_up_do
        |> List.concat_map (function SList (_, SAtom (_, "do*") :: xs) -> xs | n -> [ n ])
      in
      let body_count = List.length body in
      let body =
        body
        |> List.filteri (fun i x ->
               match (i = body_count - 1, x) with true, _ -> true | false, SAtom _ -> false | false, _ -> true)
      in
      SList (m, do_ :: body)
  | SList (m, (SAtom (_, "fn*") as fn_) :: args :: body) ->
      let body = List.map invoke_up_do body in
      SList (m, fn_ :: args :: body)
  | SList (m, [ (SAtom (_, "if*") as if_); (SList (_, SAtom (_, "do*") :: _) as cond); then_; else_ ]) ->
      (* failsexp __LOC__ [ node ] |> ignore; *)
      let cond = match invoke_up_do cond with SList (_, SAtom (_, "do*") :: body) -> body | n -> [ n ] in
      let then_ = invoke_up_do then_ in
      let else_ = invoke_up_do else_ in
      SList
        ( m,
          List.concat
            [
              [ SAtom (unknown_location, "do*") ];
              butlast cond;
              [ SList (unknown_location, [ if_; last cond; then_; else_ ]) ];
            ] )
  | SList (m, [ (SAtom (_, "if*") as if_); cond; then_; else_ ]) ->
      (* failsexp __LOC__ [ node ] |> ignore; *)
      SList (m, [ if_; invoke_up_do cond; invoke_up_do then_; invoke_up_do else_ ])
  | SList (m, fname :: args) -> (
      (* print_endline @@ "LOG[2.1.1]:: " ^ debug_show_cljexp [ SBList (fname :: args) ]; *)
      let args = List.map invoke_up_do args in
      let lets =
        args
        |> List.concat_map (function
             | SList (_, SAtom (_, "do*") :: body) -> butlast body
             | SAtom _ -> []
             | SList _ -> [])
      in
      let args = args |> List.map (function SList (_, SAtom (_, "do*") :: body) -> last body | n -> n) in
      match lets with
      | [] -> SList (m, fname :: args)
      | _ -> SList (m, (SAtom (unknown_location, "do*") :: lets) @ [ SList (unknown_location, fname :: args) ]))
  | n -> failsexp __LOC__ [ n ]

let rec invoke_up_do_ level (node : sexp) : sexp =
  if level = 0 then failwith "Recurse too deep";
  let result = invoke_up_do node in
  if result = node then node else invoke_up_do_ (level - 1) result

let invoke (node : sexp) : sexp =
  (* print_endline @@ "LOG[1]:: " ^ debug_show_cljexp [ node ]; *)
  invoke node |> invoke_up_do_ 10
