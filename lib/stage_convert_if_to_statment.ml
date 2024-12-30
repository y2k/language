open Common

let rec invoke (node : cljexp) : cljexp =
  (* print_endline @@ "LOG[0.1]:: " ^ debug_show_cljexp [ node ]; *)
  match node with
  | Atom _ as n -> n
  | RBList (_, Atom (_, "quote*") :: _) as n -> n
  | RBList (_, [ (Atom (_, "if*") as if_); cond; then_; else_ ]) ->
      let cond = invoke cond in
      let then_ = invoke then_ in
      let else_ = invoke else_ in
      let var = NameGenerator.get_new_var () in
      RBList
        ( unknown_location,
          [
            Atom (unknown_location, "do*");
            RBList (unknown_location, [ Atom (unknown_location, "let*"); Atom (unknown_location, var) ]);
            RBList
              ( unknown_location,
                [
                  if_;
                  cond;
                  RBList (unknown_location, [ Atom (unknown_location, "set!"); Atom (unknown_location, var); then_ ]);
                  RBList (unknown_location, [ Atom (unknown_location, "set!"); Atom (unknown_location, var); else_ ]);
                ] );
            Atom (unknown_location, var);
          ] )
  | RBList (m, (Atom (_, "fn*") as fn_) :: args :: body) ->
      let body = List.map invoke body in
      RBList (m, fn_ :: args :: body)
  | RBList (m, fname :: args) ->
      let args = List.map invoke args in
      RBList (m, fname :: args)
  | n -> failnode __LOC__ [ n ]

let rec invoke_up_do (node : cljexp) : cljexp =
  (* print_endline @@ "LOG[2.1]:: " ^ debug_show_cljexp [ node ]; *)
  match node with
  | Atom _ as n -> n
  | RBList (_, Atom (_, "quote*") :: _) as n -> n
  | RBList (_, [ Atom (_, "let*"); _ ]) as n -> n
  | RBList (m, (Atom (_, "do*") as do_) :: body) ->
      let body =
        body |> List.map invoke_up_do
        |> List.concat_map (function RBList (_, Atom (_, "do*") :: xs) -> xs | n -> [ n ])
      in
      let body_count = List.length body in
      let body =
        body
        |> List.filteri (fun i x ->
               match (i = body_count - 1, x) with true, _ -> true | false, Atom _ -> false | false, _ -> true)
      in
      RBList (m, do_ :: body)
  | RBList (m, (Atom (_, "fn*") as fn_) :: args :: body) ->
      let body = List.map invoke_up_do body in
      RBList (m, fn_ :: args :: body)
  | RBList (m, [ (Atom (_, "if*") as if_); (RBList (_, Atom (_, "do*") :: _) as cond); then_; else_ ]) ->
      (* failnode __LOC__ [ node ] |> ignore; *)
      let cond = match invoke_up_do cond with RBList (_, Atom (_, "do*") :: body) -> body | n -> [ n ] in
      let then_ = invoke_up_do then_ in
      let else_ = invoke_up_do else_ in
      RBList
        ( m,
          List.concat
            [
              [ Atom (unknown_location, "do*") ];
              butlast cond;
              [ RBList (unknown_location, [ if_; last cond; then_; else_ ]) ];
            ] )
  | RBList (m, [ (Atom (_, "if*") as if_); cond; then_; else_ ]) ->
      (* failnode __LOC__ [ node ] |> ignore; *)
      RBList (m, [ if_; invoke_up_do cond; invoke_up_do then_; invoke_up_do else_ ])
  | RBList (m, fname :: args) -> (
      (* print_endline @@ "LOG[2.1.1]:: " ^ debug_show_cljexp [ SBList (fname :: args) ]; *)
      let args = List.map invoke_up_do args in
      let lets =
        args
        |> List.concat_map (function
             | RBList (_, Atom (_, "do*") :: body) -> butlast body
             | Atom _ -> []
             | RBList _ -> []
             | n -> failnode __LOC__ [ n ])
      in
      let args = args |> List.map (function RBList (_, Atom (_, "do*") :: body) -> last body | n -> n) in
      match lets with
      | [] -> RBList (m, fname :: args)
      | _ -> RBList (m, (Atom (unknown_location, "do*") :: lets) @ [ RBList (unknown_location, fname :: args) ]))
  | n -> failnode __LOC__ [ n ]

let rec invoke_up_do_ level (node : cljexp) : cljexp =
  if level = 0 then failwith "Recurse too deep";
  let result = invoke_up_do node in
  if result = node then node else invoke_up_do_ (level - 1) result

let invoke (node : cljexp) : cljexp =
  (* print_endline @@ "LOG[1]:: " ^ debug_show_cljexp [ node ]; *)
  invoke node
  (* |> (fun node ->
  print_endline @@ "LOG[2]:: " ^ debug_show_cljexp [ node ];
  node) *)
  |> invoke_up_do_ 10
