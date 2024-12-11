open Common

type code_builder = { new_code : cljexp list; args : cljexp list }
[@@deriving show]

let butlast xs = match List.rev xs with [] -> [] | _ :: tail -> List.rev tail
let last xs = match List.rev xs with [] -> failwith __LOC__ | x :: _ -> x
let unpack_do = function RBList (Atom (_, "do*") :: xs) -> xs | x -> [ x ]
let pack_do xs = RBList (Atom (unknown_location, "do*") :: xs)

let convert_args convert args =
  args
  |> List.fold_left
       (fun acc arg ->
         (* print_endline @@ "== LOG 6 == " ^ debug_show_cljexp [ arg ]; *)
         match arg with
         (* TODO: delete this matches (collections) *)
         | SBList [] as x -> failnode __LOC__ [ x ]
         | CBList [] as x -> failnode __LOC__ [ x ]
         | RBList (Atom (_, "vector") :: _) as x ->
             { acc with args = acc.args @ [ x ] }
         | RBList (Atom (_, "hash-map") :: _) as x ->
             { acc with args = acc.args @ [ x ] }
         | Atom _ -> { acc with args = acc.args @ [ arg ] }
         | RBList (Atom (_, "fn*") :: _) as arg ->
             (* print_endline @@ "LOG: 1.1\n" ^ show_cljexp arg; *)
             let arg2 = convert arg in
             (* print_endline @@ "LOG: 1.2\n" ^ show_cljexp arg2; *)
             { acc with args = acc.args @ [ arg2 ] }
         | RBList (Atom (_, "quote*") :: _) ->
             { acc with args = acc.args @ [ arg ] }
         | RBList [ (Atom (_, "spread") as spread); value ] -> (
             let values = convert value |> unpack_do in
             match values with
             | [ _ ] ->
                 let new_var = NameGenerator.get_new_var () in
                 {
                   new_code =
                     acc.new_code
                     @ [
                         RBList
                           [
                             Atom (unknown_location, "let*");
                             Atom (unknown_location, new_var);
                             value;
                           ];
                       ];
                   args =
                     acc.args
                     @ [ RBList [ spread; Atom (unknown_location, new_var) ] ];
                 }
             | values ->
                 let new_var = NameGenerator.get_new_var () in
                 (* failwith __LOC__ |> ignore; *)
                 {
                   new_code =
                     acc.new_code @ butlast values
                     @ [
                         RBList
                           [
                             Atom (unknown_location, "let*");
                             Atom (unknown_location, new_var);
                             last values;
                           ];
                       ];
                   args =
                     acc.args
                     @ [ RBList [ spread; Atom (unknown_location, new_var) ] ];
                 })
         | _ ->
             let nn = NameGenerator.get_new_var () in
             let new_code =
               match convert arg with
               | RBList (Atom (_, "do*") :: exp_arg_items) ->
                   (exp_arg_items |> List.rev |> List.tl |> List.rev)
                   @ [
                       RBList
                         [
                           Atom (unknown_location, "let*");
                           Atom (unknown_location, nn);
                           exp_arg_items |> List.rev |> List.hd;
                         ];
                     ]
               | expanded_arg ->
                   [
                     RBList
                       [
                         Atom (unknown_location, "let*");
                         Atom (unknown_location, nn);
                         expanded_arg;
                       ];
                   ]
             in
             {
               new_code = acc.new_code @ new_code;
               args = acc.args @ [ Atom (unknown_location, nn) ];
             })
       { new_code = []; args = [] }
(* |> fun x ->
   print_endline @@ "LOG 4.1\n" ^ show_code_builder x;
   x *)

let unpack_field_name tr = String.sub tr 2 (String.length tr - 2)
let last xs = xs |> List.rev |> List.hd
let butlast xs = xs |> List.rev |> List.tl |> List.rev

let rec flatter_binds node =
  (* print_endline @@ __LOC__ ^ "== LOG == " ^ debug_show_cljexp [ node ]; *)
  match node with
  | RBList [ (Atom (m, "let*") as b); target; value ] -> (
      match value with
      | Atom _ as v -> RBList [ b; target; v ]
      | RBList (Atom (_, "let*") :: t2 :: _) as v ->
          RBList [ Atom (m, "do*"); v; RBList [ b; target; t2 ] ]
      | RBList (Atom (_, "do*") :: xs) ->
          RBList
            (List.concat
               [
                 [ Atom (m, "do*") ];
                 butlast xs;
                 [ RBList [ b; target; last xs ] ];
               ])
          (* failnode __LOC__ [ b; target; value ] *)
      | v -> RBList [ b; target; v ])
  | RBList ((Atom (_, "do*") as d) :: xs) ->
      let ys =
        xs
        |> List.concat_map (fun x ->
               match flatter_binds x with
               | RBList (Atom (_, "do*") :: xs) -> xs
               | x -> [ x ])
      in
      RBList (d :: ys)
  | xs -> xs

let rec convert (form : cljexp) : cljexp =
  (* print_endline @@ "== LOG == " ^ debug_show_cljexp [ form ]; *)
  match form with
  | RBList [ Atom (m, "set!"); target; value ] -> (
      match target with
      | Atom _ -> RBList [ Atom (m, "bind-update*"); target; value ]
      | RBList [ Atom (_, "."); Atom (m2, tl); Atom (_, tr) ] ->
          RBList
            [
              Atom (m, "bind-update*");
              Atom (m2, tl ^ "." ^ unpack_field_name tr);
              value;
            ]
      | RBList [ Atom (_, "."); t; Atom (_, field) ] ->
          let t = convert t in
          let var = NameGenerator.get_new_var () in
          (* let t = RBList [ Atom (m, "let*"); Atom (m, var); t ] in *)
          let t =
            RBList
              [
                Atom (m, "do*");
                RBList [ Atom (m, "let*"); Atom (m, var); t ];
                RBList
                  [
                    Atom (m, "bind-update*");
                    Atom (m, var ^ "." ^ unpack_field_name field);
                    value;
                  ];
              ]
          in
          let t = flatter_binds t in
          (* failnode __LOC__ [ target; t ] |> ignore; *)
          t
      | n -> failnode __LOC__ [ n ])
  | RBList (Atom (_, "quote*") :: _) as form -> form
  (* TODO: Delete this matches *)
  (* | RBList (Atom (m, "hash-map") :: items) ->
      let converted_items = convert_args convert items in
      (* print_endline @@ "LOG2: " ^ show_code_builder converted_items; *)
      if converted_items.new_code = [] then
        RBList (Atom (m, "hash-map") :: converted_items.args)
      else
        pack_do
          (converted_items.new_code
          @ [ RBList (Atom (m, "hash-map") :: converted_items.args) ]) *)
  | CBList _ -> failnode __LOC__ [ form ]
  (* let converted_items = convert_args convert items in
      (* print_endline @@ "LOG2: " ^ show_code_builder converted_items; *)
      if converted_items.new_code = [] then CBList converted_items.args
      else pack_do (converted_items.new_code @ [ CBList converted_items.args ]) *)
  (* Vector *)
  (* | RBList (Atom (m, "vector") :: items) ->
      let converted_items = convert_args convert items in
      if converted_items.new_code = [] then
        RBList (Atom (m, "vector") :: converted_items.args)
      else
        pack_do
          (converted_items.new_code
          @ [ RBList (Atom (m, "vector") :: converted_items.args) ]) *)
  | SBList _ ->
      failnode __LOC__ [ form ]
      (* let converted_items = convert_args convert items in
      if converted_items.new_code = [] then
        RBList (Atom (unknown_location, "vector") :: converted_items.args)
      else
        pack_do
          (converted_items.new_code
          @ [
              RBList (Atom (unknown_location, "vector") :: converted_items.args);
            ]) *)
  | RBList [ Atom (_, "if*"); cond; then_; else_ ] ->
      (* print_endline @@ __LOC__ ^ " " ^ debug_show_cljexp [ form ]; *)
      let cond = convert cond in
      let cond_var = NameGenerator.get_new_var () in
      let result_var = NameGenerator.get_new_var () in
      let convert_to_set_result node =
        (* print_endline @@ "LOG1: " ^ debug_show_cljexp [ node ]; *)
        let node = convert node in
        (* print_endline @@ "LOG2: " ^ debug_show_cljexp [ node ]; *)
        let nodes = unpack_do node in
        (* print_endline @@ "LOG3: " ^ debug_show_cljexp nodes; *)
        let update_result =
          RBList
            [
              Atom (unknown_location, "bind-update*");
              Atom (unknown_location, result_var);
              last nodes;
            ]
        in
        match butlast nodes with
        | [] -> update_result
        | xs -> pack_do (xs @ [ update_result ])
      in
      let unpacked_cond = unpack_do cond in
      (* print_endline @@ __LOC__ ^ " " ^ debug_show_cljexp unpacked_cond; *)
      let cond_is_complex =
        match unpacked_cond with [ Atom _ ] -> false | _ -> true
      in
      RBList
        (List.concat
           [
             [
               Atom (unknown_location, "do*");
               RBList
                 [
                   Atom (unknown_location, "let*");
                   Atom (unknown_location, result_var);
                   (* Atom (unknown_location, "undefined"); *)
                 ];
             ];
             unpacked_cond |> butlast;
             (if cond_is_complex then
                [
                  RBList
                    [
                      Atom (unknown_location, "let*");
                      Atom (unknown_location, cond_var);
                      unpacked_cond |> last;
                    ];
                ]
              else []);
             [
               RBList
                 [
                   Atom (unknown_location, "if*");
                   (if cond_is_complex then Atom (unknown_location, cond_var)
                    else cond);
                   convert_to_set_result then_;
                   convert_to_set_result else_;
                 ];
               Atom (unknown_location, result_var);
             ];
           ])
  | RBList [ (Atom (_, "def*") as def); name; arg ] -> (
      match RBList [ def; name; convert arg ] with
      | RBList
          [
            (Atom (_, "def*") as def);
            name;
            RBList ((Atom (_, "do*") as d) :: arg_body);
          ] ->
          (* failnode __LOC__ [ form ] |> ignore; *)
          RBList
            (List.concat
               [
                 [ d ];
                 butlast arg_body;
                 [ RBList [ def; name; last arg_body ] ];
               ])
      | x -> x)
  | RBList ((Atom (_, "fn*") as fn) :: (RBList _ as args) :: body) ->
      (* print_endline @@ "LOG2: " ^ show_cljexp form; *)
      let body = body |> List.concat_map (fun x -> unpack_do (convert x)) in
      RBList (fn :: args :: body)
      (* |> fun r ->
         print_endline @@ "LOG 2.2: " ^ show_cljexp r;
         r *)
  | RBList (Atom (_, "let*") :: RBList _ :: _) -> failnode __LOC__ [ form ]
  | RBList [ (Atom (_, "let*") as l); key; value ] -> (
      let values = convert value |> unpack_do in
      match butlast values @ [ RBList [ l; key; last values ] ] with
      | [ x ] -> x
      | xs -> RBList (Atom (unknown_location, "do*") :: xs))
  (* | RBList (Atom (_, "let*") :: RBList bindins :: body) ->
      (* print_endline @@ "LOG3: " ^ show_cljexp form; *)
      let rec convert_key_value_to_bind = function
        | [] -> []
        | key :: value :: tail ->
            let values = convert value |> unpack_do in
            butlast values
            @ [ RBList [ Atom (unknown_location, "let*"); key; last values ] ]
            @ convert_key_value_to_bind tail
        | xs -> failnode __LOC__ xs
      in
      let bindins = convert_key_value_to_bind bindins in
      let body = body |> List.concat_map (fun x -> unpack_do (convert x)) in
      RBList ([ Atom (unknown_location, "do*") ] @ bindins @ body)
      (* |> fun r ->
         print_endline @@ "LOG 3.2: " ^ show_cljexp r;
         r *) *)
  | RBList ((Atom (_, "do*") as m) :: args) ->
      let args =
        args
        |> List.concat_map (fun x ->
               match convert x with
               | RBList (Atom (_, "do*") :: xs) -> xs
               | x -> [ x ])
      in
      RBList (m :: args)
  | RBList (Atom (_, "ns") :: _) -> form
  | RBList ((Atom _ as fname) :: args) ->
      (* print_endline @@ "== LOG 5.1 == " ^ debug_show_cljexp [ form ]; *)
      let new_args = convert_args convert args in
      if new_args.new_code = [] then RBList (fname :: new_args.args)
      else pack_do (new_args.new_code @ [ RBList (fname :: new_args.args) ])
      (* |> fun r ->
         print_endline @@ "LOG 5.2:\n" ^ debug_show_cljexp [ r ];
         r *)
  | _ -> form
