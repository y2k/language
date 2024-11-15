open Common

type code_builder = { new_code : cljexp list; args : cljexp list }
[@@deriving show]

let butlast xs = match List.rev xs with [] -> [] | _ :: tail -> List.rev tail
let last xs = match List.rev xs with [] -> failwith __LOC__ | x :: _ -> x
let unpack_do = function RBList (Atom (_, "do*") :: xs) -> xs | x -> [ x ]
let wrap_do xs = RBList (Atom (unknown_location, "do*") :: xs)

let convert_items convert args =
  args
  |> List.fold_left
       (fun acc arg ->
         (* print_endline @@ "LOG: " ^ show_cljexp arg; *)
         match arg with
         | Atom _ -> { acc with args = acc.args @ [ arg ] }
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
                             Atom (unknown_location, "bind*");
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
                             Atom (unknown_location, "bind*");
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
                           Atom (unknown_location, "bind*");
                           Atom (unknown_location, nn);
                           exp_arg_items |> List.rev |> List.hd;
                         ];
                     ]
               | expanded_arg ->
                   [
                     RBList
                       [
                         Atom (unknown_location, "bind*");
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

let rec convert (form : cljexp) : cljexp =
  match form with
  | RBList (Atom (_, "quote") :: _) as form -> form
  | CBList items ->
      let converted_items = convert_items convert items in
      (* print_endline @@ "LOG2: " ^ show_code_builder converted_items; *)
      if converted_items.new_code = [] then form
      else wrap_do (converted_items.new_code @ [ CBList converted_items.args ])
  | SBList items ->
      let converted_items = convert_items convert items in
      if converted_items.new_code = [] then form
      else wrap_do (converted_items.new_code @ [ SBList converted_items.args ])
  | RBList [ Atom (_, "if"); cond; then_; else_ ] ->
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
        | xs -> wrap_do (xs @ [ update_result ])
      in
      let unpacked_cond = unpack_do cond in
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
                   Atom (unknown_location, "bind*");
                   Atom (unknown_location, result_var);
                   (* Atom (unknown_location, "null"); *)
                 ];
             ];
             unpacked_cond |> butlast;
             (if cond_is_complex then
                [
                  RBList
                    [
                      Atom (unknown_location, "bind*");
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
  | RBList [ (Atom (_, "def") as def); name; args ] ->
      RBList [ def; name; convert args ]
  | RBList ((Atom (_, "fn*") as fn) :: (SBList _ as args) :: body) ->
      let body = body |> List.concat_map (fun x -> unpack_do (convert x)) in
      RBList (fn :: args :: body)
  | RBList (Atom (_, "let*") :: SBList bindins :: body) ->
      let rec convert_key_value_to_bind = function
        | [] -> []
        | key :: value :: tail ->
            let values = convert value |> unpack_do in
            butlast values
            @ [ RBList [ Atom (unknown_location, "bind*"); key; last values ] ]
            @ convert_key_value_to_bind tail
        | xs -> failnode __LOC__ xs
      in
      let bindins = convert_key_value_to_bind bindins in
      let body = body |> List.concat_map (fun x -> unpack_do (convert x)) in
      RBList ([ Atom (unknown_location, "do*") ] @ bindins @ body)
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
      (* print_endline @@ "LOG1.1: "
         ^ (args |> List.map show_cljexp
           |> List.fold_left (Printf.sprintf "%s, %s") ""); *)
      let new_args = convert_items convert args in
      (* print_endline @@ "LOG1.2: " ^ show_code_builder new_args; *)
      if new_args.new_code = [] then form
      else wrap_do (new_args.new_code @ [ RBList (fname :: new_args.args) ])
  | _ -> form
