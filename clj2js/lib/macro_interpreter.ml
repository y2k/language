open Common

let compute_args (arg_names : cljexp list) (arg_values : cljexp list) :
    cljexp StringMap.t =
  let rec compute_args' acc arg_names arg_values =
    match (arg_names, arg_values) with
    | [ Atom (_, "&"); Atom (_, name) ], vt ->
        StringMap.add name (RBList vt) acc
    | Atom (_, name) :: nt, v :: vt ->
        compute_args' (StringMap.add name v acc) nt vt
    | [], [] -> acc
    | a, b -> failnode __LOC__ (List.concat [ a; b ])
  in
  compute_args' StringMap.empty arg_names arg_values

let rec unpack_to_map = function
  | [] -> []
  | Atom (_, k) :: v :: tail -> (k, v) :: unpack_to_map tail
  | n -> failnode __LOC__ n

let rec run (context : context) (macro : cljexp) (macro_args : cljexp list) :
    cljexp =
  match macro with
  | RBList (_ :: _ :: SBList macro_arg_names :: body) ->
      let rec execute (node : cljexp) : cljexp =
        let args = compute_args macro_arg_names macro_args in
        (* print_endline @@ "LOG: " ^ show_cljexp node; *)
        match node with
        | RBList [ Atom (_, "transform_nodes"); CBList opt; xs ] ->
            let xs =
              match execute xs with
              | RBList xs -> xs
              | n -> failnode __LOC__ [ n ]
            in
            let sep =
              unpack_to_map opt |> List.assoc_opt ":sep"
              |> (function Some x -> x | None -> failnode __LOC__ opt)
              |> execute
            in
            let len = List.length xs in
            let r =
              xs
              |> List.mapi (fun i x -> (i, x))
              |> List.concat_map (fun (i, x) ->
                     if i < len - 1 then [ x; sep ] else [ x ])
            in
            RBList r
        | RBList [ Atom (_, "vec"); Atom (_, xs) ] -> (
            match StringMap.find xs args with
            | RBList xs -> SBList xs
            | x -> failnode __LOC__ [ x ])
        | RBList (Atom (_, "concat") :: xs) ->
            let r =
              xs |> List.map execute
              |> List.concat_map (function
                   | RBList xs -> xs
                   | n -> failnode __LOC__ [ n ])
            in
            RBList r
        | RBList (Atom (_, "str") :: str_args) ->
            let result =
              str_args |> List.map execute
              |> List.map (function
                   | Atom (_, x)
                     when String.starts_with ~prefix:"\"" x
                          && String.ends_with ~suffix:"\"" x ->
                       String.sub x 1 (String.length x - 2)
                   | Atom (_, x) -> x
                   | n -> failnode __LOC__ [ n ])
              |> String.concat ""
            in
            Atom (unknown_location, "\"" ^ result ^ "\"")
        | RBList (Atom (_, "list") :: list_args) ->
            RBList (List.map execute list_args)
        | RBList (Atom (_, "vector") :: vec_args) ->
            SBList (List.map execute vec_args)
        | RBList [ Atom (l, "symbol"); n ] ->
            Atom
              ( l,
                match execute n with
                | Atom (_, x) when String.starts_with ~prefix:"\"" x ->
                    String.sub x 1 (String.length x - 2)
                | n -> failnode __LOC__ [ n ] )
        | RBList [ Atom (l, "quote"); arg ] -> (
            match execute arg with
            | Atom (l, x) -> Atom (l, "'" ^ x)
            | n -> RBList [ Atom (l, "quote"); n ])
        | RBList [ Atom (_, "-"); ea; eb ] -> (
            match (execute ea, execute eb) with
            | Atom (_, a), Atom (_, b) ->
                Atom
                  ( unknown_location,
                    string_of_int (int_of_string a - int_of_string b) )
            | a, b -> failnode __LOC__ [ a; b ])
        | Atom (_, "__FILENAME__") -> Atom (unknown_location, context.filename)
        | Atom (_, "__LINE__") ->
            Atom
              ( unknown_location,
                string_of_int (context.loc.line - context.start_line) )
        | Atom (_, "__POSITION__") ->
            Atom (unknown_location, string_of_int context.loc.pos)
        | Atom (m, x) when String.starts_with ~prefix:"'" x ->
            Atom (m, String.sub x 1 (String.length x - 1))
        (* Args *)
        | Atom (m, x) when StringMap.exists (fun k _ -> k = x) args -> (
            StringMap.find x args |> function
            | Atom (_, arg_val) -> Atom (m, arg_val)
            | x -> x)
        (* /Args *)
        | Atom (_, x)
          when String.starts_with ~prefix:"\"" x
               && String.ends_with ~suffix:"\"" x ->
            Atom (unknown_location, x)
        | Atom (_, x) when int_of_string_opt x |> Option.is_some ->
            Atom (unknown_location, x)
        (* Function call *)
        | RBList (Atom (_, fname) :: args) ->
            let f =
              context.functions |> StringMap.find_opt fname |> function
              | Some x -> x
              | None -> failnode __LOC__ [ node ]
            in
            let fake_macro =
              RBList
                (Atom (unknown_location, "")
                :: Atom (unknown_location, "")
                :: SBList f.params :: f.body)
            in
            args |> List.map execute |> run context fake_macro
        | node -> failnode __LOC__ [ node ]
      in
      body |> List.hd |> execute
  | n -> failnode __LOC__ [ n ]
