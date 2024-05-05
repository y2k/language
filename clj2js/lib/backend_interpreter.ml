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

let rec interpret (context : context) (node : cljexp) : cljexp =
  (* let args = compute_args macro_arg_names macro_args in *)
  (* FIXME: *)
  let args = StringMap.empty in
  match node with
  | RBList [ Atom (_, "transform_nodes"); CBList opt; xs ] ->
      let xs =
        match interpret context xs with
        | RBList xs -> xs
        | n -> failnode __LOC__ [ n ]
      in
      let sep =
        unpack_to_map opt |> List.assoc_opt ":sep"
        |> (function Some x -> x | None -> failnode __LOC__ opt)
        |> interpret context
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
        xs
        |> List.map (interpret context)
        |> List.concat_map (function
             | RBList xs -> xs
             | n -> failnode __LOC__ [ n ])
      in
      RBList r
  | RBList (Atom (_, "str") :: str_args) ->
      let result =
        str_args
        |> List.map (interpret context)
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
      RBList (List.map (interpret context) list_args)
  | RBList (Atom (_, "vector") :: vec_args) ->
      SBList (List.map (interpret context) vec_args)
  | SBList vec_args -> SBList (List.map (interpret context) vec_args)
  | RBList [ Atom (l, "symbol"); n ] ->
      Atom
        ( l,
          match (interpret context) n with
          | Atom (_, x) when String.starts_with ~prefix:"\"" x ->
              String.sub x 1 (String.length x - 2)
          | n -> failnode __LOC__ [ n ] )
  | RBList [ Atom (l, "quote"); arg ] -> (
      match (interpret context) arg with
      | Atom (l, x) -> Atom (l, "'" ^ x)
      | n -> RBList [ Atom (l, "quote"); n ])
  | RBList [ Atom (_, op); ea; eb ]
    when op = "-" || op = "+" || op = "*" || op = "/" -> (
      match ((interpret context) ea, (interpret context) eb) with
      | Atom (_, a), Atom (_, b) ->
          let ia = int_of_string a in
          let ib = int_of_string b in
          let opf =
            match op with
            | "+" -> Int.add
            | "-" -> Int.sub
            | "*" -> Int.mul
            | "/" -> Int.div
            | _ -> failnode __LOC__ [ node ]
          in
          Atom (unknown_location, string_of_int (opf ia ib))
      | a, b -> failnode __LOC__ [ a; b ] (* Constants *))
  | Atom (_, "__FILENAME__") -> Atom (unknown_location, context.filename)
  | Atom (_, "__LINE__") ->
      Atom
        (unknown_location, string_of_int (context.loc.line - context.start_line))
  | Atom (_, "__POSITION__") ->
      Atom (unknown_location, string_of_int context.loc.pos)
  (* /Constants *)
  (* Args *)
  | Atom (m, x) when StringMap.exists (fun k _ -> k = x) args -> (
      StringMap.find x args |> function
      | Atom (_, arg_val) -> Atom (m, arg_val)
      | x -> x)
  (* Function call *)
  | RBList (Atom (_, fname) :: args) ->
      let f =
        context.functions |> StringMap.find_opt fname |> function
        | Some x -> x
        | None -> failnode __LOC__ [ node ]
      in
      let _fake_macro =
        RBList
          (Atom (unknown_location, "")
          :: Atom (unknown_location, "")
          :: SBList f.params :: f.body)
      in
      args
      |> List.map (interpret context)
      |> (fun x -> RBList x)
      |> interpret context (* fake_macro *)
  | Atom _ as x -> x
  | node -> failnode __LOC__ [ node ]

let lint_code prelude_macros filename (ctx, exp) =
  (ctx, Linter.lint prelude_macros filename exp)

let rec show_sexp = function
  | Atom (_, x) -> x
  | RBList xs ->
      xs |> List.map show_sexp
      |> List.reduce_opt (Printf.sprintf "%s %s")
      |> Option.value ~default:"" |> Printf.sprintf "(%s)"
  | SBList xs ->
      xs |> List.map show_sexp
      |> List.reduce_opt (Printf.sprintf "%s %s")
      |> Option.value ~default:"" |> Printf.sprintf "[%s]"
  | CBList xs ->
      xs |> List.map show_sexp
      |> List.reduce_opt (Printf.sprintf "%s %s")
      |> Option.value ~default:"" |> Printf.sprintf "{%s}"

let main (filename : string) prelude_macros code =
  let macros_ctx =
    prelude_macros |> Frontend.parse_and_simplify empty_context "prelude" |> fst
  in
  code
  |> Frontend.parse_and_simplify macros_ctx filename
  (* |> lint_code prelude_macros filename *)
  |> (fun (ctx, exp) -> interpret ctx exp)
  |> show_sexp |> String.trim
