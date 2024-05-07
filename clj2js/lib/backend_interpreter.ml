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

let rec interpret (context : context) (node : cljexp) : context * cljexp =
  let interpret_ x = interpret context x |> snd in
  let with_context x = (context, x) in
  match node with
  | Atom (m, x) when String.starts_with ~prefix:"'" x ->
      (context, Atom (m, String.sub x 1 (String.length x - 1)))
  | Atom (_, v) as x
    when v = "true" || v = "false"
         || String.starts_with ~prefix:"\"" v
         || String.starts_with ~prefix:":" v
         || String.starts_with ~prefix:"'" v
         ||
         let ch = String.get v 0 in
         ch >= '0' && ch <= '9' ->
      (context, x)
  | RBList [ Atom (_, "transform_nodes"); CBList opt; xs ] ->
      let xs =
        match interpret_ xs with RBList xs -> xs | n -> failnode __LOC__ [ n ]
      in
      let sep =
        unpack_to_map opt |> List.assoc_opt ":sep"
        |> (function Some x -> x | None -> failnode __LOC__ opt)
        |> interpret_
      in
      let len = List.length xs in
      let r =
        xs
        |> List.mapi (fun i x -> (i, x))
        |> List.concat_map (fun (i, x) ->
               if i < len - 1 then [ x; sep ] else [ x ])
      in
      (context, RBList r)
  | RBList [ Atom (_, "vec"); x ] -> (
      match interpret_ x with
      | RBList xs -> (context, SBList xs)
      | x -> failnode __LOC__ [ x ])
  | RBList (Atom (_, "concat") :: xs) ->
      let r =
        xs |> List.map interpret_
        |> List.concat_map (function
             | RBList xs -> xs
             | SBList xs -> xs
             | n -> failnode __LOC__ [ n ])
      in
      (context, RBList r)
  | RBList (Atom (_, "str") :: str_args) ->
      let result =
        str_args |> List.map interpret_
        |> List.map (function
             | Atom (_, x)
               when String.starts_with ~prefix:"\"" x
                    && String.ends_with ~suffix:"\"" x ->
                 String.sub x 1 (String.length x - 2)
             | Atom (_, x) -> x
             | n -> failnode __LOC__ [ n ])
        |> String.concat ""
      in
      (context, Atom (unknown_location, "\"" ^ result ^ "\""))
  | RBList (Atom (_, "list") :: list_args) ->
      (context, RBList (List.map interpret_ list_args))
  | RBList (Atom (_, "vector") :: vec_args) ->
      (context, SBList (List.map interpret_ vec_args))
  | SBList vec_args -> (context, SBList (List.map interpret_ vec_args))
  | RBList [ Atom (l, "symbol"); n ] ->
      Atom
        ( l,
          match interpret_ n with
          | Atom (_, x) when String.starts_with ~prefix:"\"" x ->
              String.sub x 1 (String.length x - 2)
          | n -> failnode __LOC__ [ n ] )
      |> with_context
  | RBList [ Atom (l, "quote"); arg ] -> (
      match arg with
      | Atom (l, x) -> (context, Atom (l, "'" ^ x))
      | n -> (context, RBList [ Atom (l, "quote"); n ]))
  | RBList [ Atom (_, op); ea; eb ]
    when op = "-" || op = "+" || op = "*" || op = "/" -> (
      match (interpret_ ea, interpret_ eb) with
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
          (context, Atom (unknown_location, string_of_int (opf ia ib)))
      | a, b -> failnode __LOC__ [ a; b ])
  (* Constants *)
  | Atom (_, "__FILENAME__") ->
      (context, Atom (unknown_location, context.filename))
  | Atom (_, "__LINE__") ->
      Atom
        (unknown_location, string_of_int (context.loc.line - context.start_line))
      |> with_context
  | Atom (_, "__POSITION__") ->
      (context, Atom (unknown_location, string_of_int context.loc.pos))
  (* /Constants *)
  (* Resolve scope value *)
  | Atom (m, x) when StringMap.exists (fun k _ -> k = x) context.scope -> (
      match StringMap.find x context.scope with
      | Atom (_, arg_val), ctx -> (ctx, Atom (m, arg_val))
      | x, ctx -> (ctx, x))
  (* /Resolve scope value *)
  | RBList (Atom (_, "module") :: body) ->
      let context2, results =
        body |> List.fold_left_map (fun ctx x -> interpret ctx x) context
      in
      let last_node = results |> List.rev |> List.hd in
      (context2, last_node)
  | RBList [ Atom (_, "def"); Atom (_, name); body ] ->
      let context =
        {
          context with
          scope = context.scope |> StringMap.add name (body, context);
        }
      in
      (context, RBList [ Atom (unknown_location, "comment") ])
  | RBList [ Atom (_, "="); a; b ] -> (
      match (interpret_ a, interpret_ b) with
      | Atom (_, a), Atom (_, b) ->
          Atom (unknown_location, string_of_bool (String.equal a b))
          |> with_context
      | _ -> failnode __LOC__ [ node ])
  | RBList [ Atom (_, "if"); c; a; b ] -> (
      match interpret_ c with
      | Atom (_, "true") -> (context, interpret_ a)
      | Atom (_, "false") -> (context, interpret_ b)
      | n -> failnode __LOC__ [ n ])
  | RBList (Atom (_, "let*") :: SBList bindings :: body) ->
      let scope =
        bindings |> List.split_into_pairs
        |> List.fold_left
             (fun ctx (k, v) ->
               let name =
                 match k with Atom (_, s) -> s | n -> failnode __LOC__ [ n ]
               in
               let v = interpret { context with scope = ctx } v |> snd in
               StringMap.add name (v, context) ctx)
             context.scope
      in
      let _, results =
        body
        |> List.fold_left_map
             (fun ctx x -> interpret ctx x)
             { context with scope }
      in
      results |> List.rev |> List.hd |> with_context
  | RBList [ Atom (_, "get"); map; key ] -> (
      let map = interpret_ map in
      let key = interpret_ key in
      match (map, key) with
      | CBList xs, Atom (_, key) ->
          List.split_into_pairs xs
          |> List.find (fun (k, _) ->
                 match k with
                 | Atom (_, k) -> k = key
                 | n -> failnode __LOC__ [ n ])
          |> snd |> with_context
      | SBList xs, Atom (_, i) when int_of_string_opt i |> Option.is_some ->
          List.nth xs (int_of_string i) |> with_context
      | m, k -> failnode __LOC__ [ m; k ])
  | CBList xs -> CBList (List.map interpret_ xs) |> with_context
  | RBList (Atom (_, "fn*") :: _ :: _) as x -> x |> with_context
  (* Function call *)
  | RBList (target :: args) ->
      let arg_values = List.map interpret_ args in
      let arg_names, f_body, f_ctx =
        match interpret context target with
        | f_ctx, RBList (Atom (_, "fn*") :: SBList args :: body) ->
            ( List.map
                (function Atom (_, x) -> x | n -> failnode __LOC__ [ n ])
                args,
              body,
              f_ctx )
        | _ ->
            (* prerr_endline @@ show_context context; *)
            failnode __LOC__ [ node ]
      in

      (* Add function arguments to function scope *)
      let scope =
        List.fold_left2
          (fun ctx k v -> StringMap.add k (v, context) ctx)
          f_ctx.scope arg_names arg_values
      in

      let _, results =
        f_body
        |> List.fold_left_map
             (fun ctx x -> interpret ctx x)
             { f_ctx with scope }
      in
      results |> List.rev |> List.hd |> with_context
  | node -> failnode __LOC__ [ node ]

let run_linter prelude_macros filename (ctx, exp) =
  (ctx, Linter.lint interpret prelude_macros filename exp)

let main (filename : string) prelude_macros code =
  let macros_ctx =
    prelude_macros
    |> Frontend.parse_and_simplify
         { empty_context with interpreter = interpret }
         "prelude"
    |> fst
  in
  code
  |> Frontend.parse_and_simplify macros_ctx filename
  |> run_linter prelude_macros filename
  |> (fun (ctx, exp) -> interpret ctx exp |> snd)
  |> show_sexp |> String.trim
