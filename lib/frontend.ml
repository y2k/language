open Common

let rec desugar_and_register (context : context) node : context * cljexp =
  (* print_endline @@ "== FRONT == " ^ debug_show_cljexp [ node ]; *)
  let expand_core_macro1 = desugar_and_register context in
  let expand_core_macro2 x = desugar_and_register context x |> snd in
  let with_context x = (context, x) in
  match node with
  | Atom (l, "__POS__") -> Atom (l, Printf.sprintf {|"%s:%d:%d"|} context.filename l.line l.pos) |> with_context
  | Atom (l, x) when String.starts_with ~prefix:"'" x ->
      RBList (unknown_location, [ Atom (unknown_location, "quote*"); Atom (l, String.sub x 1 (String.length x - 1)) ])
      |> expand_core_macro1
  | Atom _ -> node |> with_context
  | CBList (m, xs) -> RBList (m, Atom (unknown_location, "hash-map") :: xs) |> expand_core_macro1
  | RBList (m, Atom (l, "quote") :: x) -> RBList (m, Atom (l, "quote*") :: x) |> with_context
  | RBList (_, Atom (_, "fn*") :: _) as o -> o |> with_context
  | RBList (_, Atom (_, "let*") :: _) as o -> o |> with_context
  | RBList (_, Atom (_, "let") :: _) as n -> Macro_let.invoke desugar_and_register expand_core_macro2 context n
  | RBList (m2, Atom (m, "def") :: xs) -> expand_core_macro1 (RBList (m2, Atom (m, "def*") :: xs))
  (* Define function *)
  | RBList (m, Atom (l, "defn") :: (Atom (_, fname) as name) :: SBList (ma, args) :: body) ->
      let fbody = expand_core_macro2 (RBList (m, Atom (l, "fn") :: SBList (ma, args) :: body)) in
      let new_body = RBList (unknown_location, [ Atom (l, "def*"); name; fbody ]) in
      let context = { context with scope = context.scope |> StringMap.add fname (fbody, ref context) } in
      (context, new_body)
  | RBList (m, Atom (l, "defn-") :: Atom (ln, name) :: rest) ->
      desugar_and_register context
        (RBList (m, Atom (l, "defn") :: Atom ({ ln with symbol = ":private" }, name) :: rest))
  | RBList (_, [ Atom (_, "__inject_raw_sexp"); x ]) -> with_context x
  | RBList (m2, Atom (m, "if") :: tail) -> RBList (m2, Atom (m, "if*") :: tail) |> expand_core_macro1
  | RBList (m, Atom (l, "case") :: target :: body) ->
      let var = NameGenerator.get_new_var () in
      let rec loop = function
        | cond :: then_ :: body ->
            RBList
              ( unknown_location,
                [
                  Atom (unknown_location, "if");
                  RBList (unknown_location, [ Atom (unknown_location, "="); Atom (unknown_location, var); cond ]);
                  then_;
                  loop body;
                ] )
        | [ x ] -> x
        | _ -> failnode __LOC__ [ node ]
      in
      RBList (m, [ Atom (l, "let"); SBList (unknown_location, [ Atom (unknown_location, var); target ]); loop body ])
      |> expand_core_macro1
  | RBList (m, Atom (_, "cond") :: body) ->
      let rec loop = function
        | [ Atom (_, ":else"); then_ ] -> then_
        | cond :: then_ :: body -> RBList (m, [ Atom (unknown_location, "if"); cond; then_; loop body ])
        | _ -> failnode __LOC__ [ node ]
      in
      loop body |> expand_core_macro2 |> with_context
  | RBList (_, [ Atom (_, "and"); a1 ]) -> a1 |> expand_core_macro1
  | RBList (m2, Atom (m, "and") :: a1 :: args) ->
      let var = NameGenerator.get_new_var () in
      RBList
        ( m2,
          [
            Atom (m, "let");
            SBList (unknown_location, [ Atom (m, var); a1 ]);
            RBList
              ( unknown_location,
                [ Atom (m, "if"); Atom (m, var); RBList (unknown_location, Atom (m, "and") :: args); Atom (m, var) ] );
          ] )
      |> expand_core_macro1
  | RBList (_, [ Atom (_, "or"); a1 ]) -> a1 |> expand_core_macro1
  | RBList (m2, Atom (m, "or") :: a1 :: args) ->
      let var = NameGenerator.get_new_var () in
      RBList
        ( m2,
          [
            Atom (m, "let");
            SBList (unknown_location, [ Atom (m, var); a1 ]);
            RBList
              ( unknown_location,
                [ Atom (m, "if"); Atom (m, var); Atom (m, var); RBList (unknown_location, Atom (m, "or") :: args) ] );
          ] )
      |> expand_core_macro1
  | RBList (m, Atom (l, "if-let") :: tail) -> expand_core_macro1 (RBList (m, Atom (l, "if-let*") :: tail))
  | RBList (m, [ Atom (_, "if-let*"); SBList (_, bindings); then_; else_ ]) ->
      let rec loop = function
        | Atom (l, name) :: value :: tail ->
            RBList
              ( m,
                [
                  Atom (l, "let");
                  SBList (unknown_location, [ Atom (unknown_location, name); value ]);
                  RBList
                    ( unknown_location,
                      [ Atom (unknown_location, "if"); Atom (unknown_location, name); loop tail; else_ ] );
                ] )
        | [] -> then_
        | _ -> failwith @@ "if-let has wrong signature [" ^ show_cljexp node ^ "] " ^ __LOC__
      in
      loop bindings |> expand_core_macro1
  | RBList (m, Atom (l, "comment") :: _) -> RBList (m, [ Atom (l, "do*") ]) |> with_context
  | RBList (_, Atom (_, "fn") :: _) as n -> Macro_fn.invoke desugar_and_register expand_core_macro2 context n
  | RBList (_, Atom (_, "->") :: body) ->
      body
      |> List.reduce __LOC__ (fun acc x ->
             match x with
             | Atom (l, z) -> RBList (l, [ Atom (l, z); acc ])
             | RBList (m, a :: bs) -> RBList (m, a :: acc :: bs)
             | xs -> failnode __LOC__ [ xs ])
      |> expand_core_macro1
  | RBList (_, Atom (_, "->>") :: body) ->
      body
      |> List.reduce __LOC__ (fun acc x ->
             match x with
             | Atom (l, z) -> RBList (l, [ acc; Atom (l, z) ])
             | RBList (m, a :: bs) -> RBList (m, (a :: bs) @ [ acc ])
             | xs -> failnode __LOC__ [ xs ])
      |> expand_core_macro1
  | RBList (m, Atom (l, "defmacro") :: Atom (_, name) :: _) as macro ->
      (* print_endline @@ "[LOG] defmacro: " ^ name; *)
      ({ context with macros = StringMap.add name macro context.macros }, RBList (m, [ Atom (l, "comment") ]))
  | RBList (m2, Atom (m, "do") :: body) ->
      let ctx2, exp2 = List.fold_left_map desugar_and_register context body in
      let xs =
        Atom (m, "do*") :: exp2
        |> List.concat_map (function
             | RBList (_, Atom (_, "do") :: xs) -> xs
             | RBList (_, Atom (_, "do*") :: xs) -> xs
             | x -> [ x ])
      in
      (ctx2, RBList (m2, xs))
  | RBList (m2, Atom (m, "do*") :: body) ->
      let ctx2, exp2 = List.fold_left_map desugar_and_register context body in
      let xs =
        Atom (m, "do*") :: exp2
        |> List.concat_map (function
             | RBList (_, Atom (_, "do") :: xs) -> xs
             | RBList (_, Atom (_, "do*") :: xs) -> xs
             | x -> [ x ])
      in
      (ctx2, RBList (m2, xs))
  | RBList (m, (Atom (_, "ns") as ns) :: tail) ->
      RBList
        (m, [ ns; RBList (unknown_location, [ Atom (unknown_location, "quote*"); RBList (unknown_location, tail) ]) ])
      |> with_context
  | RBList (m, (RBList _ as h) :: args) ->
      RBList (m, expand_core_macro2 h :: List.map expand_core_macro2 args) |> with_context
  | RBList (_, [ Atom (_, "eval!"); exp ]) ->
      (* prerr_endline @@ "NODE: [" ^ debug_show_cljexp [ exp ] ^ "]"; *)
      context.eval context exp |> with_context
  (* Desugar interop function call *)
  | RBList (m, Atom (l, fname) :: target :: args) when String.starts_with ~prefix:"." fname && String.length fname > 1
    ->
      let mname = ":" ^ String.sub fname 1 (String.length fname - 1) in
      RBList
        ( m,
          Atom (l, ".")
          :: expand_core_macro2 target
          :: Atom (unknown_location, mname)
          :: List.map expand_core_macro2 args )
      |> with_context
  (* Desugar . macro *)
  (* FIXME *)
  | RBList (m, Atom (l, ".") :: target :: Atom (lp, prop) :: args) when not (String.starts_with ~prefix:":" prop) ->
      let args = List.map expand_core_macro2 args in
      RBList (m, Atom (l, ".") :: target :: Atom (lp, ":" ^ prop) :: args) |> with_context
  (* Call macro *)
  | RBList (m, Atom (l, fname) :: args)
    when StringMap.exists (fun n _ -> n = fname) context.macros
         && not (StringMap.exists (fun k _ -> k = fname) context.scope) ->
      (* print_endline @@ "[LOG] call macro: " ^ fname; *)
      Macro_interpreter.run { context with loc = l } (StringMap.find fname context.macros) args
      |> change_meta m
      (* |> log_sexp "MACRO RESULT: " *)
      |> expand_core_macro1
  | RBList (m, [ Atom (l, name); x ]) when String.starts_with ~prefix:":" name ->
      RBList (m, [ Atom (l, "get"); x; Atom (unknown_location, name) ]) |> expand_core_macro2 |> with_context
  (* Desugar Construtors *)
  | RBList (m, Atom (l, name) :: xs) when name <> "." && String.ends_with ~suffix:"." name ->
      let cnt_name = "\"" ^ String.sub name 0 (String.length name - 1) ^ "\"" in
      RBList (m, Atom (l, "new") :: Atom (unknown_location, cnt_name) :: List.map expand_core_macro2 xs) |> with_context
  | RBList (m, (Atom (_l, _fname) as x) :: args) ->
      (* prerr_endline @@ "LOG: " ^ _fname; *)
      RBList (m, x :: List.map expand_core_macro2 args) |> with_context
  | SBList (m, xs) -> RBList (m, Atom (m, "vector") :: xs) |> expand_core_macro1
  | x -> failnode __LOC__ [ x ]

let remove_comments_from_module = function
  | RBList (m, Atom (l, "do*") :: xs) -> (
      let xs = xs |> List.filter (function RBList (_, [ Atom (_, "comment") ]) -> false | _ -> true) in
      match xs with [ x ] -> x | xs -> RBList (m, Atom (l, "do*") :: xs))
  | x -> x

let parse_and_simplify (prelude_context : context) filename code =
  if prelude_context.log && filename <> "prelude" then
    print_endline "==| DEBUG |==============================================\n";
  let sexp = RBList (unknown_location, Atom (unknown_location, "do*") :: Frontend_parser.string_to_sexp code) in
  (* if prelude_context.log && filename <> "prelude" then print_endline (debug_show_cljexp [ sexp ]); *)
  let ctx, x = desugar_and_register { prelude_context with filename } sexp in
  (ctx, remove_comments_from_module x)
