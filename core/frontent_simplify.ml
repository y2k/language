open Common

type simplify_opt = { log : bool; macro : string; filename : string }

type simplify_ctx = {
  otp : simplify_opt;
  log : bool;
  get_macro : (string * (obj list -> obj)) list;
  run_builtin_macro : (sexp -> sexp) -> simplify_ctx -> sexp -> sexp option;
}

let sexp_to_obj = function
  | SAtom (m, _) as x -> OQuote (m, x)
  | SList (m, _) as x -> OQuote (m, x)

let rec simplify (ctx : simplify_ctx) (sexp : sexp) : sexp =
  let get_macro name = ctx.get_macro |> List.assoc_opt ("macro_" ^ name) in
  (* prerr_endline @@ "SIMPLIFY: " ^ debug_show_sexp [ sexp ]; *)
  match sexp with
  | SAtom (m, "__LOC__") ->
      SAtom
        (meta_empty, Printf.sprintf "\"%s:%i:%i\"" ctx.otp.filename m.line m.pos)
  | SAtom _ as x -> x
  | SList (_, []) as x -> x
  (* TODO move to prelude *)
  | SList (m, SAtom (_, "or") :: x :: xs) ->
      (match xs with
        | [] -> x
        | xs ->
            SList
              ( m,
                [
                  SAtom (m, "if");
                  x;
                  x;
                  SList (meta_empty, SAtom (meta_empty, "or") :: xs);
                ] ))
      |> simplify ctx
  | SList (_, [ SAtom (_, "and"); a1 ]) -> a1 |> simplify ctx
  | SList (m2, SAtom (m, "and") :: a1 :: args) ->
      let var = NameGenerator.get_new_var () in
      SList
        ( m2,
          [
            SAtom (m, "let");
            SList
              (meta_empty, [ SAtom (meta_empty, "vector"); SAtom (m, var); a1 ]);
            SList
              ( meta_empty,
                [
                  SAtom (m, "if");
                  SAtom (m, var);
                  SList (meta_empty, SAtom (m, "and") :: args);
                  SAtom (m, var);
                ] );
          ] )
      |> simplify ctx
  | SList (m, SAtom (mif, "if") :: cond_ :: then_ :: else_) ->
      let else_ = match else_ with [] -> [ SAtom (m, "nil") ] | xs -> xs in
      let if_args = [ cond_; then_ ] @ else_ in
      SList (m, SAtom (mif, "if*") :: List.map (simplify ctx) if_args)
  | SList (m, SAtom (mdo, "do") :: body) ->
      SList (m, SAtom (mdo, "do*") :: List.map (simplify ctx) body)
  | SList (m, [ SAtom (md, "def-"); name; value ]) ->
      SList (m, [ SAtom ({ md with symbol = "private" }, "def"); name; value ])
      |> simplify ctx
  | SList (_, SAtom (_, "->") :: body) ->
      body
      |> List.reduce __LOC__ (fun acc x ->
          match x with
          | SAtom (l, z) -> SList (l, [ SAtom (l, z); acc ])
          | SList (m, a :: bs) -> SList (m, a :: acc :: bs)
          | xs -> failsexp __LOC__ [ xs ])
      |> simplify ctx
  | SList (_, SAtom (_, "->>") :: body) ->
      body
      |> List.reduce __LOC__ (fun acc x ->
          match x with
          | SAtom (l, z) -> SList (l, [ acc; SAtom (l, z) ])
          | SList (m, a :: bs) -> SList (m, (a :: bs) @ [ acc ])
          | xs -> failsexp __LOC__ [ xs ])
      |> simplify ctx
  | SList (m, SAtom (mq, "quote") :: x) -> SList (m, SAtom (mq, "quote*") :: x)
  | SList (m, SAtom (md, "defn-") :: name :: args :: body) ->
      SList
        ( m,
          SAtom ({ md with symbol = "private" }, "defn") :: name :: args :: body
        )
      |> simplify ctx
  | SList (m, SAtom (md, "defn") :: name :: args :: body) ->
      SList
        ( m,
          [
            SAtom (md, "def"); name; SList (m, SAtom (md, "fn") :: args :: body);
          ] )
      |> simplify ctx
  | SList (m, [ SAtom (dm, "def"); k; v ]) ->
      SList (m, [ SAtom (dm, "def*"); k; simplify ctx v ])
  (* Macro call *)
  | SList (_, SAtom (_, name) :: args) when get_macro name <> None ->
      let f =
        match get_macro name with Some x -> x | None -> failwith __LOC__
      in
      let result = args |> List.map sexp_to_obj |> f in
      (* prerr_endline @@ "\n[MACRO 1] " ^ OUtils.debug_obj_to_string result; *)
      let result = OUtils.obj_to_sexp result in
      (* prerr_endline @@ "[MACRO 2] " ^ debug_show_sexp [ result ]; *)
      simplify ctx result
  (* Constructor *)
  | SList (m, SAtom (mn, clazz) :: args)
    when String.ends_with ~suffix:"." clazz && clazz <> "." ->
      let clazz = String.sub clazz 0 (String.length clazz - 1) in
      let args = List.map (simplify ctx) args in
      SList (m, SAtom (meta_empty, "new") :: SAtom (mn, clazz) :: args)
  (* Interop method call *)
  | SList (m, SAtom (mn, name) :: instance :: args)
    when String.starts_with ~prefix:"." name && name <> "." ->
      let name = String.sub name 1 (String.length name - 1) in
      SList (m, SAtom (meta_empty, ".") :: instance :: SAtom (mn, name) :: args)
      |> simplify ctx
  (* Handle special forms *)
  | SList (_, SAtom (_, n) :: _) as x
    when n = "def*" || n = "do*" || n = "fn*" || n = "let*" || n = "if*" ->
      x
  | SList (_, SAtom (_, name) :: _)
    when String.ends_with ~suffix:"*" name && name <> "*" ->
      failsexp __LOC__ [ sexp ]
  (* Invoke keyword *)
  | SList (m, [ (SAtom (_, name) as k); arg ])
    when String.starts_with ~prefix:":" name ->
      SList (m, [ SAtom (meta_empty, "get"); arg; k ]) |> simplify ctx
  (* Builtin Macro / Function call *)
  | SList (m, fn :: args) as node -> (
      match ctx.run_builtin_macro (simplify ctx) ctx node with
      | Some macro_result -> macro_result |> simplify ctx
      | None ->
          let args = List.map (simplify ctx) args in
          let fn = simplify ctx fn in
          SList (m, fn :: args))

let do_simplify ~builtin_macro eval_macro (opt : simplify_opt) (code : string) :
    sexp =
  let node = Frontent_parser.parse_text code in
  let macro =
    Frontent_parser.parse_text opt.macro
    |> simplify
         {
           run_builtin_macro = builtin_macro;
           log = false;
           otp = opt;
           get_macro = [];
         }
  in
  let do_simplify_inner filename type_ macro node =
    let opt = { opt with filename } in
    let log_stage = log_stage opt.log in
    node
    |> log_stage (type_ ^ "Parse ")
    |> simplify
         {
           run_builtin_macro = builtin_macro;
           log = opt.log;
           otp = opt;
           get_macro = macro;
         }
    |> log_stage (type_ ^ "Simplify ")
  in
  let macro_fn_list =
    eval_macro (do_simplify_inner "macro.clj" "  [MACRO] " [] macro)
  in
  node |> do_simplify_inner opt.filename "[SIMPLE] " macro_fn_list
