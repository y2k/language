open Lib__.Common
open Common

type simplify_opt = {
  log : bool;
  macro : string;
  filename : string;
  root_dir : string;
  compile : string -> sexp;
}

type simplify_ctx = {
  otp : simplify_opt;
  log : bool;
  get_macro : (string * (obj list -> obj)) list;
}

let sexp_to_obj = function
  | SAtom (m, _) as x -> OQuote (m, x)
  | SList (m, _) as x -> OQuote (m, x)

(* let log_stage (opt : simplify_opt) title node =
  (if opt.log then
     let padding = String.make (max 0 (30 - String.length title)) ' ' in
     prerr_endline @@ "* " ^ title ^ padding ^ " -> " ^ debug_show_sexp [ node ]);
  node *)

let rec simplify (ctx : simplify_ctx) (sexp : sexp) : sexp =
  let get_macro name = ctx.get_macro |> List.assoc_opt ("macro_" ^ name) in
  if ctx.log && false then
    prerr_endline @@ "SIMPLIFY: " ^ debug_show_sexp [ sexp ];
  match sexp with
  | SAtom _ as x -> x
  | SList (_, []) as x -> x
  | SList (m, SAtom (_, "ns") :: _ :: args) ->
      args
      |> Macro_ns.invoke m
           { root_dir = ctx.otp.root_dir; filename = ctx.otp.filename }
      |> simplify ctx
  | SList (m, SAtom (mif, "if") :: if_args) ->
      SList (m, SAtom (mif, "if*") :: List.map (simplify ctx) if_args)
  | SList (m, SAtom (mdo, "do") :: body) ->
      SList (m, SAtom (mdo, "do*") :: List.map (simplify ctx) body)
  | SList (_, SAtom (_, "let") :: SList (_, SAtom (_, "vector") :: _) :: _) ->
      Macro_let.invoke (simplify ctx) ctx sexp
  | SList (m, SAtom (ml, "let") :: name :: value) ->
      let value = List.map (simplify ctx) value in
      SList (m, SAtom (ml, "let*") :: name :: value)
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
  | SList (_, SAtom (_, "fn") :: _) as node ->
      node
      (* |> log_stage ctx.otp.log "[Macro fn BEFORE]" *)
      |> Macro_fn.invoke (simplify ctx)
      (* |> log_stage ctx.otp.log "[Macro fn AFTER]" *)
  | SList (m, [ SAtom (dm, "def"); k; v ]) ->
      SList (m, [ SAtom (dm, "def*"); k; simplify ctx v ])
  (* Macro call *)
  | SList (_, SAtom (_, name) :: args) when get_macro name <> None ->
      let f =
        match get_macro name with Some x -> x | None -> failwith __LOC__
      in
      let args = args |> List.map sexp_to_obj in
      let result = f args in
      let result = OUtils.obj_to_sexp result in
      (* prerr_endline @@ "MACRO RESULT: " ^ debug_show_sexp [ result ]; *)
      let result = simplify ctx result in
      (* prerr_endline @@ "MACRO RESULT(SIMPLE): " ^ debug_show_sexp [ result ]; *)
      (* prerr_endline @@ "LOG2: " ^ debug_show_sexp [ result ]; *)
      (* let result = Lib__.Stage_convert_if_to_statment.invoke result in *)
      (* prerr_endline @@ "LOG3: " ^ debug_show_sexp [ result ]; *)
      (* compile ctx result *)
      result
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
  | SList (_, SAtom (_, name) :: _) when String.ends_with ~suffix:"*" name ->
      failsexp __LOC__ [ sexp ]
  (* *)
  | SList (_, SAtom (_, "gen-class") :: args) -> Macro_gen_class.invoke args
  (* Invoke keyword *)
  | SList (m, [ (SAtom (_, name) as k); arg ])
    when String.starts_with ~prefix:":" name ->
      SList (m, [ SAtom (meta_empty, "get"); arg; k ]) |> simplify ctx
  (* Function call *)
  | SList (m, fn :: args) ->
      let args = List.map (simplify ctx) args in
      let fn = simplify ctx fn in
      SList (m, fn :: args)

let do_simplify eval_macro (opt : simplify_opt) (code : string) : sexp =
  let node = Frontent_parser.parse_text code in
  let macro =
    Frontent_parser.parse_text opt.macro
    |> simplify { log = false; otp = opt; get_macro = [] }
  in
  let do_simplify_inner root_dir filename type_ macro node =
    let opt = { opt with filename; root_dir } in
    let log_stage = log_stage opt.log in
    node
    |> log_stage (type_ ^ "Parse ")
    |> simplify { log = opt.log; otp = opt; get_macro = macro }
    |> log_stage (type_ ^ "Simplify ")
    (* |> Stage_resolve_ns.do_resolve opt.filename opt.root_dir
    |> log_stage opt (type_ ^ "Stage_resolve_ns") *)
    (* |> Stage_load_require.do_invoke (fun x ->
           opt.compile x |> do_simplify_inner root_dir filename "[REQUIRE] " [])
    |> log_stage opt (type_ ^ "Stage_load_require") *)
    (* |> Stage_flat_do.invoke
    |> log_stage (type_ ^ "Stage_flat_do") *)
  in
  let macro_fn_list =
    eval_macro (do_simplify_inner "" "macro.clj" "  [MACRO] " [] macro)
  in
  node |> do_simplify_inner opt.root_dir opt.filename "[SIMPLE] " macro_fn_list
