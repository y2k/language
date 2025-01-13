open Common

let rec compile (node : sexp) : string =
  match node with
  | SAtom (_, x) when String.starts_with ~prefix:":" x ->
      Printf.sprintf "\"%s\"\n" (String.sub x 1 (String.length x - 1))
  | SAtom (_, x) -> Printf.sprintf "%s\n" x
  | SList (_, xs) -> xs |> List.map compile |> String.concat "" |> Printf.sprintf "(\n%s)\n"

let main (log : bool) (filename : string) prelude_macros code =
  let prelude_ctx, prelude_sexp =
    prelude_macros
    |> Frontend.parse_and_simplify
         { empty_context with interpreter = Backend_interpreter.mk_interpret; eval = Backend_interpreter.mk_eval () }
         "prelude"
  in
  let prelude_ctx = Stage_add_def_to_scope.invoke prelude_ctx prelude_sexp |> fst in
  let rec invoke code : sexp =
    let ctx, node = code |> Frontend.desugar { prelude_ctx with log } filename in
    node
    |> try_slog "Parse_and_simplify             ->" log
    |> Stage_simplify_let.invoke
    |> try_slog "Stage_simplify_let             ->" log
    |> Stage_linter.invoke ctx prelude_sexp
    (* *)
    |> Stage_unwrap_ns.invoke (fun cfg -> invoke cfg.code) ctx
    |> try_slog "Stage_unwrap_ns                ->" log
  in
  invoke code |> compile |> String.trim
