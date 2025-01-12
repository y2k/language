open Common

let rec compile (node : cljexp) : string =
  match node with
  | Atom (_, x) when String.starts_with ~prefix:":" x ->
      Printf.sprintf "\"%s\"\n" (String.sub x 1 (String.length x - 1))
  | Atom (_, x) -> Printf.sprintf "%s\n" x
  | RBList (_, xs) -> xs |> List.map compile |> String.concat "" |> Printf.sprintf "(\n%s)\n"
  | SBList (_, xs) -> xs |> List.map compile |> String.concat "" |> Printf.sprintf "[\n%s]\n"
  | CBList (_, xs) -> xs |> List.map compile |> String.concat "" |> Printf.sprintf "{\n%s}\n"

let main (log : bool) (filename : string) prelude_macros code =
  let prelude_ctx, prelude_sexp =
    prelude_macros
    |> Frontend.parse_and_simplify
         { empty_context with interpreter = Backend_interpreter.mk_interpret; eval = Backend_interpreter.mk_eval () }
         "prelude"
  in
  let prelude_ctx = Stage_add_def_to_scope.invoke prelude_ctx prelude_sexp |> fst in
  let rec invoke code =
    let ctx, node = code |> Frontend.parse_and_simplify { prelude_ctx with log } filename in
    node
    |> try_log "Parse_and_simplify      ->" log
    |> Stage_simplify_let.invoke
    |> try_log "Stage_simplify_let      ->" log
    |> Stage_normalize_bracket.invoke
    |> try_log "Stage_normalize_bracket ->" log
    |> Stage_linter.invoke ctx prelude_sexp
    |> Stage_unwrap_ns.invoke (fun cfg -> invoke cfg.code) ctx
    |> try_log "Stage_unwrap_ns         ->" log
  in
  invoke code |> compile |> String.trim
