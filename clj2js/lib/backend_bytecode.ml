open Common

let rec compile (node : cljexp) : string =
  match node with
  | Atom (_, x) when String.starts_with ~prefix:":" x ->
      Printf.sprintf "\"%s\"\n" (String.sub x 1 (String.length x - 1))
  | Atom (_, x) -> Printf.sprintf "%s\n" x
  | RBList xs ->
      xs |> List.map compile |> String.concat "" |> Printf.sprintf "(\n%s)\n"
  | SBList xs ->
      xs |> List.map compile |> String.concat "" |> Printf.sprintf "[\n%s]\n"
  | CBList xs ->
      xs |> List.map compile |> String.concat "" |> Printf.sprintf "{\n%s}\n"

let uncurry f (x, y) = f x y

let main (log : bool) (filename : string) prelude_macros code =
  let macro_sexp =
    prelude_macros
    |> Frontend.parse_and_simplify
         { empty_context with interpreter = Backend_interpreter.interpret }
         "prelude"
  in
  let macros_ctx = macro_sexp |> uncurry Stage_add_def_to_scope.invoke |> fst in
  (* macros_ctx.scope
     |> Fun.flip (StringMap.fold (fun k _v a -> a ^ ", " ^ k)) ""
     |> print_endline; *)
  let ctx, node =
    code |> Frontend.parse_and_simplify { macros_ctx with log } filename
  in
  node
  |> try_log "Parse_and_simplify      ->" log
  |> Stage_simplify_let.invoke
  |> try_log "Stage_simplify_let      ->" log
  |> Stage_normalize_bracket.invoke
  |> try_log "Stage_normalize_bracket ->" log
  |> Stage_linter.invoke ctx (snd macro_sexp)
  |> Stage_unwrap_ns.invoke ctx
  |> try_log "Stage_unwrap_ns         ->" log
  |> compile |> String.trim
