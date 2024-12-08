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

let try_log prefix (log : bool) (node : cljexp) =
  if log then print_endline @@ prefix ^ " " ^ debug_show_cljexp [ node ];
  node

let uncurry f (x, y) = f x y

let main (log : bool) (filename : string) prelude_macros code =
  let macros_ctx =
    prelude_macros
    |> Frontend.parse_and_simplify
         { empty_context with interpreter = Backend_interpreter.interpret }
         "prelude"
    |> uncurry Stage_add_def_to_scope.invoke
    |> fst
  in
  (* macros_ctx.scope
     |> Fun.flip (StringMap.fold (fun k _v a -> a ^ ", " ^ k)) ""
     |> print_endline; *)
  code
  |> Frontend.parse_and_simplify2 { macros_ctx with log } filename
  (* |> run_linter prelude_macros filename *)
  (* |> snd *)
  |> (fun (ctx, s) -> Stage_unwrap_ns.invoke ctx s)
  |> try_log "Stage_unwrap_ns" log
  |> Stage_simplify_let.invoke
  |> try_log "Stage_simplify_let" log
  |> Stage_normalize_bracket.invoke
  |> try_log "Stage_normalize_bracket" log
  |> compile |> String.trim
