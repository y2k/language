open Common

let rec compile (node : cljexp) : string =
  match node with
  | Atom (_, x) -> Printf.sprintf "%s\n" x
  | RBList xs ->
      xs |> List.map compile |> String.concat "" |> Printf.sprintf "(\n%s)\n"
  | SBList xs ->
      xs |> List.map compile |> String.concat "" |> Printf.sprintf "[\n%s]\n"
  | CBList xs ->
      xs |> List.map compile |> String.concat "" |> Printf.sprintf "{\n%s}\n"

let main (log : bool) (filename : string) prelude_macros code =
  let macros_ctx =
    prelude_macros
    |> Frontend.parse_and_simplify
         { empty_context with interpreter = Backend_interpreter.interpret }
         "prelude"
    |> fst
  in
  code |> Frontend.parse_and_simplify { macros_ctx with log } filename
  (* |> run_linter prelude_macros filename *)
  |> fun (_, exp) -> compile exp |> String.trim
