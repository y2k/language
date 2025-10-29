open Common
module StringMap = Map.Make (String)

let rec convert = function
  | SList (m, (SAtom (_, "fn*") as fn) :: SList (ma, args) :: body) ->
      let args = args |> List.map convert |> String.concat " " in
      let cnt =
        body |> List.map convert |> String.concat "\n"
        |> String.split_on_char '\n' |> List.length
      in
      SList
        ( m,
          fn
          :: SAtom (ma, args)
          :: SAtom (meta_empty, string_of_int cnt)
          :: body )
      |> convert
  | SAtom (_, x) -> x
  | SList (_, xs) ->
      xs |> List.map convert |> String.concat "\n" |> Printf.sprintf "(\n%s\n)"

let rec compile_functions (node : sexp) : string StringMap.t =
  match node with
  | SList (_, SAtom (_, "do*") :: xs) ->
      List.fold_left
        (fun acc x ->
          StringMap.union (fun _ _ v2 -> Some v2) acc (compile_functions x))
        StringMap.empty xs
  | SList (_, [ SAtom (_, "def*"); SAtom (_, name); value ]) ->
      let body = convert value in
      StringMap.of_list [ (name, body) ]
  | n -> failsexp __LOC__ [ n ]

let invoke ~builtin_macro ~log code ~filename =
  Frontent_simplify.do_simplify ~builtin_macro (Fun.const [])
    { log; macro = ""; filename; root_dir = "" }
    code
  |> Stage_resolve_ns.do_resolve [] filename ""
  |> log_stage log "Stage_resolve_ns"
  |> compile_functions

let invoke_to_line ~builtin_macro ~log code ~filename =
  invoke ~builtin_macro ~log code ~filename
  |> StringMap.bindings
  |> List.map (fun (name, body) -> Printf.sprintf "%s\n=======\n%s" name body)
  |> String.concat "\n=======\n"
