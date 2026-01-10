open Core__.Common
open Core__
open Stage__
module StringMap = Map.Make (String)

let rec convert = function
  | SAtom (_, x) when String.starts_with ~prefix:":" x ->
      "\"" ^ String.sub x 1 (String.length x - 1) ^ "\""
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
  Frontend_simplify.do_simplify ~builtin_macro (Fun.const [])
    { log; macro = ""; filename }
    code
  |> Stage_resolve_ns.do_resolve [] filename ""
  |> log_stage log "Stage_resolve_ns"
  |> compile_functions

let invoke_to_line ~builtin_macro ~log code ~filename =
  invoke ~builtin_macro ~log code ~filename
  |> StringMap.bindings
  |> List.map (fun (name, body) -> Printf.sprintf "%s\n=======\n%s" name body)
  |> String.concat "\n=======\n"

let save_to_directory ~builtin_macro ~log code ~filename ~directory =
  let nodes = invoke ~builtin_macro ~log code ~filename in
  StringMap.iter
    (fun name body ->
      let filepath = Filename.concat directory (name ^ ".txt") in
      let oc = open_out filepath in
      output_string oc body;
      close_out oc)
    nodes
