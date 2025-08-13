open Common

let rec convert = function
  | SAtom (_, x) -> x
  | SList (_, xs) ->
      xs |> List.map convert |> String.concat "\n" |> Printf.sprintf "(\n%s\n)"

let invoke ~log code =
  Frontent_simplify.do_simplify (Fun.const [])
    { log; macro = ""; filename = ""; root_dir = "" }
    code
  (* |> Stage_resolve_ns.do_resolve "" "" *)
  |> convert
