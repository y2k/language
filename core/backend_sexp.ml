open Common

let rec convert = function
  | SList (m, (SAtom (_, "fn*") as fn) :: SList (ma, args) :: body) ->
      let args = args |> List.map convert |> String.concat " " in
      SList (m, fn :: SAtom (ma, args) :: body) |> convert
  | SAtom (_, x) -> x
  | SList (_, xs) ->
      xs |> List.map convert |> String.concat "\n" |> Printf.sprintf "(\n%s\n)"

let invoke ~log code =
  Frontent_simplify.do_simplify (Fun.const [])
    { log; macro = ""; filename = ""; root_dir = "" }
    code
  |> Stage_resolve_ns.do_resolve [] "" ""
  |> convert
