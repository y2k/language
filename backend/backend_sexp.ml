open Core__.Common
open Core__
open Stage__

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

let invoke ~builtin_macro ~log code =
  Frontend_simplify.do_simplify ~builtin_macro (Fun.const [])
    { log; macro = ""; filename = "" }
    code
  |> Stage_resolve_ns_legacy.do_resolve [] "" ""
  |> convert
