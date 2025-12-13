open Core__.Common

let rec invoke = function
  | SAtom (m, name) when not (String.starts_with ~prefix:"-" name) ->
      SAtom (m, name |> String.map (fun x -> if x = '-' then '_' else x))
  | SAtom _ as x -> x
  | SList (m, args) -> SList (m, List.map invoke args)
