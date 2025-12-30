open Core__.Common

let escape_char = function
  | '-' -> "_"
  | '!' -> "_BANG_"
  | '?' -> "_QMARK_"
  (*
  | '*' -> "_STAR_"
  | '+' -> "_PLUS_"
  | '>' -> "_GT_"
  | '<' -> "_LT_"
  | '=' -> "_EQ_" *)
  | c -> String.make 1 c

let escape_name name =
  String.to_seq name |> Seq.map escape_char |> List.of_seq |> String.concat ""

let rec invoke = function
  | SAtom (m, name)
    when (not (String.starts_with ~prefix:"-" name))
         && not (String.starts_with ~prefix:"\"" name) ->
      SAtom (m, escape_name name)
  | SAtom _ as x -> x
  | SList (m, args) -> SList (m, List.map invoke args)
