open Core__.Common

let escape_name name =
  let len = String.length name in
  if len < 2 then name
  else
    let base = String.sub name 0 (len - 1) in
    let last = name.[len - 1] in
    let base = String.map (fun c -> if c = '-' then '_' else c) base in
    match last with
    | '!' -> base ^ "_BANG_"
    | '?' -> base ^ "_QMARK_"
    | '-' -> base ^ "_"
    | c -> base ^ String.make 1 c

let special_forms =
  [ "fn*"; "do*"; "let*"; "def*"; "if*"; "cast"; "instance?"; "."; "set!" ]

let rec invoke = function
  | SAtom (m, name)
    when (not (String.starts_with ~prefix:"-" name))
         && (not (String.starts_with ~prefix:"\"" name))
         && not (List.mem name special_forms) ->
      SAtom (m, escape_name name)
  | SAtom _ as x -> x
  | SList (m, args) -> SList (m, List.map invoke args)
