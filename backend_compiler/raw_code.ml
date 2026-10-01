open Frontend

let error code reason =
  let meta = match code with SAtom (meta, _) | SList (meta, _, _) -> meta in
  failwith (Printf.sprintf "raw-code: %s at line %d, column %d" reason meta.loc.line meta.loc.column)

let payload = function
  | SList (_, _, [ SAtom (_, "raw-code"); SAtom (_, text) ]) as code ->
      let length = String.length text in
      if length >= 2 && text.[0] = '"' && text.[length - 1] = '"' then String.sub text 1 (length - 2)
      else error code "expected a string literal"
  | code -> error code "expected exactly one string literal"
