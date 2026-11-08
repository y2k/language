open Frontend

let unsupported_expression_forms =
  [
    "raw-code";
    "try";
    "throw";
    ".";
    "new";
    "set!";
    "cast";
    "instance?";
    "compiler/gen-class";
    "export-default";
    "compiler/ns";
    "def";
    "if";
    "let*";
    "fn*";
    "quote";
  ]

let builtins =
  [
    "+";
    "-";
    "*";
    "=";
    "not=";
    "<";
    "<=";
    ">";
    ">=";
    "not";
    "str";
    "list";
    "hash-map";
    "get";
    "count";
    "assoc";
    "conj";
    "map";
    "reduce";
    "atom";
    "deref";
    "reset!";
    "swap!";
  ]

let is_string name = String.length name >= 2 && name.[0] = '"' && name.[String.length name - 1] = '"'
let string_value name = String.sub name 1 (String.length name - 2)
let error message = failwith ("fex: " ^ message)

let is_decimal_integer name =
  let start = if String.starts_with ~prefix:"-" name || String.starts_with ~prefix:"+" name then 1 else 0 in
  let digits = String.sub name start (String.length name - start) in
  digits <> "" && String.for_all (function '0' .. '9' -> true | _ -> false) digits

let component name =
  let output = Buffer.create (String.length name) in
  String.iter
    (function
      | ('a' .. 'z' | '0' .. '9' | '_' | '-') as c -> Buffer.add_char output c
      | 'A' .. 'Z' as c ->
          Buffer.add_char output '!';
          Buffer.add_char output (Char.lowercase_ascii c)
      | c -> Buffer.add_string output (Printf.sprintf "%%%02X" (Char.code c)))
    name;
  Buffer.contents output

let key namespace name = String.concat "." (List.map component (String.split_on_char '.' namespace) @ [ component name ])
let string_record value = [ "string"; (if value = "" then "-" else Source_map.base64 value) ]
let serialize rows = String.concat "" (List.map (fun fields -> String.concat " " fields ^ "\n") rows)

let records namespace aliases globals value =
  let rows = ref [] in
  let count = ref 0 in
  let reserve build =
    incr count;
    let address = string_of_int !count in
    let row = ref [] in
    rows := row :: !rows;
    row := build ();
    address
  in
  let leaf fields = reserve (fun () -> fields) in
  let resolve locals name =
    if List.mem name locals then [ "local"; name ]
    else
      match String.index_opt name '/' with
      | Some i ->
          let prefix = String.sub name 0 i in
          let namespace = Option.value (List.assoc_opt prefix aliases) ~default:prefix in
          [ "global"; key namespace (String.sub name (i + 1) (String.length name - i - 1)) ]
      | None ->
          if List.mem name globals then [ "global"; key namespace name ]
          else if List.mem name builtins then [ "builtin"; name ]
          else [ "global"; key namespace name ]
  in
  let atom locals name =
    if is_string name then string_record (string_value name)
    else if List.mem name [ "nil"; "true"; "false" ] then [ name ]
    else
      match int_of_string_opt name with
      | Some n ->
          if n < -2147483648 || n > 2147483647 then error "integer outside int32 range";
          [ "int"; string_of_int n ]
      | None when is_decimal_integer name -> error "integer outside int32 range"
      | None -> (
          match float_of_string_opt name with
          | Some n ->
              if not (Float.is_finite n) then error "float must be finite";
              [ "float"; Printf.sprintf "%.17g" n ]
          | None -> resolve locals name)
  in
  let rec quote = function
    | SAtom (_, name) -> leaf (string_record (if is_string name then string_value name else name))
    | SList (_, _, items) ->
        reserve (fun () ->
            let callee = leaf [ "builtin"; "list" ] in
            "call" :: callee :: List.map quote items)
  and expr locals = function
    | SAtom (_, name) -> leaf (atom locals name)
    | SList (_, _, [ SAtom (_, "quote"); value ]) -> quote value
    | SList (_, _, SAtom (_, "do") :: items) -> reserve (fun () -> "do" :: List.map (expr locals) items)
    | SList (meta, _, [ SAtom (_, "if"); condition; then_ ]) -> conditional locals condition then_ (SAtom (meta, "nil"))
    | SList (_, _, [ SAtom (_, "if"); condition; then_; else_ ]) -> conditional locals condition then_ else_
    | SList (meta, _, SAtom (_, "let*") :: SList (_, _, bindings) :: body) ->
        let rec lower = function
          | [] -> []
          | pattern :: value :: rest -> Lowering_expression_to_statement.lower_binding meta pattern value @ lower rest
          | _ -> error "expected binding pairs"
        in
        bindings_body locals (lower bindings) body
    | SList (meta, _, SAtom (_, "fn*") :: SList (_, _, params) :: body) ->
        let params, bindings =
          List.split
            (List.map
               (function
                 | SAtom (_, name) -> (name, [])
                 | pattern ->
                     let name = Gensym.gensym_string meta in
                     (name, Lowering_expression_to_statement.lower_binding meta pattern (SAtom (meta, name))))
               params)
        in
        reserve (fun () -> "fn" :: bindings_body (params @ locals) (List.concat bindings) body :: params)
    | SList (_, _, SAtom (_, name) :: _) when List.mem name unsupported_expression_forms ->
        error ("unsupported expression: " ^ name)
    | SList (_, _, callee :: args) ->
        reserve (fun () ->
            let callee = expr locals callee in
            "call" :: callee :: List.map (expr locals) args)
    | SList _ -> error "empty expression"
  and conditional locals condition then_ else_ =
    reserve (fun () -> "if" :: List.map (expr locals) [ condition; then_; else_ ])
  and body_expr locals = function
    | [ value ] -> expr locals value
    | values -> reserve (fun () -> "do" :: List.map (expr locals) values)
  and bindings_body locals bindings body =
    match bindings with
    | [] -> body_expr locals body
    | SList (_, _, [ SAtom (_, "let*"); SAtom (_, name); value ]) :: rest ->
        reserve (fun () ->
            let value = expr locals value in
            let body = bindings_body (name :: locals) rest body in
            [ "let"; name; value; body ])
    | _ -> error "expected symbol binding"
  in
  ignore (expr [] value);
  List.rev_map (fun row -> !row) !rows

let definitions forms =
  Gensym.run (fun () ->
      let namespace, aliases, forms =
        match forms with
        | SList (_, _, [ SAtom (_, "compiler/ns"); SAtom (_, namespace); SList (_, _, requires); SList (_, _, imports) ])
          :: rest ->
            if imports <> [] then error "host imports are unsupported";
            let aliases =
              List.map
                (function
                  | SList (_, _, [ SAtom (_, required); SAtom (_, alias) ]) ->
                      let required = string_value required in
                      if is_string required then error "host requires are unsupported";
                      (string_value alias, required)
                  | _ -> error "malformed namespace require")
                requires
            in
            (string_value namespace, aliases, rest)
        | _ -> ("user", [], forms)
      in
      let definitions =
        List.map
          (function
            | SList (_, _, [ SAtom (_, "def"); SAtom (_, name); value ]) -> (name, value)
            | _ -> error "expected top-level def or initial namespace")
          forms
      in
      let globals = List.map fst definitions in
      List.map (fun (name, value) -> (key namespace name, records namespace aliases globals value)) definitions)

let compile forms =
  definitions forms
  |> List.map (fun (key, rows) -> key ^ "\n" ^ Source_map.base64 (serialize rows) ^ "\n")
  |> String.concat ""
