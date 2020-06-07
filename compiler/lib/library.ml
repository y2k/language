open Angstrom

module Language = struct
  type exp =
    | EInt of int
    | EString of string
    | EConst of string
    | EReadField of string * string
    | EConstructor of string * exp list
    | ECall of (string * string) * exp list
    | EStaticCall of string * string * exp list
    | EInstanceCall of string * exp * exp list
    | EDefn of string * string list * exp list
    | EModule of string * exp list
  [@@deriving show]

  let lbrace = char '('

  let rbrace = char ')'

  let l_sbrace = char '['

  let r_sbrace = char ']'

  let space = take_while1 (function ' ' | '\n' -> true | _ -> false)

  let space_or_empty = take_while (function ' ' | '\n' -> true | _ -> false)

  let number =
    take_while1 (function '0' .. '9' -> true | _ -> false) >>| fun n ->
    EInt (int_of_string n)

  let name = take_while1 (function 'a' .. 'z' -> true | _ -> false)

  let module_name =
    take_while1 (function 'A' .. 'Z' | 'a' .. 'z' -> true | _ -> false)

  let instance_name =
    char '.'
    *> take_while1 (function 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false)

  let namespace_name =
    take_while1 (function 'a' .. 'z' | 'A' .. 'Z' | '.' -> true | _ -> false)

  let fun_name =
    take_while1 (function 'a' .. 'z' | 'A' .. 'Z' | '.' -> true | _ -> false)

  let static_fun_name =
    lift2 (fun ns name -> (ns, name)) (namespace_name <* string "/") fun_name

  let full_fun_name =
    lift2 (fun ns name -> (ns, name)) (namespace_name <* string "/") fun_name
    <|> (fun_name >>| fun n -> ("", n))

  let string_const =
    char '"'
    *> take_while (function
         | 'a' .. 'z' | 'A' .. 'Z' | ' ' | '.' -> true
         | _ -> false)
    <* char '"'
    >>| fun n -> EString n

  let read_field =
    lift2
      (fun record field -> EReadField (record, field))
      (take_while1 (function 'a' .. 'z' -> true | _ -> false) <* char '.')
      (take_while1 (function 'a' .. 'z' -> true | _ -> false))

  let simple_const =
    take_while1 (function 'a' .. 'z' | '0' .. '9' | '.' -> true | _ -> false)
    >>| fun v -> EConst v

  let func_or_atom func_rec =
    choice [ number; string_const; read_field; simple_const; func_rec ]

  let constructor arg =
    lift2
      (fun ctr_parts args ->
        let ctr = String.concat "." ctr_parts in
        EConstructor (ctr, args))
      (many1 (module_name <* char '.'))
      (many (space *> arg))

  let func_rec =
    fix (fun func_rec ->
        lbrace
        *> choice
             [
               lift3
                 (fun name instance children ->
                   EInstanceCall (name, instance, children))
                 instance_name
                 (space *> func_or_atom func_rec)
                 (many (space *> func_or_atom func_rec));
               lift2
                 (fun (cls_name, method_name) children ->
                   EStaticCall (cls_name, method_name, children))
                 static_fun_name
                 (many (space *> func_or_atom func_rec));
               constructor (func_or_atom func_rec);
               lift2
                 (fun fun_name children -> ECall (fun_name, children))
                 full_fun_name
                 (many (space *> func_or_atom func_rec));
             ]
        <* space_or_empty *> rbrace)

  let defn =
    lbrace *> string "defn" *> space *> name >>= fun fname ->
    space *> l_sbrace *> many (space_or_empty *> name) >>= fun args ->
    r_sbrace *> many (space *> func_or_atom func_rec) <* space_or_empty
    >>= fun body -> rbrace *> return (EDefn (fname, args, body))

  let module_exp =
    lbrace *> string "module" *> space
    *> ( module_name <* space >>= fun mname ->
         many1 (space_or_empty *> defn <* space_or_empty) >>| fun xs ->
         EModule (mname, xs) )
    <* rbrace

  let eval expr str = parse_string ~consume:All expr str
end

module App = struct
  let read_file filename =
    let lines = ref [] in
    let chan = open_in filename in
    try
      while true do
        lines := input_line chan :: !lines
      done;
      failwith ""
    with End_of_file ->
      close_in chan;
      List.fold_left (fun a x -> x ^ a) "" !lines

  let example () = read_file "example.clj"

  let rec to_json exp =
    let open Language in
    match exp with
    | EModule (_, funcs) ->
        funcs
        |> List.map (function
             | EDefn (fn, _, _) as f -> (fn, to_json f)
             | _ -> failwith "???")
        |> fun fs -> `Assoc [ ("functions", `Assoc fs) ]
    | EDefn (_, args, body) ->
        `Assoc
          [
            ("argNames", `List (List.map (fun a -> `String a) args));
            ("body", `List (List.map to_json body));
          ]
    | EString x -> `Assoc [ ("@tag", `String "str"); ("value", `String x) ]
    | EInt x -> `Assoc [ ("@tag", `String "int"); ("value", `Int x) ]
    | EConst x -> `Assoc [ ("@tag", `String "var"); ("value", `String x) ]
    | ECall ((_, fname), params) ->
        `Assoc
          [
            ("@tag", `String "call");
            ("funName", `String fname);
            ("params", `List (params |> List.map to_json));
          ]
    | EStaticCall (clName, methodName, params) ->
        `Assoc
          [
            ("@tag", `String "static-call");
            ("className", `String clName);
            ("methodName", `String methodName);
            ("params", `List (params |> List.map to_json));
          ]
    | EInstanceCall (clName, self, params) ->
        `Assoc
          [
            ("@tag", `String "instance-call");
            ("methodName", `String clName);
            ("instance", to_json self);
            ("params", `List (params |> List.map to_json));
          ]
    | EReadField (name, field) ->
        `Assoc
          [
            ("@tag", `String "field");
            ("name", `String name);
            ("field", `String field);
          ]
    | EConstructor (cls, params) ->
        `Assoc
          [
            ("@tag", `String "constructor");
            ("type", `String cls);
            ("params", `List (params |> List.map to_json));
          ]

  let get_json () =
    let text = example () in
    match Language.eval Language.module_exp text with
    | Ok m -> to_json m |> Yojson.to_string
    | Error msg -> Printf.sprintf "%s | %s" msg text
end
