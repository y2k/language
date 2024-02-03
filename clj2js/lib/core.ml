open Angstrom
module A = Angstrom

type location = { line : int; pos : int; symbol : string } [@@deriving show]

let unknown_location = { line = 0; pos = 0; symbol = "" }

type cljexp =
  | Atom of location * string
  | RBList of cljexp list
  | SBList of cljexp list
  | CBList of cljexp list
[@@deriving show]

let pnode find_line_and_pos =
  let pcomment = A.string ";;" *> A.take_while (( <> ) '\n') *> A.char '\n' in
  let pspace = A.many (A.char ' ' <|> A.char '\n' <|> pcomment) in
  let patom =
    A.both A.pos
      (A.take_while1 (function
        | ' ' | '\n' | '[' | ']' | '(' | ')' | '{' | '}' -> false
        | _ -> true))
    >>| fun (pos, x) ->
    let line, pos = find_line_and_pos pos in
    ({ line; pos; symbol = "" }, x)
  in
  let patom_ = patom >>| fun (m, a) -> Atom (m, a) in
  let ptext_ =
    A.char '"' *> A.take_while1 (function '"' -> false | _ -> true)
    <* A.char '"'
  in
  let pmeta =
    A.char '^'
    *> (ptext_
       <|> A.take_while1 (fun x ->
               (x >= 'A' && x <= 'z')
               || x = '.' || x = '(' || x = ')' || x = '-' || x = '>'))
  in
  let patom_meta =
    A.map2 (pmeta <* pspace) patom ~f:(fun m (a, x) ->
        Atom ({ a with symbol = m }, x))
  in
  let ptext = ptext_ >>| fun x -> Atom (unknown_location, "\"" ^ x ^ "\"") in
  A.many1
    (A.fix (fun pnode ->
         pspace
         *> (ptext <|> patom_meta <|> patom_
            <|> (A.char '{' *> (A.many (pnode <* pspace) >>| fun xs -> CBList xs)
                <* A.char '}')
            <|> (A.char '('
                 *> (A.many1 (pnode <* pspace) >>| fun xs -> RBList xs)
                <* A.char ')')
            <|> (A.char '[' *> (A.many (pnode <* pspace) >>| fun xs -> SBList xs)
                <* A.char ']'))
         <* pspace))

let find_line_and_pos str index =
  let length = String.length str in
  let rec aux i line pos =
    if i >= length || i >= index then (line, pos)
    else if String.get str i = '\n' then aux (i + 1) (line + 1) 1
    else aux (i + 1) line (pos + 1)
  in
  aux 0 1 1

module StringMap = Map.Make (String)

type context = {
  filename : string;
  loc : location;
  macros : cljexp StringMap.t;
}

module List = struct
  include List

  let reduce f xs =
    match xs with
    | [] -> failwith "List is empty"
    | xs -> List.fold_left f (List.hd xs) (List.tl xs)

  let reduce_opt f xs = match xs with [] -> None | xs -> Some (reduce f xs)
end

let fail_node es =
  es |> List.map show_cljexp |> List.fold_left ( ^ ) ""
  |> Printf.sprintf "Can't parse:\n-------\n%s\n-------"
  |> failwith

let expand_core_macro node =
  match node with
  | RBList (Atom (_, "cond") :: body) ->
      let rec loop = function
        | [ Atom (_, ":else"); then_ ] -> then_
        | cond :: then_ :: body ->
            RBList [ Atom (unknown_location, "if"); cond; then_; loop body ]
        | _ -> fail_node [ node ]
      in
      loop body
  | RBList (Atom (_, "->") :: body) ->
      body
      |> List.reduce (fun acc x ->
             match x with
             | Atom (l, z) -> RBList [ Atom (l, z); acc ]
             | RBList (a :: bs) -> RBList (a :: acc :: bs)
             | xs -> fail_node [ xs ])
  | RBList (Atom (_, "->>") :: body) ->
      body
      |> List.reduce (fun acc x ->
             match x with
             | Atom (l, z) -> RBList [ acc; Atom (l, z) ]
             | RBList (a :: bs) -> RBList ((a :: bs) @ [ acc ])
             | xs -> fail_node [ xs ])
  | RBList [ Atom (_, "if-let"); SBList bindings; then'; else' ] ->
      let rec loop = function
        | Atom (l, name) :: value :: tail ->
            RBList
              [
                Atom (l, "let");
                SBList [ Atom (unknown_location, name); value ];
                RBList
                  [
                    Atom (unknown_location, "if");
                    Atom (unknown_location, name);
                    loop tail;
                    else';
                  ];
              ]
        | [] -> then'
        | _ ->
            failwith @@ "if-let has wrong signature [" ^ show_cljexp node ^ "] "
            ^ __LOC__
      in
      loop bindings
  | _ -> node

module MacroInterpretator = struct
  let compute_args (arg_names : cljexp list) (arg_values : cljexp list) :
      cljexp StringMap.t =
    let rec compute_args' acc arg_names arg_values =
      match (arg_names, arg_values) with
      | [ Atom (_, "&"); Atom (_, name) ], vt ->
          StringMap.add name (RBList vt) acc
      | Atom (_, name) :: nt, v :: vt ->
          compute_args' (StringMap.add name v acc) nt vt
      | [], [] -> acc
      | a, b -> fail_node (List.concat [ a; b ])
    in
    compute_args' StringMap.empty arg_names arg_values

  let run (context : context) (macro : cljexp) (macro_args : cljexp list) :
      cljexp list =
    match macro with
    | RBList (_ :: _ :: SBList macro_arg_names :: body) ->
        let rec execute (node : cljexp) : cljexp =
          let args = compute_args macro_arg_names macro_args in
          match node with
          | RBList [ Atom (_, "concat"); a; b ] -> (
              match (execute a, execute b) with
              | RBList a2, RBList b2 -> RBList (List.concat [ a2; b2 ])
              | a2, b2 -> fail_node [ a2; b2 ])
          | RBList (Atom (_, "str") :: str_args) ->
              let result =
                str_args |> List.map execute
                |> List.map (function
                     | Atom (_, x)
                       when String.starts_with ~prefix:"\"" x
                            && String.ends_with ~suffix:"\"" x ->
                         String.sub x 1 (String.length x - 2)
                     | Atom (_, x) -> x
                     | n -> fail_node [ n ])
                |> String.concat ""
              in
              Atom (unknown_location, "\"" ^ result ^ "\"")
          | RBList (Atom (_, "list") :: list_args) ->
              RBList (List.map execute list_args)
          | RBList (Atom (_, "vector") :: vec_args) ->
              SBList (List.map execute vec_args)
          | RBList [ Atom (_, "-"); ea; eb ] -> (
              match (execute ea, execute eb) with
              | Atom (_, a), Atom (_, b) ->
                  Atom
                    ( unknown_location,
                      string_of_int (int_of_string a - int_of_string b) )
              | a, b -> fail_node [ a; b ])
          | Atom (_, "__FILENAME__") -> Atom (unknown_location, context.filename)
          | Atom (_, "__LINE__") ->
              Atom (unknown_location, string_of_int context.loc.line)
          | Atom (_, "__POSITION__") ->
              Atom (unknown_location, string_of_int context.loc.pos)
          | Atom (_, x) when String.starts_with ~prefix:"'" x ->
              Atom (unknown_location, String.sub x 1 (String.length x - 1))
          | Atom (_, x) when StringMap.exists (fun k _ -> k = x) args ->
              StringMap.find x args
          | Atom (_, x)
            when String.starts_with ~prefix:"\"" x
                 && String.ends_with ~suffix:"\"" x ->
              Atom (unknown_location, x)
          | Atom (_, x) when int_of_string_opt x |> Option.is_some ->
              Atom (unknown_location, x)
          | node -> fail_node [ node ]
        in
        body |> List.map execute
    | _ -> failwith "FIXME"
end
