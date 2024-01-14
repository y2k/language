open Angstrom
module A = Angstrom

module List = struct
  include List

  let reduce f xs =
    match xs with
    | [] -> failwith "List is empty"
    | xs -> List.fold_left f (List.hd xs) (List.tl xs)

  let reduce_opt f xs = match xs with [] -> None | xs -> Some (reduce f xs)
end

type location = { line : int; pos : int } [@@deriving show]

let unknown_location = { line = 0; pos = 0 }

type cljexp =
  | Atom of location * string
  | RBList of cljexp list
  | SBList of cljexp list
  | CBList of cljexp list
[@@deriving show]

let fail_node es =
  es |> List.map show_cljexp |> List.fold_left ( ^ ) ""
  |> Printf.sprintf "Can't parse:\n-------\n%s\n-------"
  |> failwith

let find_line_and_pos str index =
  let length = String.length str in
  let rec aux i line pos =
    if i >= length || i >= index then (line, pos)
    else if String.get str i = '\n' then aux (i + 1) (line + 1) 1
    else aux (i + 1) line (pos + 1)
  in
  aux 0 1 1

let pnode find_line_and_pos =
  let pcomment = A.string ";;" *> A.take_while (( <> ) '\n') *> A.char '\n' in
  let pspace = A.many (A.char ' ' <|> A.char '\n' <|> pcomment) in
  A.many1
    (A.fix (fun pnode ->
         A.char '"' *> A.take_while1 (function '"' -> false | _ -> true)
         <* A.char '"'
         >>| (fun x -> Atom (unknown_location, "\"" ^ x ^ "\""))
         <|> ( A.both A.pos
                 (A.take_while1 (function
                   | ' ' | '\n' | '[' | ']' | '(' | ')' | '{' | '}' -> false
                   | _ -> true))
             >>| fun (pos, x) ->
               let line, pos = find_line_and_pos pos in
               Atom ({ line; pos }, x) )
         <|> (A.char '{' *> (A.many (pnode <* pspace) >>| fun xs -> CBList xs)
             <* A.char '}')
         <|> (A.char '(' *> (A.many1 (pnode <* pspace) >>| fun xs -> RBList xs)
             <* A.char ')')
         <|> (A.char '[' *> (A.many (pnode <* pspace) >>| fun xs -> SBList xs)
             <* A.char ']'))
    <* pspace)

type context = { filename : string }

let rec compile_ (context : context) (node : cljexp) : string =
  let compile node = compile_ context node in
  let rec parse_vals nodes =
    match nodes with
    | Atom (_, val_name) :: val_body :: remain ->
        "const " ^ val_name ^ " = " ^ compile val_body ^ "; "
        ^ parse_vals remain
    | [] -> ""
    | xs -> fail_node xs
  in
  match node with
  (* "Marco function" *)
  | Atom (loc, "FIXME") ->
      RBList
        [
          Atom (loc, "throw");
          RBList
            [
              Atom (unknown_location, "Error.");
              Atom
                ( unknown_location,
                  Printf.sprintf {|"Not implemented %s:%i:%i"|} context.filename
                    loc.line loc.pos );
            ];
        ]
      |> compile
  | RBList (Atom (loc, "FIXME") :: body) ->
      RBList
        [
          Atom (loc, "throw");
          RBList
            [
              Atom (unknown_location, "Error.");
              RBList
                (Atom (unknown_location, "str")
                :: Atom
                     ( unknown_location,
                       Printf.sprintf {|"FIXME %s:%i:%i - "|} context.filename
                         loc.line (loc.pos - 1) )
                :: body);
            ];
        ]
      |> compile
  | RBList (Atom (l, "println") :: body) ->
      RBList (Atom (l, "console/info") :: body) |> compile
  | RBList [ Atom (_, "concat"); a; b ] ->
      Printf.sprintf "[...%s, ...%s]" (compile a) (compile b)
  | RBList [ Atom (_, "conj"); a; b ] ->
      Printf.sprintf "[...%s, %s]" (compile a) (compile b)
  | RBList [ Atom (_, "spread"); a ] -> Printf.sprintf "...%s" (compile a)
  | RBList [ Atom (_, "merge"); a; b ] ->
      Printf.sprintf "{ ...%s, ...%s }" (compile a) (compile b)
  | RBList [ Atom (_, "assoc"); Atom (_, map); Atom (_, key); value ]
    when String.starts_with ~prefix:":" key ->
      Printf.sprintf "{ ...%s, %s: %s }" map
        (String.sub key 1 (String.length key - 1))
        (compile value)
  | RBList [ Atom (_, "__unsafe_insert_js"); Atom (_, body) ]
    when String.starts_with ~prefix:"\"" body
         && String.ends_with ~suffix:"\"" body ->
      String.sub body 1 (String.length body - 2)
  | RBList (Atom (l, "str") :: body) ->
      RBList (Atom (l, "+") :: Atom (unknown_location, "\"\"") :: body)
      |> compile
  | RBList (Atom (_, "->") :: body) ->
      body
      |> List.reduce (fun acc x ->
             match x with
             | Atom (l, z) -> RBList [ Atom (l, z); acc ]
             | RBList (a :: bs) -> RBList (a :: acc :: bs)
             | xs -> fail_node [ xs ])
      |> compile
  | RBList (Atom (_, "->>") :: body) ->
      body
      |> List.reduce (fun acc x ->
             match x with
             | Atom (l, z) -> RBList [ acc; Atom (l, z) ]
             | RBList (a :: bs) -> RBList ((a :: bs) @ [ acc ])
             | xs -> fail_node [ xs ])
      |> compile
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
      loop bindings |> compile
  (* Core forms *)
  | Atom (_, x) when String.starts_with ~prefix:":" x ->
      "\"" ^ String.sub x 1 (String.length x - 1) ^ "\""
  | Atom (_, x) -> x
  | RBList [ Atom (_, "not="); a; b ] ->
      Printf.sprintf "%s != %s" (compile a) (compile b)
  | RBList [ Atom (_, "throw"); ex ] ->
      Printf.sprintf "(function(){throw %s})()" (compile ex)
  | RBList (Atom (_, "try") :: body) ->
      let to_string_with_returns nodes =
        let count = List.length nodes in
        nodes
        |> List.mapi (fun i x ->
               let l = compile x in
               Some (if i < count - 1 then l else "return " ^ l))
        |> List.filter_map Fun.id
        |> List.reduce (Printf.sprintf "%s\n%s")
      in
      let try_body =
        body
        |> List.filter (function
             | RBList (Atom (_, "catch") :: _) -> false
             | _ -> true)
        |> to_string_with_returns
      in
      let e_name, catch_body =
        body
        |> List.find_map (function
             | RBList (Atom (_, "catch") :: Atom (_, e) :: body) ->
                 Some (e, to_string_with_returns body)
             | _ -> None)
        |> Option.get
      in
      Printf.sprintf "(function() { try { %s } catch (%s) { %s } })()" try_body
        e_name catch_body
  | RBList [ Atom (_, "def"); Atom (_, name); body ] ->
      Printf.sprintf "const %s = %s;" name (compile body)
  | RBList [ Atom (_, "<"); a; b ] ->
      Printf.sprintf "(%s < %s)" (compile a) (compile b)
  | RBList [ Atom (_, ">"); a; b ] ->
      Printf.sprintf "(%s > %s)" (compile a) (compile b)
  | RBList [ Atom (_, "<="); a; b ] ->
      Printf.sprintf "(%s <= %s)" (compile a) (compile b)
  | RBList [ Atom (_, ">="); a; b ] ->
      Printf.sprintf "(%s >= %s)" (compile a) (compile b)
  | RBList (Atom (_, "and") :: xs) ->
      xs |> List.map compile
      |> List.reduce (Printf.sprintf "%s && %s")
      |> Printf.sprintf "(%s)"
  | RBList (Atom (_, "or") :: xs) ->
      xs |> List.map compile
      |> List.reduce (Printf.sprintf "%s || %s")
      |> Printf.sprintf "(%s)"
  | SBList xs ->
      xs |> List.map compile
      |> List.reduce_opt (Printf.sprintf "%s, %s")
      |> Option.value ~default:"" |> Printf.sprintf "[%s]"
  | RBList [ Atom (_, "if"); c; a; b ] ->
      Printf.sprintf "(%s) ? (%s) : (%s)" (compile c) (compile a) (compile b)
  | RBList (Atom (_, "comment") :: _) -> ""
  | RBList [ Atom (_, "export-default"); body ] ->
      Printf.sprintf "export default %s" (compile body)
  | CBList xs ->
      let rec to_pairs = function
        | k :: v :: xs ->
            let a = compile k in
            let kn =
              if String.starts_with a ~prefix:":" then
                String.sub a 1 (String.length a - 1)
              else a
            in
            let b = kn ^ ": " ^ compile v in
            let tail = to_pairs xs in
            if tail == "" then b else b ^ ", " ^ tail
        | [] -> ""
        | _ -> failwith __LOC__
      in
      to_pairs xs |> Printf.sprintf "{%s}"
  | RBList [ Atom (_, "="); a; b ] -> compile a ^ " == " ^ compile b
  | RBList (Atom (_, "+") :: xs) ->
      xs |> List.map compile
      |> List.reduce (Printf.sprintf "%s + %s")
      |> Printf.sprintf "(%s)"
  | RBList (Atom (_, "-") :: xs) ->
      xs |> List.map compile
      |> List.reduce (Printf.sprintf "%s - %s")
      |> Printf.sprintf "(%s)"
  | RBList (Atom (_, "*") :: xs) ->
      xs |> List.map compile
      |> List.reduce (Printf.sprintf "%s * %s")
      |> Printf.sprintf "(%s)"
  | RBList [ Atom (_, "/"); a; b ] ->
      Printf.sprintf "(%s / %s)" (compile a) (compile b)
  | RBList [ Atom (l, "first"); body ] ->
      RBList
        [
          Atom (l, ".at");
          RBList [ Atom (unknown_location, "Array/from"); body ];
          Atom (unknown_location, "0");
        ]
      |> compile
  | RBList [ Atom (l, "second"); body ] ->
      RBList
        [
          Atom (l, ".at");
          RBList [ Atom (unknown_location, "Array/from"); body ];
          Atom (unknown_location, "1");
        ]
      |> compile
  | RBList (Atom (l, "defn") :: Atom (_, fname) :: SBList args :: body) ->
      let fn = RBList (Atom (l, "fn") :: SBList args :: body) in
      Printf.sprintf "export const %s = %s" fname (compile fn)
  | RBList (Atom (l, "defn-") :: Atom (_, fname) :: SBList args :: body) ->
      let fn = RBList (Atom (l, "fn") :: SBList args :: body) in
      Printf.sprintf "const %s = %s" fname (compile fn)
  | RBList (Atom (_, "while") :: condition :: body) ->
      Printf.sprintf "while (%s) {%s}" (compile condition)
        (body |> List.map compile |> List.reduce (Printf.sprintf "%s;%s"))
  | RBList (Atom (_, "fn") :: SBList args :: body) ->
      let sargs =
        match args with
        | [] -> ""
        | _ ->
            args
            |> List.map (function Atom (_, x) -> x | x -> fail_node [ x ])
            |> List.reduce (Printf.sprintf "%s, %s")
      in
      let sbody =
        body |> List.map compile |> List.rev
        |> List.mapi (fun i x -> if i = 0 then "return " ^ x else x)
        |> List.rev
        |> List.reduce (Printf.sprintf "%s; %s")
      in
      Printf.sprintf "(%s) => { %s }" sargs sbody
  | RBList (Atom (_, "let") :: SBList vals :: body) ->
      let svals = parse_vals vals in
      let sbody =
        body |> List.map compile |> List.rev
        |> List.mapi (fun i x -> if i = 0 then "return " ^ x else x)
        |> List.rev
        |> List.reduce (Printf.sprintf "%s; %s")
      in
      "(function () { " ^ svals ^ sbody ^ " })()"
  (* Functions calls *)
  | RBList (Atom (_, fname) :: args) ->
      if String.starts_with ~prefix:"." fname then
        let mname = String.sub fname 1 (String.length fname - 1) in
        let this = List.hd args |> compile in
        let sargs =
          match args with
          | [ _ ] -> "()"
          | args ->
              args
              |> List.filteri (fun i _ -> i >= 1)
              |> List.map compile
              |> List.reduce (Printf.sprintf "%s, %s")
              |> Printf.sprintf "(%s)"
        in
        this ^ "." ^ mname ^ sargs
      else if String.ends_with ~suffix:"." fname then
        let cnst_name = String.sub fname 0 (String.length fname - 1) in
        let fargs =
          if List.length args = 0 then ""
          else args |> List.map compile |> List.reduce (Printf.sprintf "%s, %s")
        in
        fargs |> Printf.sprintf "new %s(%s)" cnst_name
      else
        let sargs =
          if List.length args = 0 then ""
          else args |> List.map compile |> List.reduce (Printf.sprintf "%s, %s")
        in
        String.map (function '/' -> '.' | x -> x) fname ^ "(" ^ sargs ^ ")"
  | x -> fail_node [ x ]

let main (filename : string) str =
  let result =
    str |> A.parse_string ~consume:All (pnode (find_line_and_pos str))
  in
  match result with
  | Ok result ->
      (* result |> List.map show_cljexp
         |> List.reduce (Printf.sprintf "%s\n%s")
         |> Printf.sprintf "%s\n" |> print_endline; *)
      result
      |> List.map (compile_ { filename })
      |> List.reduce (Printf.sprintf "%s\n%s")
  | Error error -> failwith ("Parse SEXP error: " ^ error)
