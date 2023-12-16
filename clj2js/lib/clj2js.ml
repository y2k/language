open Angstrom
module A = Angstrom

module List = struct
  include List

  let reduce f xs =
    match xs with
    | [] -> failwith "List is empty"
    | xs -> List.fold_left f (List.hd xs) (List.tl xs)
end

type cljexp =
  | Atom of string
  | RBList of cljexp list
  | SBList of cljexp list
  | CBList of cljexp list
[@@deriving show]

let fail_node es =
  es |> List.map show_cljexp |> List.fold_left ( ^ ) ""
  |> Printf.sprintf "Can't parse:\n-------\n%s\n-------"
  |> failwith

let pnode =
  let pcomment = A.string ";;" *> A.take_while (( <> ) '\n') *> A.char '\n' in
  let pspace = A.many (A.char ' ' <|> A.char '\n' <|> pcomment) in
  A.many1
    (A.fix (fun pnode ->
         A.char '"' *> A.take_while1 (function '"' -> false | _ -> true)
         <* A.char '"'
         >>| (fun x -> Atom ("\"" ^ x ^ "\""))
         <|> ( A.take_while1 (function
                 | ' ' | '\n' | '[' | ']' | '(' | ')' | '{' | '}' -> false
                 | _ -> true)
             >>| fun x -> Atom x )
         <|> (A.char '{' *> (A.many1 (pnode <* pspace) >>| fun xs -> CBList xs)
             <* A.char '}')
         <|> (A.char '(' *> (A.many1 (pnode <* pspace) >>| fun xs -> RBList xs)
             <* A.char ')')
         <|> (A.char '[' *> (A.many (pnode <* pspace) >>| fun xs -> SBList xs)
             <* A.char ']'))
    <* pspace)

let rec compile (node : cljexp) : string =
  let rec parse_vals nodes =
    match nodes with
    | Atom val_name :: val_body :: remain ->
        "const " ^ val_name ^ " = " ^ compile val_body ^ "; "
        ^ parse_vals remain
    | [] -> ""
    | xs -> fail_node xs
  in
  match node with
  (* "Marco function" *)
  | RBList (Atom "str" :: body) ->
      RBList (Atom "+" :: Atom "\"\"" :: body) |> compile
  | RBList (Atom "->" :: body) ->
      body
      |> List.reduce (fun acc x ->
             match x with
             | Atom z -> RBList [ Atom z; acc ]
             | RBList (a :: bs) -> RBList (a :: acc :: bs)
             | xs -> fail_node [ xs ])
      |> compile
  | RBList (Atom "->>" :: body) ->
      body
      |> List.reduce (fun acc x ->
             match x with
             | Atom z -> RBList [ acc; Atom z ]
             | RBList (a :: bs) -> RBList ((a :: bs) @ [ acc ])
             | xs -> fail_node [ xs ])
      |> compile
  | RBList [ Atom "if-let"; SBList bindings; then'; else' ] ->
      let rec loop = function
        | Atom name :: value :: tail ->
            RBList
              [
                Atom "let";
                SBList [ Atom name; value ];
                RBList [ Atom "if"; Atom name; loop tail; else' ];
              ]
        | [] -> then'
        | _ -> failwith "???"
      in
      loop bindings |> compile
  (* Core forms *)
  | Atom x -> x
  | RBList (Atom "try" :: body) ->
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
             | RBList (Atom "catch" :: _) -> false
             | _ -> true)
        |> to_string_with_returns
      in
      let e_name, catch_body =
        body
        |> List.find_map (function
             | RBList (Atom "catch" :: Atom e :: body) ->
                 Some (e, to_string_with_returns body)
             | _ -> None)
        |> Option.get
      in
      Printf.sprintf "(function() { try { %s } catch (%s) { %s } })()" try_body
        e_name catch_body
  | RBList [ Atom "def"; Atom name; body ] ->
      Printf.sprintf "const %s = %s;" name (compile body)
  | RBList [ Atom "<"; a; b ] ->
      Printf.sprintf "(%s < %s)" (compile a) (compile b)
  | RBList [ Atom ">"; a; b ] ->
      Printf.sprintf "(%s > %s)" (compile a) (compile b)
  | RBList [ Atom "<="; a; b ] ->
      Printf.sprintf "(%s <= %s)" (compile a) (compile b)
  | RBList [ Atom ">="; a; b ] ->
      Printf.sprintf "(%s >= %s)" (compile a) (compile b)
  | RBList (Atom "and" :: xs) ->
      xs |> List.map compile
      |> List.reduce (Printf.sprintf "%s && %s")
      |> Printf.sprintf "(%s)"
  | RBList (Atom "or" :: xs) ->
      xs |> List.map compile
      |> List.reduce (Printf.sprintf "%s || %s")
      |> Printf.sprintf "(%s)"
  | SBList xs ->
      xs |> List.map compile
      |> List.reduce (Printf.sprintf "%s, %s")
      |> Printf.sprintf "[%s]"
  | RBList [ Atom "if"; c; a; b ] ->
      Printf.sprintf "(%s) ? (%s) : (%s)" (compile c) (compile a) (compile b)
  | RBList (Atom "comment" :: _) -> ""
  | RBList [ Atom "export-default"; body ] ->
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
        | _ -> failwith "???"
      in
      to_pairs xs |> Printf.sprintf "{ %s }"
  | RBList [ Atom "="; a; b ] -> compile a ^ " == " ^ compile b
  | RBList (Atom "+" :: xs) ->
      xs |> List.map compile
      |> List.reduce (Printf.sprintf "%s + %s")
      |> Printf.sprintf "(%s)"
  | RBList (Atom "-" :: xs) ->
      xs |> List.map compile
      |> List.reduce (Printf.sprintf "%s - %s")
      |> Printf.sprintf "(%s)"
  | RBList [ Atom "first"; body ] ->
      RBList [ Atom ".at"; RBList [ Atom "Array/from"; body ]; Atom "0" ]
      |> compile
  | RBList [ Atom "second"; body ] ->
      RBList [ Atom ".at"; RBList [ Atom "Array/from"; body ]; Atom "1" ]
      |> compile
  | RBList (Atom "defn" :: Atom fname :: SBList args :: body) ->
      let fn = RBList (Atom "fn" :: SBList args :: body) in
      Printf.sprintf "const %s = %s" fname (compile fn)
  | RBList (Atom "while" :: condition :: body) ->
      Printf.sprintf "while (%s) {%s}" (compile condition)
        (body |> List.map compile |> List.reduce (Printf.sprintf "%s;%s"))
  | RBList (Atom "fn" :: SBList args :: body) ->
      let sargs =
        match args with
        | [] -> ""
        | _ ->
            args
            |> List.map (function Atom x -> x | x -> fail_node [ x ])
            |> List.reduce (Printf.sprintf "%s, %s")
      in
      let sbody =
        body |> List.map compile |> List.rev
        |> List.mapi (fun i x -> if i = 0 then "return " ^ x else x)
        |> List.rev
        |> List.reduce (Printf.sprintf "%s; %s")
      in
      Printf.sprintf "(%s) => { %s }" sargs sbody
  | RBList (Atom "let" :: SBList vals :: body) ->
      let svals = parse_vals vals in
      let sbody =
        body |> List.map compile |> List.rev
        |> List.mapi (fun i x -> if i = 0 then "return " ^ x else x)
        |> List.rev
        |> List.reduce (Printf.sprintf "%s; %s")
      in
      "(function () { " ^ svals ^ sbody ^ " })()"
  (* Functions calls *)
  | RBList (Atom fname :: args) ->
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

let main str =
  let result = str |> A.parse_string ~consume:All pnode in
  match result with
  | Ok result ->
      (* result |> List.map show_cljexp
         |> List.reduce (Printf.sprintf "%s\n%s")
         |> Printf.sprintf "%s\n" |> print_endline; *)
      result |> List.map compile |> List.reduce (Printf.sprintf "%s\n%s")
  | Error error -> failwith ("Parse SEXP error: " ^ error)
