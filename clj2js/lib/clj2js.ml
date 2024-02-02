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

module StringMap = Map.Make (String)

type context = {
  filename : string;
  loc : location;
  macros : cljexp StringMap.t;
}

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

let rec compile_ (context : context) (node : cljexp) : context * string =
  let compileOut node = compile_ context node in
  let compile node = compile_ context node |> snd in
  let withContext node = (context, node) in
  let rec parse_vals nodes =
    match nodes with
    | Atom (_, val_name) :: val_body :: remain ->
        "const " ^ val_name ^ " = " ^ compile val_body ^ "; "
        ^ parse_vals remain
    | [] -> ""
    | xs -> fail_node xs
  in
  match node with
  (* "Macro function" *)
  | RBList (Atom (_, "cond") :: body) ->
      let rec loop = function
        | [ Atom (_, ":else"); then_ ] -> then_
        | cond :: then_ :: body ->
            RBList [ Atom (unknown_location, "if"); cond; then_; loop body ]
        | _ -> fail_node [ node ]
      in
      loop body |> compileOut
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
      |> compileOut
  | RBList (Atom (_, "->") :: body) ->
      body
      |> List.reduce (fun acc x ->
             match x with
             | Atom (l, z) -> RBList [ Atom (l, z); acc ]
             | RBList (a :: bs) -> RBList (a :: acc :: bs)
             | xs -> fail_node [ xs ])
      |> compileOut
  | RBList (Atom (_, "->>") :: body) ->
      body
      |> List.reduce (fun acc x ->
             match x with
             | Atom (l, z) -> RBList [ acc; Atom (l, z) ]
             | RBList (a :: bs) -> RBList ((a :: bs) @ [ acc ])
             | xs -> fail_node [ xs ])
      |> compileOut
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
      loop bindings |> compileOut
  | RBList [ Atom (l, "first"); body ] ->
      RBList
        [
          Atom (l, ".at");
          RBList [ Atom (unknown_location, "Array/from"); body ];
          Atom (unknown_location, "0");
        ]
      |> compileOut
  | RBList [ Atom (l, "second"); body ] ->
      RBList
        [
          Atom (l, ".at");
          RBList [ Atom (unknown_location, "Array/from"); body ];
          Atom (unknown_location, "1");
        ]
      |> compileOut
  (* Core forms *)
  | RBList [ Atom (_, "not"); arg ] ->
      Printf.sprintf "!%s" (compile arg) |> withContext
  | RBList [ Atom (_, "type"); arg ] ->
      Printf.sprintf "typeof %s" (compile arg) |> withContext
  | RBList [ Atom (_, "set!"); target; value ] ->
      Printf.sprintf "%s = %s;" (compile target) (compile value) |> withContext
  | RBList [ Atom (_, field); target ]
    when String.starts_with ~prefix:".-" field ->
      Printf.sprintf "%s.%s" (compile target)
        (String.sub field 2 (String.length field - 2))
      |> withContext
  | RBList (Atom (_, "require") :: requiries) ->
      requiries
      |> List.map (function
           | SBList [ Atom (_, name); Atom (_, ":as"); Atom (_, alias) ] ->
               Printf.sprintf "import * as %s from './%s.js';" alias
                 (String.map (function '.' -> '/' | ch -> ch) name)
           | _ -> fail_node requiries)
      |> List.reduce (Printf.sprintf "%s\n%s")
      |> withContext
  | RBList
      [
        Atom (_, "import");
        SBList [ Atom (_, name); Atom (_, ":as"); Atom (_, alias) ];
      ] ->
      Printf.sprintf "import * as %s from '%s';" alias
        (String.map (function '.' -> '/' | ch -> ch) name)
      |> withContext
  | RBList (Atom (_, "defmacro") :: Atom (_, name) :: _) as macro ->
      ({ context with macros = StringMap.add name macro context.macros }, "")
  | RBList [ Atom (_, "concat"); a; b ] ->
      Printf.sprintf "[...%s, ...%s]" (compile a) (compile b) |> withContext
  | RBList [ Atom (_, "conj"); a; b ] ->
      Printf.sprintf "[...%s, %s]" (compile a) (compile b) |> withContext
  | RBList [ Atom (_, "spread"); a ] ->
      Printf.sprintf "...%s" (compile a) |> withContext
  | RBList [ Atom (_, "merge"); a; b ] ->
      Printf.sprintf "{ ...%s, ...%s }" (compile a) (compile b) |> withContext
  | RBList [ Atom (_, "assoc"); map; Atom (_, key); value ]
    when String.starts_with ~prefix:":" key ->
      Printf.sprintf "{ ...%s, %s: %s }" (compile map)
        (String.sub key 1 (String.length key - 1))
        (compile value)
      |> withContext
  | RBList [ Atom (_, "__unsafe_insert_js"); Atom (_, body) ]
    when String.starts_with ~prefix:"\"" body
         && String.ends_with ~suffix:"\"" body ->
      String.sub body 1 (String.length body - 2) |> withContext
  | Atom (_, x) when String.starts_with ~prefix:":" x ->
      "\"" ^ String.sub x 1 (String.length x - 1) ^ "\"" |> withContext
  | Atom (_, x) -> x |> withContext
  | RBList [ Atom (_, "not="); a; b ] ->
      Printf.sprintf "%s !== %s" (compile a) (compile b) |> withContext
  | RBList [ Atom (_, "throw"); ex ] ->
      Printf.sprintf "(function(){throw %s})()" (compile ex) |> withContext
  | RBList (Atom (_, "try") :: body) ->
      (let to_string_with_returns nodes =
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
         e_name catch_body)
      |> withContext
  | RBList [ Atom (_, "def"); Atom (_, name); body ] ->
      Printf.sprintf "const %s = %s;" name (compile body) |> withContext
  | RBList [ Atom (_, "<"); a; b ] ->
      Printf.sprintf "(%s < %s)" (compile a) (compile b) |> withContext
  | RBList [ Atom (_, ">"); a; b ] ->
      Printf.sprintf "(%s > %s)" (compile a) (compile b) |> withContext
  | RBList [ Atom (_, "<="); a; b ] ->
      Printf.sprintf "(%s <= %s)" (compile a) (compile b) |> withContext
  | RBList [ Atom (_, ">="); a; b ] ->
      Printf.sprintf "(%s >= %s)" (compile a) (compile b) |> withContext
  | RBList (Atom (_, "and") :: xs) ->
      xs |> List.map compile
      |> List.reduce (Printf.sprintf "%s && %s")
      |> Printf.sprintf "(%s)" |> withContext
  | RBList (Atom (_, "or") :: xs) ->
      xs |> List.map compile
      |> List.reduce (Printf.sprintf "%s || %s")
      |> Printf.sprintf "(%s)" |> withContext
  | SBList xs ->
      xs |> List.map compile
      |> List.reduce_opt (Printf.sprintf "%s, %s")
      |> Option.value ~default:"" |> Printf.sprintf "[%s]" |> withContext
  | RBList [ Atom (_, "if"); c; a; b ] ->
      Printf.sprintf "(%s ? %s : %s)" (compile c) (compile a) (compile b)
      |> withContext
  | RBList (Atom (_, "comment") :: _) -> "" |> withContext
  | RBList [ Atom (_, "export-default"); body ] ->
      Printf.sprintf "export default %s" (compile body) |> withContext
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
      to_pairs xs |> Printf.sprintf "{%s}" |> withContext
  | RBList [ Atom (_, "="); a; b ] ->
      compile a ^ " === " ^ compile b |> withContext
  | RBList (Atom (_, "+") :: xs) ->
      xs |> List.map compile
      |> List.reduce (Printf.sprintf "%s + %s")
      |> Printf.sprintf "(%s)" |> withContext
  | RBList (Atom (_, "-") :: xs) ->
      xs |> List.map compile
      |> List.reduce (Printf.sprintf "%s - %s")
      |> Printf.sprintf "(%s)" |> withContext
  | RBList (Atom (_, "*") :: xs) ->
      xs |> List.map compile
      |> List.reduce (Printf.sprintf "%s * %s")
      |> Printf.sprintf "(%s)" |> withContext
  | RBList [ Atom (_, "/"); a; b ] ->
      Printf.sprintf "(%s / %s)" (compile a) (compile b) |> withContext
  | RBList (Atom (l, "defn") :: Atom (_, fname) :: SBList args :: body) ->
      let fn = RBList (Atom (l, "fn") :: SBList args :: body) in
      Printf.sprintf "export const %s = %s" fname (compile fn) |> withContext
  | RBList (Atom (_, "while") :: condition :: body) ->
      Printf.sprintf "while (%s) {%s}" (compile condition)
        (body |> List.map compile |> List.reduce (Printf.sprintf "%s;%s"))
      |> withContext
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
      Printf.sprintf "(%s) => { %s }" sargs sbody |> withContext
  | RBList (Atom (_, "let") :: SBList vals :: body) ->
      let svals = parse_vals vals in
      let sbody =
        body |> List.map compile |> List.rev
        |> List.mapi (fun i x -> if i = 0 then "return " ^ x else x)
        |> List.rev
        |> List.reduce (Printf.sprintf "%s; %s")
      in
      "(function () { " ^ svals ^ sbody ^ " })()" |> withContext
  (* Functions or Macro calls *)
  | RBList (Atom (l, fname) :: args) ->
      (if String.starts_with ~prefix:"." fname then
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
           else
             args |> List.map compile |> List.reduce (Printf.sprintf "%s, %s")
         in
         fargs |> Printf.sprintf "new %s(%s)" cnst_name
       else if StringMap.exists (fun n _ -> n = fname) context.macros then
         MacroInterpretator.run { context with loc = l }
           (StringMap.find fname context.macros)
           args
         |> List.map compile
         |> List.reduce (Printf.sprintf "%s;\n%s")
       else
         let sargs =
           if List.length args = 0 then ""
           else
             args |> List.map compile |> List.reduce (Printf.sprintf "%s, %s")
         in
         String.map (function '/' -> '.' | x -> x) fname ^ "(" ^ sargs ^ ")")
      |> withContext
  | x -> fail_node [ x ]

let main (filename : string) str =
  let prelude_macros =
    {|(defmacro defn- [name args & body]
        (list 'def name
          (concat
            (list 'fn args)
            body)))
      (defmacro do [& body] (concat (list 'let (vector)) body))
      (defmacro println [& args] (concat (list 'console/info) args))
      (defmacro FIXME [& args]
        (list 'throw
          (list 'Error.
            (concat
              (list
                'str
                (str "FIXME " __FILENAME__ ":" __LINE__ ":" (- __POSITION__ 1) " - "))
              args))))
      (defmacro str [& args] (concat (list '+ "") args))
    |}
    |> A.parse_string ~consume:All (pnode (find_line_and_pos str))
    |> Result.get_ok
  in
  let result =
    str |> A.parse_string ~consume:All (pnode (find_line_and_pos str))
  in
  match result with
  | Ok result ->
      List.concat [ prelude_macros; result ]
      |> List.fold_left_map compile_
           { filename; loc = unknown_location; macros = StringMap.empty }
      |> snd
      |> List.reduce (Printf.sprintf "%s\n%s")
      |> String.trim
  | Error error -> failwith ("Parse SEXP error: " ^ error)
