open Angstrom
module A = Angstrom

type meta = { line : int; pos : int; symbol : string } [@@deriving show]

let unknown_location = { line = 0; pos = 0; symbol = "" }

type cljexp =
  | Atom of meta * string
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
    A.char '"'
    *> ( A.many1
           (A.char '\\' *> A.char '"'
           >>| Fun.const "\\\""
           <|> (A.satisfy (( <> ) '"') >>| String.make 1))
       >>| fun x -> String.concat "" x )
    <* A.char '"'
  in
  let pmeta =
    A.char '^'
    *> (ptext_
       <|> A.take_while1 (fun x ->
               (x >= 'A' && x <= 'z')
               || x = '.' || x = '(' || x = ')' || x = '-' || x = '>' || x = ':'
               || x = '?'))
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

let string_to_sexp code =
  code
  |> A.parse_string ~consume:All (pnode (find_line_and_pos code))
  |> Result.fold ~ok:Fun.id ~error:(fun error ->
         failwith ("Parse SEXP error: " ^ error))

module StringMap = Map.Make (String)

type context = {
  filename : string;
  loc : meta;
  start_line : int;
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
  |> prerr_endline;
  failwith "Invalid node"

module NameGenerator : sig
  val get_new_var : unit -> string
  val reset : unit -> unit
end = struct
  let index = ref 0
  let reset () = index := 0

  let get_new_var () =
    index := !index + 1;
    "p__" ^ string_of_int !index
end

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
              Atom
                ( unknown_location,
                  string_of_int (context.loc.line - context.start_line) )
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

let rec expand_core_macro (context : context) node : context * cljexp =
  let expand_core_macro1 = expand_core_macro context in
  let expand_core_macro2 x = expand_core_macro context x |> snd in
  let with_context x = (context, x) in
  match node with
  | Atom _ -> node |> with_context
  | CBList xs ->
      RBList
        (Atom (unknown_location, "hash-map") :: List.map expand_core_macro2 xs)
      |> with_context
  | RBList (Atom (_, "let") :: SBList vals :: body) ->
      let unpack_let_args args body =
        let rec loop = function
          | [] -> []
          | (Atom _ as k) :: v :: tail -> k :: expand_core_macro2 v :: loop tail
          | SBList xs :: v :: tail ->
              let temp_val = NameGenerator.get_new_var () in
              let a = [ Atom (unknown_location, temp_val); v ] in
              let b =
                xs
                |> List.fold_left
                     (fun (i, acc) x ->
                       ( i + 1,
                         acc
                         @ [
                             x;
                             RBList
                               [
                                 Atom (unknown_location, "get");
                                 Atom (unknown_location, temp_val);
                                 Atom (unknown_location, string_of_int i);
                               ];
                           ] ))
                     (0, a)
                |> snd
              in
              b @ loop tail
          | xs -> fail_node xs
        in
        RBList (Atom (unknown_location, "let*") :: SBList (loop args) :: body)
      in
      unpack_let_args vals (List.map expand_core_macro2 body) |> with_context
  | RBList (Atom (l, "defn") :: (Atom _ as name) :: SBList args :: body) ->
      RBList
        [
          Atom (l, "def");
          name;
          expand_core_macro2 (RBList (Atom (l, "fn") :: SBList args :: body));
        ]
      |> with_context
  | RBList (Atom (l, "defn-") :: Atom (ln, name) :: SBList args :: body) ->
      RBList
        [
          Atom (l, "def");
          Atom ({ ln with symbol = ":private" }, name);
          expand_core_macro2 (RBList (Atom (l, "fn") :: SBList args :: body));
        ]
      |> with_context
  | RBList [ Atom (_, "__inject_raw_sexp"); x ] -> with_context x
  | RBList ((Atom (_, "proxy") as _a) :: _b :: _c :: body) ->
      let body =
        body
        |> List.map (function
             | Atom _ as x -> x
             | RBList ((Atom _ as x) :: args :: body) ->
                 let body = body |> List.map expand_core_macro2 in
                 RBList (x :: args :: body)
             | x -> fail_node [ x ])
      in
      RBList (_a :: _b :: _c :: body) |> with_context
  | RBList (Atom (l, "case") :: target :: body) ->
      let rec loop = function
        | cond :: then_ :: body ->
            RBList
              [
                Atom (unknown_location, "if");
                RBList
                  [
                    Atom (unknown_location, "=");
                    Atom (unknown_location, "gen_1");
                    cond;
                  ];
                then_;
                loop body;
              ]
        | [ x ] -> x
        | _ -> fail_node [ node ]
      in
      RBList
        [
          Atom (l, "let");
          SBList [ Atom (unknown_location, "gen_1"); target ];
          loop body;
        ]
      |> expand_core_macro1
  | RBList (Atom (_, "cond") :: body) ->
      let rec loop = function
        | [ Atom (_, ":else"); then_ ] -> then_
        | cond :: then_ :: body ->
            RBList [ Atom (unknown_location, "if"); cond; then_; loop body ]
        | _ -> fail_node [ node ]
      in
      loop body |> expand_core_macro2 |> with_context
  | RBList (Atom (l, "if-let") :: tail) ->
      expand_core_macro1 (RBList (Atom (l, "if-let*") :: tail))
  | RBList [ Atom (_, "if-let*"); SBList bindings; then'; else' ] ->
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
      loop bindings |> expand_core_macro1
  | RBList (Atom (l, "fn") :: SBList args :: body) ->
      (let rec loop new_args let_args = function
         | [] -> (new_args, let_args)
         | (Atom _ as x) :: tail -> loop (new_args @ [ x ]) let_args tail
         | SBList xs :: tail ->
             let virt_arg =
               Atom (unknown_location, NameGenerator.get_new_var ())
             in
             let let_args =
               xs
               |> List.fold_left
                    (fun (i, acc) x ->
                      ( i + 1,
                        acc
                        @ [
                            x;
                            RBList
                              [
                                Atom (unknown_location, "get");
                                virt_arg;
                                Atom (unknown_location, string_of_int i);
                              ];
                          ] ))
                    (0, let_args)
               |> snd
             in
             loop (new_args @ [ virt_arg ]) let_args tail
         | xs -> fail_node xs
       in
       let body = List.map expand_core_macro2 body in
       match loop [] [] args with
       | _, [] -> RBList (Atom (l, "fn*") :: SBList args :: body)
       | new_args, let_args ->
           RBList
             [
               Atom (l, "fn*");
               SBList new_args;
               RBList
                 (Atom (unknown_location, "let*") :: SBList let_args :: body);
             ])
      |> with_context
  | RBList (Atom (_, "->") :: body) ->
      body
      |> List.reduce (fun acc x ->
             match x with
             | Atom (l, z) -> RBList [ Atom (l, z); acc ]
             | RBList (a :: bs) -> RBList (a :: acc :: bs)
             | xs -> fail_node [ xs ])
      |> expand_core_macro1
  | RBList (Atom (_, "->>") :: body) ->
      body
      |> List.reduce (fun acc x ->
             match x with
             | Atom (l, z) -> RBList [ acc; Atom (l, z) ]
             | RBList (a :: bs) -> RBList ((a :: bs) @ [ acc ])
             | xs -> fail_node [ xs ])
      |> expand_core_macro1
  | RBList (Atom (l, "defmacro") :: Atom (_, name) :: _) as macro ->
      (* print_endline @@ "[LOG] defmacro: " ^ name; *)
      ( { context with macros = StringMap.add name macro context.macros },
        RBList [ Atom (l, "comment") ] )
  | RBList ((Atom (_, "module") as x) :: body) ->
      RBList (x :: (List.fold_left_map expand_core_macro context body |> snd))
      |> with_context
  | RBList (Atom (_, "ns") :: _) as node -> node |> with_context
  | RBList ((RBList _ as h) :: args) ->
      RBList (expand_core_macro2 h :: List.map expand_core_macro2 args)
      |> with_context
  | RBList (Atom (l, fname) :: target :: args)
    when String.starts_with ~prefix:"." fname && String.length fname > 1 ->
      let mname = String.sub fname 1 (String.length fname - 1) in
      RBList
        (Atom (l, ".")
        :: expand_core_macro2 target
        :: Atom (unknown_location, mname)
        :: List.map expand_core_macro2 args)
      |> with_context
  | RBList (Atom (l, fname) :: args)
    when StringMap.exists (fun n _ -> n = fname) context.macros ->
      (* print_endline @@ "[LOG] call macro: " ^ fname; *)
      MacroInterpretator.run { context with loc = l }
        (StringMap.find fname context.macros)
        args
      |> List.hd |> expand_core_macro1
  | RBList ((Atom (_l, _fname) as x) :: args) ->
      (* print_endline @@ "[LOG] call function: " ^ fname; *)
      (* context.macros
         |> StringMap.iter (fun n _ -> print_endline @@ "[LOG] Macro: " ^ n); *)
      RBList (x :: List.map expand_core_macro2 args) |> with_context
  | SBList body ->
      RBList (Atom (unknown_location, "vector") :: body)
      |> expand_core_macro2 |> with_context
  | x -> fail_node [ x ]

let parse_and_simplify start_line filename code =
  NameGenerator.reset ();
  (* print_endline "==| DEBUG |=============================================="; *)
  let sexp =
    match string_to_sexp ("(module " ^ code ^ ")") with
    | [ x ] -> x
    | _ -> failwith "Illegal state"
  in
  expand_core_macro
    { filename; loc = unknown_location; start_line; macros = StringMap.empty }
    sexp
  |> function
  | ctx, x -> (ctx, x)
