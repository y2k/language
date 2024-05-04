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

type function_decl = { params : cljexp list; body : cljexp list }

type context = {
  filename : string;
  loc : meta;
  start_line : int;
  macros : cljexp StringMap.t;
  functions : function_decl StringMap.t;
}

let empty_context =
  {
    filename = "";
    loc = unknown_location;
    start_line = 0;
    macros = StringMap.empty;
    functions = StringMap.empty;
  }

module List = struct
  include List

  let reduce f xs =
    match xs with
    | [] -> failwith "[REDUCE] List is empty"
    | xs -> List.fold_left f (List.hd xs) (List.tl xs)

  let reduce_opt f xs = match xs with [] -> None | xs -> Some (reduce f xs)
end

let failnode prefix es =
  es |> List.map show_cljexp |> List.fold_left ( ^ ) ""
  |> Printf.sprintf "Can't parse:\n-------\n%s\n-------"
  |> prerr_endline;
  failwith ("Invalid node [" ^ prefix ^ "]")

(* let log_sexp prefix node =
   print_endline @@ prefix ^ " " ^ show_cljexp node;
   node *)

let rec unpack_to_map = function
  | [] -> []
  | Atom (_, k) :: v :: tail -> (k, v) :: unpack_to_map tail
  | n -> failnode __LOC__ n

module NameGenerator = struct
  type _ Effect.t += CreateVal : string Effect.t

  let with_scope f =
    let index = ref 0 in
    let open Effect.Deep in
    Effect.Deep.try_with f ()
      {
        effc =
          (fun (type a) (eff : a Effect.t) ->
            match eff with
            | CreateVal ->
                index := !index + 1;
                let result = "p__" ^ string_of_int !index in
                Some (fun (k : (a, _) continuation) -> continue k result)
            | _ -> None);
      }

  let get_new_var () = Effect.perform CreateVal
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
      | a, b -> failnode __LOC__ (List.concat [ a; b ])
    in
    compute_args' StringMap.empty arg_names arg_values

  let rec run (context : context) (macro : cljexp) (macro_args : cljexp list) :
      cljexp =
    match macro with
    | RBList (_ :: _ :: SBList macro_arg_names :: body) ->
        let rec execute (node : cljexp) : cljexp =
          let args = compute_args macro_arg_names macro_args in
          (* print_endline @@ "LOG: " ^ show_cljexp node; *)
          match node with
          | RBList [ Atom (_, "transform_nodes"); CBList opt; xs ] ->
              let xs =
                match execute xs with
                | RBList xs -> xs
                | n -> failnode __LOC__ [ n ]
              in
              let sep =
                unpack_to_map opt |> List.assoc_opt ":sep"
                |> (function Some x -> x | None -> failnode __LOC__ opt)
                |> execute
              in
              let len = List.length xs in
              let r =
                xs
                |> List.mapi (fun i x -> (i, x))
                |> List.concat_map (fun (i, x) ->
                       if i < len - 1 then [ x; sep ] else [ x ])
              in
              RBList r
          | RBList [ Atom (_, "vec"); Atom (_, xs) ] -> (
              match StringMap.find xs args with
              | RBList xs -> SBList xs
              | x -> failnode __LOC__ [ x ])
          | RBList (Atom (_, "concat") :: xs) ->
              let r =
                xs |> List.map execute
                |> List.concat_map (function
                     | RBList xs -> xs
                     | n -> failnode __LOC__ [ n ])
              in
              RBList r
          | RBList (Atom (_, "str") :: str_args) ->
              let result =
                str_args |> List.map execute
                |> List.map (function
                     | Atom (_, x)
                       when String.starts_with ~prefix:"\"" x
                            && String.ends_with ~suffix:"\"" x ->
                         String.sub x 1 (String.length x - 2)
                     | Atom (_, x) -> x
                     | n -> failnode __LOC__ [ n ])
                |> String.concat ""
              in
              Atom (unknown_location, "\"" ^ result ^ "\"")
          | RBList (Atom (_, "list") :: list_args) ->
              RBList (List.map execute list_args)
          | RBList (Atom (_, "vector") :: vec_args) ->
              SBList (List.map execute vec_args)
          | RBList [ Atom (l, "symbol"); n ] ->
              Atom
                ( l,
                  match execute n with
                  | Atom (_, x) when String.starts_with ~prefix:"\"" x ->
                      String.sub x 1 (String.length x - 2)
                  | n -> failnode __LOC__ [ n ] )
          | RBList [ Atom (l, "quote"); arg ] -> (
              match execute arg with
              | Atom (l, x) -> Atom (l, "'" ^ x)
              | n -> RBList [ Atom (l, "quote"); n ])
          | RBList [ Atom (_, "-"); ea; eb ] -> (
              match (execute ea, execute eb) with
              | Atom (_, a), Atom (_, b) ->
                  Atom
                    ( unknown_location,
                      string_of_int (int_of_string a - int_of_string b) )
              | a, b -> failnode __LOC__ [ a; b ])
          | Atom (_, "__FILENAME__") -> Atom (unknown_location, context.filename)
          | Atom (_, "__LINE__") ->
              Atom
                ( unknown_location,
                  string_of_int (context.loc.line - context.start_line) )
          | Atom (_, "__POSITION__") ->
              Atom (unknown_location, string_of_int context.loc.pos)
          | Atom (m, x) when String.starts_with ~prefix:"'" x ->
              Atom (m, String.sub x 1 (String.length x - 1))
          (* Args *)
          | Atom (m, x) when StringMap.exists (fun k _ -> k = x) args -> (
              StringMap.find x args |> function
              | Atom (_, arg_val) -> Atom (m, arg_val)
              | x -> x)
          (* /Args *)
          | Atom (_, x)
            when String.starts_with ~prefix:"\"" x
                 && String.ends_with ~suffix:"\"" x ->
              Atom (unknown_location, x)
          | Atom (_, x) when int_of_string_opt x |> Option.is_some ->
              Atom (unknown_location, x)
          (* Function call *)
          | RBList (Atom (_, fname) :: args) ->
              let f =
                context.functions |> StringMap.find_opt fname |> function
                | Some x -> x
                | None -> failnode __LOC__ [ node ]
              in

              let args_value = args |> List.map execute in

              (* failnode __LOC__
                   [
                     RBList (args |> StringMap.to_list |> List.map snd);
                     RBList _args;
                     RBList f.params;
                     RBList f.body;
                   ]
                 |> ignore; *)
              let fake_macro =
                RBList
                  (Atom (unknown_location, "")
                  :: Atom (unknown_location, "")
                  :: SBList f.params :: f.body)
              in

              run context fake_macro args_value
          | node -> failnode __LOC__ [ node ]
        in
        body |> List.hd |> execute
    | n -> failnode __LOC__ [ n ]
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
      |> expand_core_macro2 |> with_context
  (* | RBList ((Atom (_, "comment") as c) :: _ :: _) ->
      RBList [ c ] |> with_context *)
  | RBList (Atom (_, "fn*") :: _) as o -> o |> with_context
  | RBList (Atom (_, "let*") :: _) as o -> o |> with_context
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
                             expand_core_macro2
                               (RBList
                                  [
                                    Atom (unknown_location, "get");
                                    Atom (unknown_location, temp_val);
                                    Atom (unknown_location, string_of_int i);
                                  ]);
                           ] ))
                     (0, a)
                |> snd
              in
              b @ loop tail
          | xs -> failnode __LOC__ xs
        in
        RBList (Atom (unknown_location, "let*") :: SBList (loop args) :: body)
      in
      unpack_let_args vals (List.map expand_core_macro2 body) |> with_context
  (* Define function *)
  | RBList (Atom (l, "defn") :: (Atom (_, fname) as name) :: SBList args :: body)
    ->
      let fbody =
        expand_core_macro2 (RBList (Atom (l, "fn") :: SBList args :: body))
      in
      let new_body = RBList [ Atom (l, "def"); name; fbody ] in
      let context =
        {
          context with
          functions =
            context.functions |> StringMap.add fname { params = args; body };
        }
      in
      (context, new_body)
  | RBList (Atom (l, "defn-") :: Atom (ln, name) :: SBList args :: body) ->
      RBList
        [
          Atom (l, "def");
          Atom ({ ln with symbol = ":private" }, name);
          expand_core_macro2 (RBList (Atom (l, "fn") :: SBList args :: body));
        ]
      |> with_context
  | RBList [ Atom (_, "__inject_raw_sexp"); x ] -> with_context x
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
        | _ -> failnode __LOC__ [ node ]
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
        | _ -> failnode __LOC__ [ node ]
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
         | CBList map_items :: tail ->
             let virt_arg =
               Atom (unknown_location, NameGenerator.get_new_var ())
             in
             let rec loop2 = function
               | arg :: key :: tail ->
                   arg
                   :: expand_core_macro2
                        (RBList
                           [ Atom (unknown_location, "get"); virt_arg; key ])
                   :: loop2 tail
               | [] -> []
               | xs -> failnode __LOC__ xs
             in
             loop (new_args @ [ virt_arg ]) (loop2 map_items) tail
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
                            expand_core_macro2
                              (RBList
                                 [
                                   Atom (unknown_location, "get");
                                   virt_arg;
                                   Atom (unknown_location, string_of_int i);
                                 ]);
                          ] ))
                    (0, let_args)
               |> snd
             in
             loop (new_args @ [ virt_arg ]) let_args tail
         | xs -> failnode __LOC__ xs
       in
       let body = List.map expand_core_macro2 body in
       match loop [] [] args with
       | new_args, [] -> RBList (Atom (l, "fn*") :: SBList new_args :: body)
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
             | xs -> failnode __LOC__ [ xs ])
      |> expand_core_macro1
  | RBList (Atom (_, "->>") :: body) ->
      body
      |> List.reduce (fun acc x ->
             match x with
             | Atom (l, z) -> RBList [ acc; Atom (l, z) ]
             | RBList (a :: bs) -> RBList ((a :: bs) @ [ acc ])
             | xs -> failnode __LOC__ [ xs ])
      |> expand_core_macro1
  | RBList (Atom (l, "defmacro") :: Atom (_, name) :: _) as macro ->
      (* print_endline @@ "[LOG] defmacro: " ^ name; *)
      ( { context with macros = StringMap.add name macro context.macros },
        RBList [ Atom (l, "comment") ] )
  | RBList ((Atom (_, "module") as x) :: body) ->
      let ctx2, exp2 = List.fold_left_map expand_core_macro context body in
      let xs =
        x :: exp2
        |> List.concat_map (function
             | RBList (Atom (_, "module") :: xs) -> xs
             | x -> [ x ])
      in
      (ctx2, RBList xs)
  | RBList (Atom (_, "ns") :: _) as node -> node |> with_context
  | RBList ((RBList _ as h) :: args) ->
      RBList (expand_core_macro2 h :: List.map expand_core_macro2 args)
      |> with_context
  (* Desugar interop function call *)
  | RBList (Atom (l, fname) :: target :: args)
    when String.starts_with ~prefix:"." fname && String.length fname > 1 ->
      let mname = "'" ^ String.sub fname 1 (String.length fname - 1) in
      RBList
        (Atom (l, ".")
        :: expand_core_macro2 target
        :: Atom (unknown_location, mname)
        :: List.map expand_core_macro2 args)
      |> with_context
  (* Desugar . macro *)
  | RBList (Atom (l, ".") :: target :: Atom (lp, prop) :: args)
    when not (String.starts_with ~prefix:"'" prop) ->
      let args = List.map expand_core_macro2 args in
      RBList (Atom (l, ".") :: target :: Atom (lp, "'" ^ prop) :: args)
      |> with_context
  (* Expand user macro *)
  | RBList (Atom (l, fname) :: args)
    when StringMap.exists (fun n _ -> n = fname) context.macros ->
      (* print_endline @@ "[LOG] call macro: " ^ fname; *)
      MacroInterpretator.run { context with loc = l }
        (StringMap.find fname context.macros)
        args
      |> expand_core_macro1
  | RBList [ Atom (l, name); x ] when String.starts_with ~prefix:":" name ->
      RBList [ Atom (l, "get"); x; Atom (unknown_location, name) ]
      |> expand_core_macro2 |> with_context
  (* Desugar Construtors *)
  | RBList (Atom (l, name) :: xs)
    when name <> "." && String.ends_with ~suffix:"." name ->
      let cnt_name = "\"" ^ String.sub name 0 (String.length name - 1) ^ "\"" in
      RBList
        (Atom (l, "new")
        :: Atom (unknown_location, cnt_name)
        :: List.map expand_core_macro2 xs)
      |> with_context
  | RBList ((Atom (_l, _fname) as x) :: args) ->
      RBList (x :: List.map expand_core_macro2 args) |> with_context
  | SBList xs -> SBList (xs |> List.map expand_core_macro2) |> with_context
  | x -> failnode __LOC__ [ x ]

let parse_and_simplify (prelude_context : context) start_line filename code =
  (* if filename <> "prelude" then
    print_endline "==| DEBUG |==============================================\n"; *)
  let sexp =
    RBList (Atom (unknown_location, "module") :: string_to_sexp code)
  in
  (* if filename <> "prelude" then print_endline (show_cljexp sexp); *)
  expand_core_macro
    {
      filename;
      loc = unknown_location;
      start_line;
      macros = prelude_context.macros;
      functions = prelude_context.functions;
    }
    sexp
  |> function
  | ctx, x ->
      let x =
        match x with
        | RBList (Atom (l, "module") :: xs) -> (
            let xs =
              xs
              |> List.filter (function
                   | RBList [ Atom (_, "comment") ] -> false
                   | _ -> true)
            in
            match xs with [ x ] -> x | xs -> RBList (Atom (l, "module") :: xs))
        | x -> x
      in
      (* if filename <> "prelude" then print_endline (show_cljexp x); *)
      (ctx, x)
