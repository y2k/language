open Angstrom
module A = Angstrom
open Common

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

let rec desugar_and_register (context : context) node : context * cljexp =
  let expand_core_macro1 = desugar_and_register context in
  let expand_core_macro2 x = desugar_and_register context x |> snd in
  let with_context x = (context, x) in
  match node with
  | Atom _ -> node |> with_context
  | CBList xs -> CBList (xs |> List.map expand_core_macro2) |> with_context
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
        { context with scope = context.scope |> StringMap.add fname fbody }
      in
      (context, new_body)
  | RBList (Atom (l, "defn-") :: Atom (ln, name) :: SBList args :: body) ->
      let fn =
        expand_core_macro2 (RBList (Atom (l, "fn") :: SBList args :: body))
      in
      ( { context with scope = context.scope |> StringMap.add name fn },
        RBList
          [ Atom (l, "def"); Atom ({ ln with symbol = ":private" }, name); fn ]
      )
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
      let ctx2, exp2 = List.fold_left_map desugar_and_register context body in
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
  (* Expand (call) user macro *)
  | RBList (Atom (l, fname) :: args)
    when StringMap.exists (fun n _ -> n = fname) context.macros ->
      (* print_endline @@ "[LOG] call macro: " ^ fname; *)
      Macro_interpreter.run { context with loc = l }
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

let parse_and_simplify (prelude_context : context) filename code =
  if filename <> "prelude" then
    print_endline "==| DEBUG |==============================================\n";
  let sexp =
    RBList (Atom (unknown_location, "module") :: string_to_sexp code)
  in
  if filename <> "prelude" then print_endline (show_cljexp sexp);
  desugar_and_register { prelude_context with filename } sexp |> function
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
      if filename <> "prelude" then print_endline (show_cljexp x);
      (ctx, x)
