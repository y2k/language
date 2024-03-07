module A = Angstrom
open Core

let rec compile_ (context : context) (node : cljexp) : context * string =
  let compile node = compile_ context node |> snd in
  let withContext node = (context, node) in
  match node with
  | Atom (_, x) when String.starts_with ~prefix:":" x ->
      "\"" ^ String.sub x 1 (String.length x - 1) ^ "\"" |> withContext
  | Atom (_, x) when String.starts_with ~prefix:"\"" x -> x |> withContext
  | Atom (_, x) ->
      x |> String.map (function '/' -> '.' | x -> x) |> withContext
  | SBList xs ->
      xs |> List.map compile
      |> List.reduce_opt (Printf.sprintf "%s, %s")
      |> Option.value ~default:""
      |> Printf.sprintf "listOf(%s)"
      |> withContext
  (* ========================== *)
  | RBList (Atom (_, "module") :: body) ->
      body |> List.map compile
      |> List.reduce (Printf.sprintf "%s\n%s")
      |> withContext
  | RBList [ Atom (_, "__unsafe_inject_code"); Atom (_, code) ]
    when String.get code 0 = '\"' ->
      String.sub code 1 (String.length code - 2)
      |> Scanf.unescaped |> withContext
  | RBList (Atom (_, "str") :: args) ->
      args |> List.map compile
      |> List.fold_left (fun acc x -> acc ^ "+" ^ x) {|""|}
      |> Printf.sprintf "(%s)" |> withContext
  | RBList [ Atom (_, "="); a; b ] ->
      compile a ^ " == " ^ compile b |> withContext
  | RBList [ Atom (_, "not="); a; b ] ->
      compile a ^ " != " ^ compile b |> withContext
  | RBList [ Atom (_, "get"); target; index ] ->
      Printf.sprintf "geta(%s, %s)" (compile target) (compile index)
      |> withContext
  | RBList [ Atom (_, "if"); c; a; b ] ->
      Printf.sprintf "if (%s) { %s } else { %s }" (compile c) (compile a)
        (compile b)
      |> withContext
  | RBList
      [
        Atom (_, "gen-class");
        Atom (_, ":name");
        Atom (_, clsName);
        Atom (_, ":extends");
        Atom (_, superCls);
        Atom (_, ":prefix");
        Atom (_, prefix);
        Atom (_, ":methods");
        SBList methods;
      ]
    when String.starts_with ~prefix:"\"" prefix ->
      let prefix = String.sub prefix 1 (String.length prefix - 2) in
      let ms =
        methods
        |> List.map (function
             | SBList [ Atom (_, mname); SBList args; Atom (_, rtype) ] ->
                 let args_ =
                   args
                   |> List.mapi (fun i a ->
                          match a with
                          | Atom (_, a) -> Printf.sprintf "p%i: %s" i a
                          | x -> fail_node [ x ])
                   |> List.reduce (Printf.sprintf "%s, %s")
                 in
                 let args__ =
                   args
                   |> List.mapi (fun i _ -> Printf.sprintf "p%i" i)
                   |> List.reduce (Printf.sprintf "%s, %s")
                 in
                 Printf.sprintf "override fun %s(%s): %s = %s%s(this, %s)" mname
                   args_ rtype prefix mname args__
             | x -> fail_node [ x ])
        |> List.reduce (Printf.sprintf "%s\n%s")
      in
      Printf.sprintf "class %s : %s() { %s }" clsName superCls ms |> withContext
  | RBList [ Atom (_, "as"); value; cls ] ->
      let target = compile cls in
      let target =
        if String.starts_with ~prefix:"\"" target then
          String.sub target 1 (String.length target - 2)
        else target
      in
      Printf.sprintf "(%s as %s)" (compile value) target |> withContext
  | RBList (Atom (_, "ns") :: Atom (_, name) :: ns_params) ->
      let imports =
        ns_params
        |> List.map (function
             | RBList (Atom (_, ":import") :: imports) ->
                 imports
                 |> List.map (function
                      | SBList (Atom (_, pkg) :: classes) ->
                          List.map compile classes
                          |> List.map (fun c ->
                                 Printf.sprintf "import %s.%s;" pkg c)
                          |> List.reduce (Printf.sprintf "%s\n%s")
                      | n -> fail_node [ n ])
                 |> List.reduce (Printf.sprintf "%s\n%s")
             | n -> fail_node [ n ])
        |> List.fold_left (Printf.sprintf "%s\n%s") ""
      in
      Printf.sprintf "package %s;%s\n" name imports |> withContext
  | RBList (Atom (_, "and") :: xs) ->
      xs |> List.map compile
      |> List.reduce (Printf.sprintf "%s && %s")
      |> Printf.sprintf "(%s)" |> withContext
  | RBList (Atom (_, "or") :: xs) ->
      xs |> List.map compile
      |> List.reduce (Printf.sprintf "%s || %s")
      |> Printf.sprintf "(%s)" |> withContext
  | RBList (Atom (_, "+") :: args) ->
      args |> List.map compile
      |> List.reduce (Printf.sprintf "%s, %s")
      |> Printf.sprintf "prelude.plus(%s)"
      |> withContext
  | RBList [ Atom (_, "-"); a; b ] ->
      Printf.sprintf "prelude.minus(%s, %s)" (compile a) (compile b)
      |> withContext
  | RBList [ Atom (_, ">"); a; b ] ->
      Printf.sprintf "(%s > %s)" (compile a) (compile b) |> withContext
  | RBList
      [ Atom (_, "def"); k; RBList (Atom (_, "fn*") :: SBList args :: body) ] ->
      let sargs =
        match args with
        | [] -> ""
        | _ ->
            args
            |> List.map (function
                 | Atom (m, x) ->
                     if m.symbol = "" then x ^ ":Any?" else x ^ ":" ^ m.symbol
                 | x -> fail_node [ x ])
            |> List.reduce (Printf.sprintf "%s, %s")
      in
      let sbody =
        body |> List.map compile |> List.rev
        |> List.mapi (fun i x -> if i = 0 then x else x)
        |> List.rev
        |> List.reduce (Printf.sprintf "%s\n%s")
      in
      let modifier =
        match k with
        | Atom (l, _) when l.symbol = ":private" -> "private "
        | _ -> ""
      in
      let ret_type =
        match k with
        | Atom (l, _) when l.symbol = ":private" -> ""
        | Atom (l, _) when String.length l.symbol > 0 -> ":" ^ l.symbol
        | _ -> ""
      in
      Printf.sprintf "%sfun %s(%s)%s = run { %s };" modifier (compile k) sargs
        ret_type sbody
      |> withContext
  | RBList [ Atom (_, "def"); k; v ] ->
      let modifier =
        match k with
        | Atom (l, _) when l.symbol = ":private" -> "private "
        | _ -> ""
      in
      Printf.sprintf "%sval %s = %s;" modifier (compile k) (compile v)
      |> withContext
  (* | RBList (Atom (l, "fn") :: SBList args :: body) ->
      Core.unpack_args (Atom (l, "fn*")) args body |> compile |> withContext *)
  | RBList (Atom (_, "fn*") :: SBList args :: body) ->
      let sargs =
        match args with
        | [] -> ""
        | _ ->
            args
            |> List.map (function
                 | Atom (m, x) ->
                     if m.symbol = "" then x else x ^ ":" ^ m.symbol
                 | x -> fail_node [ x ])
            |> List.reduce (Printf.sprintf "%s, %s")
            |> Printf.sprintf "%s ->"
      in
      let sbody =
        body |> List.map compile |> List.rev
        |> List.mapi (fun i x -> if i = 0 then x else x)
        |> List.rev
        |> List.reduce (Printf.sprintf "%s; %s")
      in
      Printf.sprintf "{ %s %s }" sargs sbody |> withContext
  (* | RBList (Atom (_, "let") :: SBList vals :: body) ->
      Core.unpack_let_args vals body |> compile |> withContext *)
  | RBList (Atom (_, "let*") :: SBList vals :: body) ->
      let rec parse_vals nodes =
        match nodes with
        | Atom (_, val_name) :: val_body :: remain ->
            "val " ^ val_name ^ " = " ^ compile val_body ^ "; "
            ^ parse_vals remain
        | [] -> ""
        | xs -> fail_node xs
      in
      let svals = parse_vals vals in
      let sbody =
        body |> List.map compile |> List.rev
        |> List.mapi (fun i x -> if i = 0 then "" ^ x else x)
        |> List.rev
        |> List.reduce (Printf.sprintf "%s\n%s")
      in
      "" ^ svals ^ sbody ^ "" |> withContext
  | RBList (Atom (_, "proxy") :: SBList supers :: _ :: body) ->
      let super =
        match supers with
        | Atom (_, n) :: _ -> ": " ^ n ^ "()"
        | [] -> ""
        | x -> fail_node x
      in
      let _, b =
        body
        |> List.fold_left
             (fun (attrs, out) n ->
               match n with
               | Atom (_, attr_name) -> (attr_name :: attrs, out)
               | RBList (Atom (_, fname) :: SBList (_this_ :: args) :: body) ->
                   let attrs_s =
                     attrs
                     |> List.map (fun x -> "@" ^ x)
                     |> List.fold_left (Printf.sprintf "%s\n%s") ""
                   in
                   let fargs =
                     args
                     |> List.map (function
                          | Atom (m, x) -> x ^ ":" ^ m.symbol
                          | n -> fail_node [ n ])
                     |> function
                     | [] -> ""
                     | xs -> xs |> List.reduce (Printf.sprintf "%s, %s")
                   in
                   let fbody =
                     RBList
                       (Atom (unknown_location, "let*") :: SBList [] :: body)
                     |> compile
                   in
                   ( [],
                     out
                     ^ Printf.sprintf "%s\n%sfun %s (%s) { %s }" attrs_s
                         (if super = "" then "" else " override ")
                         fname fargs fbody )
               | _ -> fail_node [ n ])
             ([], "")
      in
      Printf.sprintf "object %s { %s }" super b |> withContext
  | RBList (Atom (_, "defmacro") :: Atom (_, name) :: _) as macro ->
      ({ context with macros = StringMap.add name macro context.macros }, "")
  | RBList [ Atom (_, "spread"); x ] ->
      Printf.sprintf "*%s" (compile x) |> withContext
  | RBList [ Atom (_, "class"); Atom (_, name) ] ->
      Printf.sprintf "%s::class.java" name |> withContext
  (* Functions or Macro calls *)
  | RBList [ (Atom (_, fname) as key); source ]
    when String.starts_with ~prefix:":" fname ->
      Printf.sprintf "getm(%s, %s)" (compile source) (compile key)
      |> withContext
  | RBList [ Atom (_, "."); target; Atom (_, field) ]
    when String.starts_with ~prefix:"-" field ->
      Printf.sprintf "%s.%s" (compile target)
        (String.sub field 1 (String.length field - 1))
      |> withContext
  | RBList (Atom (_, ".") :: target :: mname :: args) ->
      let sargs =
        match args with
        | [] -> ""
        | args ->
            args |> List.map compile
            |> List.reduce (Printf.sprintf "%s, %s")
            |> Printf.sprintf "%s"
      in
      Printf.sprintf "%s.%s(%s)" (compile target) (compile mname) sargs
      |> withContext
  | RBList (Atom (_, fname) :: args) ->
      (if String.ends_with ~suffix:"." fname then
         let cnst_name = String.sub fname 0 (String.length fname - 1) in
         let fargs =
           if List.length args = 0 then ""
           else
             args |> List.map compile |> List.reduce (Printf.sprintf "%s, %s")
         in
         fargs |> Printf.sprintf "%s(%s)" cnst_name
       else
         let sargs =
           if List.length args = 0 then ""
           else
             args |> List.map compile |> List.reduce (Printf.sprintf "%s, %s")
         in
         String.map (function '/' -> '.' | x -> x) fname ^ "(" ^ sargs ^ ")")
      |> withContext
  | n -> fail_node [ n ]

let main (filename : string) code =
  let prelude_macros =
    {|(defmacro do [& body] (concat (list 'let (vector)) body))
      (defmacro FIXME [& args]
        (list 'throw
          (list 'Exception.
            (concat
              (list
                'str
                (str "FIXME " __FILENAME__ ":" __LINE__ ":" (- __POSITION__ 1) " - "))
              args))))
    |}
  in
  String.concat "\n" [ prelude_macros; code ]
  |> Core.parse_and_simplify filename
  |> (fun (ctx, exp) -> compile_ ctx exp)
  |> snd |> String.trim
