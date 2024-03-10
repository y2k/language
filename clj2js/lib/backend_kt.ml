module A = Angstrom
open Frontend

let prelude_code =
  {|private fun __prelude_plus(a: Any?, b: Any?) = (a as Int) + (b as Int)
private fun __prelude_minus(a: Any?, b: Any?) = (a as Int) - (b as Int)
private fun __prelude_getm(x: Any?, y: String): Any? = if (x is Map<*, *>) x.get(y) else error("require Map")
private fun <T> __prelude_geta(x: List<T>, y: Int): T = x[y]
|}

let rec compile_ (context : context) (node : cljexp) : context * string =
  let compile node = compile_ context node |> snd in
  let with_context node = (context, node) in
  match node with
  | Atom (_, x) when String.starts_with ~prefix:":" x ->
      "\"" ^ String.sub x 1 (String.length x - 1) ^ "\"" |> with_context
  | Atom (_, x) when String.starts_with ~prefix:"\"" x -> x |> with_context
  | Atom (_, x) ->
      x |> String.map (function '/' -> '.' | x -> x) |> with_context
  | SBList xs ->
      xs |> List.map compile
      |> List.reduce_opt (Printf.sprintf "%s, %s")
      |> Option.value ~default:""
      |> Printf.sprintf "listOf(%s)"
      |> with_context
  (* ========================== *)
  | RBList (Atom (_, "module") :: body) ->
      body |> List.map compile
      |> List.reduce (Printf.sprintf "%s\n%s")
      |> with_context
  | RBList [ Atom (_, "__unsafe_inject_code"); Atom (_, code) ]
    when String.get code 0 = '\"' ->
      String.sub code 1 (String.length code - 2)
      |> Scanf.unescaped |> with_context
  | RBList (Atom (_, "str") :: args) ->
      args |> List.map compile
      |> List.fold_left (fun acc x -> acc ^ "+" ^ x) {|""|}
      |> Printf.sprintf "(%s)" |> with_context
  | RBList [ Atom (_, "="); a; b ] ->
      compile a ^ " == " ^ compile b |> with_context
  | RBList [ Atom (_, "not="); a; b ] ->
      compile a ^ " != " ^ compile b |> with_context
  | RBList [ Atom (_, "get"); target; index ] ->
      Printf.sprintf "__prelude_geta(%s, %s)" (compile target) (compile index)
      |> with_context
  | RBList [ Atom (_, "if"); c; a; b ] ->
      Printf.sprintf "if (%s) { %s } else { %s }" (compile c) (compile a)
        (compile b)
      |> with_context
  | RBList
      [
        Atom (_, "gen-class");
        Atom (_, ":name");
        Atom (_, clsName);
        Atom (_, ":extends");
        Atom (_, superCls);
        Atom (_, ":constructors");
        CBList [ SBList params; SBList _ ];
        Atom (_, ":prefix");
        Atom (_, prefix);
        Atom (_, ":methods");
        SBList methods;
      ] ->
      let prefix = String.sub prefix 1 (String.length prefix - 2) in
      let cnt_params =
        match params with
        | [] -> ""
        | params ->
            params
            |> List.mapi (fun i x ->
                   let type1 = compile x in
                   let type2 =
                     if String.starts_with ~prefix:"\"" type1 then
                       String.sub type1 1 (String.length type1 - 2)
                     else type1
                   in
                   Printf.sprintf "p%i:%s" i type2)
            |> List.reduce (Printf.sprintf "%s, %s")
      in
      let state =
        match params with
        | [] -> ""
        | params ->
            params
            |> List.mapi (fun i _ -> Printf.sprintf "p%i" i)
            |> List.reduce (Printf.sprintf "%s, %s")
            |> Printf.sprintf "val state = listOf<Any>(%s); "
      in
      let ms =
        methods
        |> List.map (function
             | SBList [ Atom (m, mname); SBList args; Atom (_, rtype) ] ->
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
                 let annot =
                   match m.symbol with "Override" -> "override" | x -> "@" ^ x
                 in
                 Printf.sprintf "%s fun %s(%s): %s = %s%s(this, %s)" annot mname
                   args_ rtype prefix mname args__
             | x -> fail_node [ x ])
        |> List.reduce (Printf.sprintf "%s\n%s")
      in
      Printf.sprintf "class %s(%s) : %s() { %s%s }" clsName cnt_params superCls
        state ms
      |> with_context
  | RBList [ Atom (_, "as"); value; cls ] ->
      let target = compile cls in
      let target =
        if String.starts_with ~prefix:"\"" target then
          String.sub target 1 (String.length target - 2)
        else target
      in
      Printf.sprintf "(%s as %s)" (compile value) target |> with_context
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
      Printf.sprintf "package %s;%s\n%s\n" name imports prelude_code
      |> with_context
  | RBList (Atom (_, "and") :: xs) ->
      xs |> List.map compile
      |> List.reduce (Printf.sprintf "%s && %s")
      |> Printf.sprintf "(%s)" |> with_context
  | RBList (Atom (_, "or") :: xs) ->
      xs |> List.map compile
      |> List.reduce (Printf.sprintf "%s || %s")
      |> Printf.sprintf "(%s)" |> with_context
  | RBList (Atom (_, "+") :: args) ->
      args |> List.map compile
      |> List.reduce (Printf.sprintf "%s, %s")
      |> Printf.sprintf "__prelude_plus(%s)"
      |> with_context
  | RBList [ Atom (_, "-"); a; b ] ->
      Printf.sprintf "__prelude_minus(%s, %s)" (compile a) (compile b)
      |> with_context
  | RBList [ Atom (_, ">"); a; b ] ->
      Printf.sprintf "(%s > %s)" (compile a) (compile b) |> with_context
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
      |> with_context
  | RBList [ Atom (_, "def"); k; v ] ->
      let modifier =
        match k with
        | Atom (l, _) when l.symbol = ":private" -> "private "
        | _ -> ""
      in
      Printf.sprintf "%sval %s = %s;" modifier (compile k) (compile v)
      |> with_context
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
      Printf.sprintf "{ %s %s }" sargs sbody |> with_context
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
      "" ^ svals ^ sbody ^ "" |> with_context
  | RBList (Atom (_, "defmacro") :: Atom (_, name) :: _) as macro ->
      ({ context with macros = StringMap.add name macro context.macros }, "")
  | RBList [ Atom (_, "spread"); x ] ->
      Printf.sprintf "*%s" (compile x) |> with_context
  | RBList [ Atom (_, "class"); Atom (_, name) ] ->
      Printf.sprintf "%s::class.java" name |> with_context
  | RBList [ Atom (_, "is"); source; Atom (_, type_) ] ->
      Printf.sprintf "(%s is %s)" (compile source) type_ |> with_context
  (* Functions or Macro calls *)
  | RBList [ (Atom (_, fname) as key); source ]
    when String.starts_with ~prefix:":" fname ->
      Printf.sprintf "__prelude_getm(%s, %s)" (compile source) (compile key)
      |> with_context
  | RBList [ Atom (_, "."); target; Atom (_, field) ]
    when String.starts_with ~prefix:"-" field ->
      Printf.sprintf "%s.%s" (compile target)
        (String.sub field 1 (String.length field - 1))
      |> with_context
  | RBList (Atom (_, "comment") :: _) -> "" |> with_context
  (* Interop method call *)
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
      |> with_context
  (* Constructor *)
  | RBList (Atom (_, fname) :: args) when String.ends_with ~suffix:"." fname ->
      (let cnst_name = String.sub fname 0 (String.length fname - 1) in
       let fargs =
         if List.length args = 0 then ""
         else args |> List.map compile |> List.reduce (Printf.sprintf "%s, %s")
       in
       fargs |> Printf.sprintf "%s(%s)" cnst_name)
      |> with_context
  (* Function call *)
  | RBList (head :: args) ->
      (let sargs =
         if List.length args = 0 then ""
         else args |> List.map compile |> List.reduce (Printf.sprintf "%s, %s")
       in
       String.map (function '/' -> '.' | x -> x) (compile head)
       ^ "(" ^ sargs ^ ")")
      |> with_context
  | n -> fail_node [ n ]

let main (filename : string) code =
  let prelude_macros =
    {|(defmacro gen-class [& body] (list '__inject_raw_sexp (concat (list 'gen-class) body)))
      (defmacro do [& body] (concat (list 'let (vector)) body))
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
  let prefix_lines_count =
    String.fold_left
      (fun acc c -> if c = '\n' then acc + 1 else acc)
      1 prelude_macros
  in
  String.concat "\n" [ prelude_macros; code ]
  |> Frontend.parse_and_simplify prefix_lines_count filename
  |> (fun (ctx, exp) -> compile_ ctx exp)
  |> snd |> String.trim
