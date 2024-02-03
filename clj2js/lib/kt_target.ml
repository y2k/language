module A = Angstrom
open Core

let rec compile_ (context : context) (node : cljexp) : context * string =
  (* let compileOut node = compile_ context node in *)
  let compile node = compile_ context node |> snd in
  let withContext node = (context, node) in
  match expand_core_macro node with
  | Atom (_, x) when String.starts_with ~prefix:":" x ->
      "\"" ^ String.sub x 1 (String.length x - 1) ^ "\"" |> withContext
  | Atom (_, x) -> x |> withContext
  | SBList xs ->
      xs |> List.map compile
      |> List.reduce_opt (Printf.sprintf "%s, %s")
      |> Option.value ~default:""
      |> Printf.sprintf "listOf(%s)"
      |> withContext
  (* ========================== *)
  | RBList (Atom (_, "+") :: xs) ->
      xs |> List.map compile
      |> List.reduce (Printf.sprintf "%s + %s")
      |> Printf.sprintf "(%s)" |> withContext
  | RBList (Atom (l, "defn") :: Atom (_, fname) :: SBList args :: body) ->
      let fn = RBList (Atom (l, "fn") :: SBList args :: body) in
      Printf.sprintf "val %s = %s" fname (compile fn) |> withContext
  | RBList (Atom (_, "fn") :: SBList args :: body) ->
      let sargs =
        match args with
        | [] -> ""
        | _ ->
            args
            |> List.map (function Atom (_, x) -> x | x -> fail_node [ x ])
            |> List.reduce (Printf.sprintf "%s, %s")
            |> Printf.sprintf "%s -> "
      in
      let sbody =
        body |> List.map compile |> List.rev
        |> List.mapi (fun i x -> if i = 0 then x else x)
        |> List.rev
        |> List.reduce (Printf.sprintf "%s; %s")
      in
      Printf.sprintf "{ %s %s }" sargs sbody |> withContext
  | RBList (Atom (_, "let") :: SBList vals :: body) ->
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
        |> List.reduce (Printf.sprintf "%s; %s")
      in
      "" ^ svals ^ sbody ^ "" |> withContext
  | RBList (Atom (_, "proxy") :: _ :: _ :: body) ->
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
                     args |> List.map compile |> function
                     | [] -> ""
                     | xs -> xs |> List.reduce (Printf.sprintf "%s, %s")
                   in
                   let fbody =
                     RBList (Atom (unknown_location, "let") :: SBList [] :: body)
                     |> compile
                   in
                   ( [],
                     Printf.sprintf "%s\nfun %s (%s) { %s }" attrs_s fname fargs
                       fbody )
               | _ -> fail_node [ n ])
             ([], "")
      in
      Printf.sprintf "object { %s }" b |> withContext
  | RBList (Atom (_, "defmacro") :: Atom (_, name) :: _) as macro ->
      ({ context with macros = StringMap.add name macro context.macros }, "")
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
         fargs |> Printf.sprintf "%s(%s)" cnst_name
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
  | n -> fail_node [ n ]

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
