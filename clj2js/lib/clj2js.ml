module A = Angstrom
open Core

let rec compile_ (context : context) (node : cljexp) : context * string =
  let compileOut node = compile_ context node in
  let compile node = compile_ context node |> snd in
  let withContext node = (context, node) in
  match expand_core_macro node with
  (* "Macro function" *)
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
  | RBList [ Atom (l, "first"); body ] ->
      RBList
        [
          Atom (l, "get");
          RBList [ Atom (unknown_location, "Array/from"); body ];
          Atom (unknown_location, "0");
        ]
      |> compileOut
  | RBList [ Atom (l, "second"); body ] ->
      RBList
        [
          Atom (l, "get");
          RBList [ Atom (unknown_location, "Array/from"); body ];
          Atom (unknown_location, "1");
        ]
      |> compileOut
  (* Core forms *)
  | Atom (_, x) when String.starts_with ~prefix:":" x ->
      "\"" ^ String.sub x 1 (String.length x - 1) ^ "\"" |> withContext
  | Atom (_, x) -> x |> withContext
  | SBList xs ->
      xs |> List.map compile
      |> List.reduce_opt (Printf.sprintf "%s, %s")
      |> Option.value ~default:"" |> Printf.sprintf "[%s]" |> withContext
  (*  *)
  | RBList [ Atom (_, "get"); target; index ] ->
      Printf.sprintf "%s[%s]" (compile target) (compile index) |> withContext
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
  | RBList [ Atom (_, "assoc"); map; key; value ] ->
      Printf.sprintf
        "(function(){const temp={...%s};temp[%s]=%s;return temp})()"
        (compile map) (compile key) (compile value)
      |> withContext
  | RBList [ Atom (_, "__unsafe_insert_js"); Atom (_, body) ]
    when String.starts_with ~prefix:"\"" body
         && String.ends_with ~suffix:"\"" body ->
      String.sub body 1 (String.length body - 2) |> withContext
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
      let rec parse_vals nodes =
        match nodes with
        | Atom (_, val_name) :: val_body :: remain ->
            "const " ^ val_name ^ " = " ^ compile val_body ^ "; "
            ^ parse_vals remain
        | [] -> ""
        | xs -> fail_node xs
      in
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

let main_kt (filename : string) str = Kt_target.main filename str
