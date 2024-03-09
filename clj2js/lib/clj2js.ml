module A = Angstrom
open Core

let prelude =
  {|
(defn atom [x] (Array/of x))
   (defn reset [a x]
     (.pop a)
     (.push a x))
   (defn deref [a] (get a 0))
|}

let prelude_imports = "import { atom, reset, deref } from './prelude.js';"

let rec compile_ (context : context) (node : cljexp) : context * string =
  let compileOut node = compile_ context node in
  let compile node = compile_ context node |> snd in
  let with_context node = (context, node) in
  match node with
  (* "Macro function" *)
  (* | Atom (loc, "FIXME") ->
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
      |> compileOut *)
  | RBList (Atom (_, "module") :: body) ->
      body |> List.map compile
      |> List.reduce (Printf.sprintf "%s\n%s")
      |> with_context
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
      "\"" ^ String.sub x 1 (String.length x - 1) ^ "\"" |> with_context
  | Atom (_, x) -> x |> with_context
  | SBList xs -> RBList (Atom (unknown_location, "vector") :: xs) |> compileOut
  | RBList (Atom (_, "vector") :: xs) ->
      xs |> List.map compile
      |> List.reduce_opt (Printf.sprintf "%s, %s")
      |> Option.value ~default:"" |> Printf.sprintf "[%s]" |> with_context
  (*  *)
  | RBList [ Atom (_, "get"); target; index ] ->
      Printf.sprintf "%s[%s]" (compile target) (compile index) |> with_context
  | RBList [ Atom (_, "not"); arg ] ->
      Printf.sprintf "!%s" (compile arg) |> with_context
  | RBList [ Atom (_, "type"); arg ] ->
      Printf.sprintf "typeof %s" (compile arg) |> with_context
  | RBList [ Atom (_, "set!"); target; value ] ->
      Printf.sprintf "(%s = %s);" (compile target) (compile value)
      |> with_context
  | RBList (Atom (_, "ns") :: _ :: depencencies) ->
      depencencies
      |> List.map (function
           | RBList (Atom (_, ":require") :: requiries) ->
               requiries
               |> List.map (function
                    | SBList
                        [ Atom (_, package); Atom (_, ":as"); Atom (_, alias) ]
                      ->
                        let target =
                          if String.starts_with package ~prefix:"js." then
                            String.sub package 3 (String.length package - 3)
                            |> String.map (function '.' -> '/' | ch -> ch)
                          else
                            Printf.sprintf "./%s.js"
                              (String.map
                                 (function '.' -> '/' | ch -> ch)
                                 package)
                        in
                        Printf.sprintf "import * as %s from '%s';" alias target
                    | _ -> fail_node requiries)
               |> List.reduce (Printf.sprintf "%s\n%s")
           | x -> fail_node [ x ])
      |> List.reduce (Printf.sprintf "%s\n%s")
      |> with_context
  | RBList (Atom (_, "require") :: requiries) ->
      requiries
      |> List.map (function
           | SBList [ Atom (_, name); Atom (_, ":as"); Atom (_, alias) ] ->
               Printf.sprintf "import * as %s from './%s.js';" alias
                 (String.map (function '.' -> '/' | ch -> ch) name)
           | _ -> fail_node requiries)
      |> List.reduce (Printf.sprintf "%s\n%s")
      |> with_context
  | RBList
      [
        Atom (_, "import");
        SBList [ Atom (_, name); Atom (_, ":as"); Atom (_, alias) ];
      ] ->
      Printf.sprintf "import * as %s from '%s';" alias
        (String.map (function '.' -> '/' | ch -> ch) name)
      |> with_context
  | RBList (Atom (_, "defmacro") :: Atom (_, name) :: _) as macro ->
      ({ context with macros = StringMap.add name macro context.macros }, "")
  | RBList [ Atom (_, "concat"); a; b ] ->
      Printf.sprintf "[...%s, ...%s]" (compile a) (compile b) |> with_context
  | RBList [ Atom (_, "conj"); a; b ] ->
      Printf.sprintf "[...%s, %s]" (compile a) (compile b) |> with_context
  | RBList [ Atom (_, "spread"); a ] ->
      Printf.sprintf "...%s" (compile a) |> with_context
  | RBList [ Atom (_, "merge"); a; b ] ->
      Printf.sprintf "{ ...%s, ...%s }" (compile a) (compile b) |> with_context
  | RBList [ Atom (_, "assoc!"); col; key; value ] ->
      Printf.sprintf "%s[%s]=%s" (compile col) (compile key) (compile value)
      |> with_context
  | RBList [ Atom (_, "assoc"); map; Atom (_, key); value ]
    when String.starts_with ~prefix:":" key ->
      Printf.sprintf "{ ...%s, %s: %s }" (compile map)
        (String.sub key 1 (String.length key - 1))
        (compile value)
      |> with_context
  | RBList [ Atom (_, "assoc"); map; key; value ] ->
      Printf.sprintf
        "(function(){const temp={...%s};temp[%s]=%s;return temp})()"
        (compile map) (compile key) (compile value)
      |> with_context
  | RBList [ Atom (_, "__unsafe_insert_js"); Atom (_, body) ]
    when String.starts_with ~prefix:"\"" body
         && String.ends_with ~suffix:"\"" body ->
      String.sub body 1 (String.length body - 2) |> with_context
  | RBList [ Atom (_, "not="); a; b ] ->
      Printf.sprintf "%s !== %s" (compile a) (compile b) |> with_context
  | RBList [ Atom (_, "throw"); ex ] ->
      Printf.sprintf "(function(){throw %s})()" (compile ex) |> with_context
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
      |> with_context
  | RBList
      [
        Atom (l, "def");
        Atom (mn, fname);
        RBList (Atom (_, "fn*") :: SBList args :: body);
      ] ->
      let modifier = match mn.symbol with ":private" -> "" | _ -> "export " in
      let fn = RBList (Atom (l, "fn*") :: SBList args :: body) in
      Printf.sprintf "%sconst %s = %s;" modifier fname (compile fn)
      |> with_context
  | RBList [ Atom (m, "def"); Atom (_, name); body ] ->
      if m.symbol = "export" then
        Printf.sprintf "export const %s = %s;" name (compile body)
        |> with_context
      else Printf.sprintf "const %s = %s;" name (compile body) |> with_context
  | RBList [ Atom (_, "<"); a; b ] ->
      Printf.sprintf "(%s < %s)" (compile a) (compile b) |> with_context
  | RBList [ Atom (_, ">"); a; b ] ->
      Printf.sprintf "(%s > %s)" (compile a) (compile b) |> with_context
  | RBList [ Atom (_, "<="); a; b ] ->
      Printf.sprintf "(%s <= %s)" (compile a) (compile b) |> with_context
  | RBList [ Atom (_, ">="); a; b ] ->
      Printf.sprintf "(%s >= %s)" (compile a) (compile b) |> with_context
  | RBList (Atom (_, "and") :: xs) ->
      xs |> List.map compile
      |> List.reduce (Printf.sprintf "%s && %s")
      |> Printf.sprintf "(%s)" |> with_context
  | RBList (Atom (_, "or") :: xs) ->
      xs |> List.map compile
      |> List.reduce (Printf.sprintf "%s || %s")
      |> Printf.sprintf "(%s)" |> with_context
  | RBList [ Atom (_, "if"); c; a; b ] ->
      Printf.sprintf "(%s ? %s : %s)" (compile c) (compile a) (compile b)
      |> with_context
  | RBList (Atom (_, "comment") :: _) -> "" |> with_context
  | RBList [ Atom (_, "export-default"); body ] ->
      Printf.sprintf "export default %s" (compile body) |> with_context
  | CBList xs ->
      RBList (Atom (unknown_location, "hash-map") :: xs) |> compileOut
  | RBList (Atom (_, "hash-map") :: xs) ->
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
      to_pairs xs |> Printf.sprintf "{%s}" |> with_context
  | RBList [ Atom (_, "="); a; b ] ->
      compile a ^ " === " ^ compile b |> with_context
  | RBList (Atom (_, "+") :: xs) ->
      xs |> List.map compile
      |> List.reduce (Printf.sprintf "%s + %s")
      |> Printf.sprintf "(%s)" |> with_context
  | RBList (Atom (_, "%") :: xs) ->
      xs |> List.map compile
      |> List.reduce (Printf.sprintf "%s %% %s")
      |> Printf.sprintf "(%s)" |> with_context
  | RBList (Atom (_, "-") :: xs) ->
      xs |> List.map compile
      |> List.reduce (Printf.sprintf "%s - %s")
      |> Printf.sprintf "(%s)" |> with_context
  | RBList (Atom (_, "*") :: xs) ->
      xs |> List.map compile
      |> List.reduce (Printf.sprintf "%s * %s")
      |> Printf.sprintf "(%s)" |> with_context
  | RBList [ Atom (_, "/"); a; b ] ->
      Printf.sprintf "(%s / %s)" (compile a) (compile b) |> with_context
  | RBList (Atom (_, "while") :: condition :: body) ->
      Printf.sprintf "while (%s) {%s}" (compile condition)
        (body |> List.map compile |> List.reduce (Printf.sprintf "%s;%s"))
      |> with_context
  | RBList (Atom (_, "fn*") :: SBList args :: body) ->
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
      Printf.sprintf "(%s) => { %s }" sargs sbody |> with_context
  | RBList (Atom (_, "let*") :: SBList vals :: body) ->
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
      "(function () { " ^ svals ^ sbody ^ " })()" |> with_context
  (* Functions or Macro calls *)
  | RBList [ Atom (_, "."); target; Atom (_, field) ]
    when String.starts_with ~prefix:"-" field ->
      Printf.sprintf "%s.%s" (compile target)
        (String.sub field 1 (String.length field - 1))
      |> with_context
  | RBList (Atom (_, ".") :: target :: mname :: args) ->
      let sargs =
        match args with
        | [] -> ""
        | args ->
            args |> List.map compile |> List.reduce (Printf.sprintf "%s, %s")
      in
      Printf.sprintf "%s.%s(%s)" (compile target) (compile mname) sargs
      |> with_context
  | RBList (Atom (_, fname) :: args) ->
      (if String.ends_with ~suffix:"." fname then
         let cnst_name = String.sub fname 0 (String.length fname - 1) in
         let fargs =
           if List.length args = 0 then ""
           else
             args |> List.map compile |> List.reduce (Printf.sprintf "%s, %s")
         in
         fargs |> Printf.sprintf "new %s(%s)" cnst_name
       else
         let sargs =
           if List.length args = 0 then ""
           else
             args |> List.map compile |> List.reduce (Printf.sprintf "%s, %s")
         in
         String.map (function '/' -> '.' | x -> x) fname ^ "(" ^ sargs ^ ")")
      |> with_context
  | x -> fail_node [ x ]

let main (filename : string) code =
  let prelude_macros =
    {|(defmacro do [& body] (concat (list 'let (vector)) body))
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
  in
  String.concat "\n" [ prelude_macros; code ]
  |> Core.parse_and_simplify 12 filename
  |> fun (ctx, exp) ->
  (* print_endline @@ show_cljexp exp ^ "\n"; *)
  let a, b = compile_ ctx exp in
  (a, String.trim b)

let main_kt (filename : string) str = Kt_target.main filename str
let main_sh (filename : string) str = Bash_target.main filename str
let main_json (filename : string) str = Ast_target.main filename str
