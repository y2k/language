module A = Angstrom
open Frontend

let prelude =
  {|
(defn atom [x] (Array/of x))
   (defn reset [a x]
     (.pop a)
     (.push a x))
   (defn deref [a] (get a 0))
|}

let unpack_string x = String.sub x 1 (String.length x - 2)
let unpack_symbol x = String.sub x 1 (String.length x - 1)

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
  | Atom (_, x) when String.starts_with ~prefix:"'" x ->
      unpack_symbol x |> with_context
  | Atom (_, x) when String.starts_with ~prefix:"\"" x -> x |> with_context
  | Atom (_, x) -> String.map (function '/' -> '.' | x -> x) x |> with_context
  | SBList xs -> RBList (Atom (unknown_location, "vector") :: xs) |> compileOut
  | RBList (Atom (_, "vector") :: xs) ->
      xs |> List.map compile
      |> List.reduce_opt (Printf.sprintf "%s, %s")
      |> Option.value ~default:"" |> Printf.sprintf "[%s]" |> with_context
  (*  *)
  | RBList [ Atom (_, "get"); target; index ] ->
      Printf.sprintf "%s[%s]" (compile target) (compile index) |> with_context
  | RBList [ Atom (_, "not"); RBList [ Atom (_, "="); a; b ] ] ->
      Printf.sprintf "%s !== %s" (compile a) (compile b) |> with_context
  | RBList [ Atom (_, "not"); arg ] ->
      Printf.sprintf "!(%s)" (compile arg) |> with_context
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
                          if String.starts_with ~prefix:"\"" package then
                            String.sub package 1 (String.length package - 2)
                            ^ ".js"
                          else if String.starts_with package ~prefix:"js." then
                            String.sub package 3 (String.length package - 3)
                            |> String.map (function '.' -> '/' | ch -> ch)
                          else Printf.sprintf "./%s.js" package
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
              | RBList (Atom (_, "catch") :: _err_type :: Atom (_, e) :: body)
                ->
                  Some (e, to_string_with_returns body)
              | _ -> None)
         |> Option.get
       in
       Printf.sprintf "(function() { try { %s } catch (%s) { %s } })()" try_body
         e_name catch_body)
      |> with_context
  (* Functions *)
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
  (* Constants *)
  | RBList [ Atom (dm, "def"); Atom (sm, name); body ] ->
      (match (dm.symbol, sm.symbol) with
      | _, ":private" -> Printf.sprintf "const %s = %s;" name (compile body)
      | "export", _ ->
          Printf.sprintf "export const %s = %s;" name (compile body)
      | _ -> Printf.sprintf "export const %s = %s;" name (compile body))
      |> with_context
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
      xs
      |> List.map (function
           | Atom _ as x -> compile x
           | x -> compile x |> Printf.sprintf "(%s)")
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
      let rec loop_args = function
        | Atom (_, "&") :: Atom (_, x) :: _ -> Printf.sprintf "...%s" x
        | Atom (_, x) :: [] -> x
        | Atom (_, x) :: xs -> Printf.sprintf "%s, %s" x (loop_args xs)
        | [] -> ""
        | n -> fail_node n
      in
      let sargs = loop_args args in
      (* match args with
         | [] -> ""
         | _ ->
             args
             |> List.map (function Atom (_, x) -> x | x -> fail_node [ x ])
             |> List.reduce (Printf.sprintf "%s, %s") *)
      (* in *)
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
  (* Interop field *)
  | RBList [ Atom (_, "."); target; Atom (_, field) ]
    when String.starts_with ~prefix:"'-" field ->
      Printf.sprintf "%s.%s" (compile target)
        (String.sub field 2 (String.length field - 2))
      |> with_context
  (* Interop method *)
  | RBList (Atom (_, ".") :: target :: mname :: args) ->
      let sargs =
        match args with
        | [] -> ""
        | args ->
            args |> List.map compile |> List.reduce (Printf.sprintf "%s, %s")
      in
      Printf.sprintf "%s.%s(%s)" (compile target) (compile mname) sargs
      |> with_context
  (* Constructor *)
  | RBList (Atom (_, "new") :: Atom (_, cnst_name) :: args) ->
      (if List.length args = 0 then ""
       else args |> List.map compile |> List.reduce (Printf.sprintf "%s, %s"))
      |> Printf.sprintf "new %s(%s)" (unpack_string cnst_name)
      |> with_context
  (* Function call *)
  | RBList (head :: args) ->
      (let sargs =
         if List.length args = 0 then ""
         else args |> List.map compile |> List.reduce (Printf.sprintf "%s, %s")
       in
       let fname =
         match head with
         | RBList (Atom (_, "fn*") :: _) -> "(" ^ compile head ^ ")"
         | _ -> compile head
       in
       String.map (function '/' -> '.' | x -> x) fname ^ "(" ^ sargs ^ ")")
      |> with_context
  | x -> fail_node [ x ]

let main (filename : string) prelude_macros code =
  let macros_ctx =
    prelude_macros
    |> Frontend.parse_and_simplify StringMap.empty 0 "prelude"
    |> fst
  in
  code |> Frontend.parse_and_simplify macros_ctx.macros 0 filename
  |> fun (ctx, exp) ->
  (ctx, Linter.lint prelude_macros filename exp) |> fun (ctx, exp) ->
  let a, b = compile_ ctx exp in
  (a, String.trim b)
