module A = Angstrom
open Common

let unpack_string x = String.sub x 1 (String.length x - 2)
let unpack_symbol x = String.sub x 1 (String.length x - 1)
let unwrap_do = function RBList (_, Atom (_, "do*") :: xs) -> xs | x -> [ x ]

let rec compile_ (context : context) (node : cljexp) : context * string =
  (* log_sexp "js: " node |> ignore; *)
  let compile node = compile_ context node |> snd in
  let with_context node = (context, node) in
  match node with
  (* Atoms *)
  | Atom (_, "nil") -> with_context "null"
  | Atom (_, x) when String.starts_with ~prefix:":" x ->
      "\"" ^ String.sub x 1 (String.length x - 1) ^ "\"" |> with_context
  | Atom (_, x) when String.starts_with ~prefix:"\"" x -> x |> with_context
  | Atom (_, x) -> String.map (function '/' -> '.' | x -> x) x |> with_context
  (* Version 2.0 *)
  | RBList (_, Atom (_, "do*") :: _body) ->
      (* failwith __LOC__ *)
      let js_body =
        _body |> List.map compile |> List.reduce_opt (Printf.sprintf "%s\n%s") |> Option.value ~default:""
      in
      with_context js_body
  | RBList (_, [ Atom (_, "let*"); Atom (_, name) ]) ->
      let js_code = Printf.sprintf "let %s;" name in
      with_context js_code
  | RBList (_, [ Atom (_, "let*"); Atom (_, name); value ]) ->
      let js_code = Printf.sprintf "const %s = %s;" name (compile value) in
      with_context js_code
  | RBList (_, [ Atom (_, "bind-update*"); Atom (_, name); value ]) ->
      let js_code = Printf.sprintf "%s = %s;" name (compile value) in
      with_context js_code
  | RBList (_, [ Atom (_, "set!"); Atom (_, name); value ]) ->
      let js_code = Printf.sprintf "%s = %s;" name (compile value) in
      with_context js_code
  | RBList (_, [ Atom (_, "set!"); name; value ]) ->
      let js_code = Printf.sprintf "%s = %s;" (compile name) (compile value) in
      with_context js_code
  | RBList (_, [ Atom (_, "if*"); (Atom _ as cond); then_; else_ ]) ->
      Printf.sprintf "if (%s) {\n%s\n} else {\n%s\n}" (compile cond) (compile then_) (compile else_) |> with_context
  | RBList (_, [ Atom (_, "if*"); cond; then_; else_ ]) ->
      Printf.sprintf "if (%s) {\n%s\n} else {\n%s\n}" (compile cond) (compile then_) (compile else_) |> with_context
  | RBList (_, [ Atom (_, "spread"); Atom (_, value) ]) -> Printf.sprintf "...%s" value |> with_context
  | RBList (_, [ Atom (_, "spread"); value ]) -> Printf.sprintf "...%s" (compile value) |> with_context
  (* Version 2.0 *)
  (* Expressions *)
  (* | RBList (Atom (_, "do*") :: body) ->
      body |> List.map compile
      |> List.reduce (Printf.sprintf "%s\n%s")
      |> with_context *)
  | RBList (m, [ Atom (l, "quote*"); x ]) ->
      RBList
        ( m,
          [
            Atom (l, "hash-map");
            Atom (l, ":__y2k_type");
            Atom (l, ":quote");
            Atom (l, ":value");
            Atom (l, "\"" ^ show_sexp x ^ "\"");
          ] )
      |> compile |> with_context
  (* Vector *)
  | RBList (_, Atom (_, "vector") :: xs) ->
      xs |> List.map compile
      |> List.reduce_opt (Printf.sprintf "%s, %s")
      |> Option.value ~default:"" |> Printf.sprintf "[%s]" |> with_context
  | SBList (_, xs) ->
      xs |> List.map compile
      |> List.reduce_opt (Printf.sprintf "%s, %s")
      |> Option.value ~default:"" |> Printf.sprintf "[%s]" |> with_context
  (* *)
  | RBList (_, [ Atom (_, "ns"); RBList (_, [ Atom (_, "quote*"); RBList (_, _ :: depencencies) ]) ]) ->
      depencencies
      |> List.map (function
           | RBList (_, Atom (_, ":require") :: requiries) ->
               requiries
               |> List.map (function
                    | SBList (_, [ Atom (_, package); Atom (_, ":as"); Atom (_, alias) ]) ->
                        let target =
                          if String.starts_with ~prefix:"\"" package then
                            String.sub package 1 (String.length package - 2) ^ ".js"
                          else if String.starts_with package ~prefix:"js." then
                            String.sub package 3 (String.length package - 3)
                            |> String.map (function '.' -> '/' | ch -> ch)
                          else Printf.sprintf "./%s.js" package
                        in
                        Printf.sprintf "import * as %s from '%s';" alias target
                    | _ -> failnode __LOC__ requiries)
               |> List.reduce __LOC__ (Printf.sprintf "%s\n%s")
           | x -> failnode __LOC__ [ x ])
      |> List.reduce_opt (Printf.sprintf "%s\n%s")
      |> Option.value ~default:"" |> with_context
  | RBList (_, Atom (_, "__raw_template") :: args) ->
      args |> List.map compile
      |> List.mapi (fun i x -> if i mod 2 = 0 then unpack_string x else x)
      |> List.reduce __LOC__ ( ^ ) |> with_context
  | RBList (_, [ Atom (_, "assoc"); map; key; value ]) ->
      Printf.sprintf "{ ...%s, [%s]: %s }" (compile map) (compile key) (compile value) |> with_context
  | RBList (_, Atom (_, "try") :: body) ->
      (let to_string_with_returns nodes =
         let count = List.length nodes in
         nodes
         |> List.mapi (fun i x ->
                let l = compile x in
                Some (if i < count - 1 then l else "return " ^ l))
         |> List.filter_map Fun.id
         |> List.reduce __LOC__ (Printf.sprintf "%s\n%s")
       in
       let try_body =
         body
         |> List.filter (function RBList (_, Atom (_, "catch") :: _) -> false | _ -> true)
         |> to_string_with_returns
       in
       let e_name, catch_body =
         body
         |> List.find_map (function
              | RBList (_, Atom (_, "catch") :: _err_type :: Atom (_, e) :: body) ->
                  Some (e, to_string_with_returns body)
              | _ -> None)
         |> Option.get
       in
       Printf.sprintf "(function() { try { %s } catch (%s) { %s } })()" try_body e_name catch_body)
      |> with_context
  (* Functions *)
  | RBList (_, [ Atom (l, "def*"); Atom (mn, fname); RBList (m2, Atom (_, "fn*") :: RBList (m3, args) :: body) ]) ->
      let modifier = match mn.symbol with ":private" -> "" | _ -> "export " in
      let fn = RBList (m2, Atom (l, "fn*") :: RBList (m3, args) :: body) in
      Printf.sprintf "%sconst %s = %s;" modifier fname (compile fn) |> with_context
  (* Constants *)
  | RBList (_, [ Atom (dm, "def*"); Atom (sm, name); body ]) ->
      (match (dm.symbol, sm.symbol) with
      | _, ":private" -> Printf.sprintf "const %s = %s;" name (compile body)
      | "export", _ -> Printf.sprintf "export const %s = %s;" name (compile body)
      | _ -> Printf.sprintf "export const %s = %s;" name (compile body))
      |> with_context
  (* Object literal *)
  | RBList (_, Atom (_, "hash-map") :: xs) ->
      let rec to_pairs = function
        | k :: v :: xs ->
            let a = compile k in
            let kn = if String.starts_with a ~prefix:":" then String.sub a 1 (String.length a - 1) else a in
            let b = "[" ^ kn ^ "]: " ^ compile v in
            let tail = to_pairs xs in
            if tail == "" then b else b ^ ", " ^ tail
        | [] -> ""
        | _ -> failwith __LOC__
      in
      to_pairs xs |> Printf.sprintf "{%s}" |> with_context
  | CBList (_, xs) ->
      let rec to_pairs = function
        | k :: v :: xs ->
            let a = compile k in
            let kn = if String.starts_with a ~prefix:":" then String.sub a 1 (String.length a - 1) else a in
            let b = "[" ^ kn ^ "]: " ^ compile v in
            let tail = to_pairs xs in
            if tail == "" then b else b ^ ", " ^ tail
        | [] -> ""
        | _ -> failwith __LOC__
      in
      to_pairs xs |> Printf.sprintf "{%s}" |> with_context
  | RBList (_, Atom (_, "while") :: condition :: body) ->
      Printf.sprintf "while (%s) {%s}" (compile condition)
        (body |> List.map compile |> List.reduce __LOC__ (Printf.sprintf "%s;%s"))
      |> with_context
  (* Lambda *)
  | RBList (_, Atom (_, "fn*") :: RBList (_, args) :: body) ->
      let rec loop_args = function
        | Atom (_, "&") :: Atom (_, x) :: _ -> Printf.sprintf "...%s" x
        | Atom (_, x) :: [] -> x
        | Atom (_, x) :: xs -> Printf.sprintf "%s, %s" x (loop_args xs)
        | [] -> ""
        | n -> failnode __LOC__ n
      in
      let sargs = loop_args args in
      let sbody =
        let body = body |> List.concat_map unwrap_do in
        body |> List.map compile |> List.rev
        |> List.mapi (fun i x -> if i = 0 then "return " ^ x else x)
        |> List.rev
        |> List.reduce __LOC__ (Printf.sprintf "%s;\n%s")
      in
      Printf.sprintf "((%s) => {\n%s })" sargs sbody |> with_context
  (* Interop field *)
  | RBList (_, [ Atom (_, "."); target; Atom (_, field) ]) when String.starts_with ~prefix:":-" field ->
      Printf.sprintf "%s.%s" (compile target) (String.sub field 2 (String.length field - 2)) |> with_context
  (* Interop method *)
  | RBList (_, Atom (_, ".") :: target :: Atom (_, mname) :: args) ->
      let sargs =
        match args with [] -> "" | args -> args |> List.map compile |> List.reduce __LOC__ (Printf.sprintf "%s, %s")
      in
      Printf.sprintf "%s.%s(%s)" (compile target) (unpack_symbol mname) sargs |> with_context
  (* Constructor *)
  | RBList (_, Atom (_, "new") :: Atom (_, cnst_name) :: args) ->
      (if List.length args = 0 then "" else args |> List.map compile |> List.reduce __LOC__ (Printf.sprintf "%s, %s"))
      |> Printf.sprintf "new %s(%s)" (unpack_string cnst_name)
      |> with_context
  (* Function call *)
  | RBList (_, head :: args) ->
      (let sargs =
         if List.length args = 0 then "" else args |> List.map compile |> List.reduce __LOC__ (Printf.sprintf "%s, %s")
       in
       let fname =
         match head with
         | RBList (_, Atom (_, "fn*") :: _) -> "(" ^ compile head ^ ")"
         | Atom (_, fname) -> String.map (function '/' -> '.' | x -> x) fname
         | _ -> compile head
       in
       fname ^ "(" ^ sargs ^ ")")
      |> with_context
  | x -> failnode __LOC__ [ x ]

let main (log : bool) (filename : string) prelude_macros code =
  let prelude_ctx, prelude_sexp =
    prelude_macros
    |> Frontend.parse_and_simplify
         {
           empty_context with
           (* scope = Backend_interpreter.Functions.functions |> StringMap.map (fun f -> (OLambda f, ref empty_context)); *)
           interpreter = Backend_interpreter.mk_interpret;
           eval = Backend_interpreter.mk_eval ();
         }
         "prelude"
  in
  let prelude_ctx = Stage_add_def_to_scope.invoke prelude_ctx prelude_sexp |> fst in
  let ctx, node = code |> Frontend.parse_and_simplify { prelude_ctx with log } filename in
  node
  |> try_log "Parse_and_simplify      ->" log
  |> Stage_simplify_let.invoke
  |> try_log "Stage_simplify_let      ->" log
  |> Stage_normalize_bracket.invoke
  |> try_log "Stage_normalize_bracket ->" log
  |> Stage_linter.invoke ctx prelude_sexp
  (* *)
  |> Stage_convert_if_to_statment.invoke
  |> try_log "Stage_normalize_if      ->" log
  |> compile_ ctx |> snd |> String.trim
