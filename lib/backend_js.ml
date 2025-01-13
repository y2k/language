module A = Angstrom
open Common

let unpack_string x = String.sub x 1 (String.length x - 2)
let unpack_symbol x = String.sub x 1 (String.length x - 1)
let unwrap_do = function SList (_, SAtom (_, "do*") :: xs) -> xs | x -> [ x ]

let rec compile_ (context : context) (node : sexp) : context * string =
  (* log_sexp "js: " node |> ignore; *)
  let compile node = compile_ context node |> snd in
  let with_context node = (context, node) in
  match node with
  (* Atoms *)
  | SAtom (_, "nil") -> with_context "null"
  | SAtom (_, x) when String.starts_with ~prefix:":" x ->
      "\"" ^ String.sub x 1 (String.length x - 1) ^ "\"" |> with_context
  | SAtom (_, x) when String.starts_with ~prefix:"\"" x -> x |> with_context
  | SAtom (_, x) -> String.map (function '/' -> '.' | x -> x) x |> with_context
  (* Version 2.0 *)
  | SList (_, SAtom (_, "do*") :: _body) ->
      (* failwith __LOC__ *)
      let js_body =
        _body |> List.map compile |> List.reduce_opt (Printf.sprintf "%s\n%s") |> Option.value ~default:""
      in
      with_context js_body
  | SList (_, [ SAtom (_, "let*"); SAtom (_, name) ]) ->
      let js_code = Printf.sprintf "let %s;" name in
      with_context js_code
  | SList (_, [ SAtom (_, "let*"); SAtom (_, name); value ]) ->
      let js_code = Printf.sprintf "const %s = %s;" name (compile value) in
      with_context js_code
  | SList (_, [ SAtom (_, "bind-update*"); SAtom (_, name); value ]) ->
      let js_code = Printf.sprintf "%s = %s;" name (compile value) in
      with_context js_code
  | SList (_, [ SAtom (_, "set!"); SAtom (_, name); value ]) ->
      let js_code = Printf.sprintf "%s = %s;" name (compile value) in
      with_context js_code
  | SList (_, [ SAtom (_, "set!"); name; value ]) ->
      let js_code = Printf.sprintf "%s = %s;" (compile name) (compile value) in
      with_context js_code
  | SList (_, [ SAtom (_, "if*"); (SAtom _ as cond); then_; else_ ]) ->
      Printf.sprintf "if (%s) {\n%s\n} else {\n%s\n}" (compile cond) (compile then_) (compile else_) |> with_context
  | SList (_, [ SAtom (_, "if*"); cond; then_; else_ ]) ->
      Printf.sprintf "if (%s) {\n%s\n} else {\n%s\n}" (compile cond) (compile then_) (compile else_) |> with_context
  | SList (_, [ SAtom (_, "spread"); SAtom (_, value) ]) -> Printf.sprintf "...%s" value |> with_context
  | SList (_, [ SAtom (_, "spread"); value ]) -> Printf.sprintf "...%s" (compile value) |> with_context
  (* Expressions *)
  | SList (m, [ SAtom (l, "quote*"); x ]) ->
      SList
        ( m,
          [
            SAtom (l, "hash-map");
            SAtom (l, ":__y2k_type");
            SAtom (l, ":quote");
            SAtom (l, ":value");
            SAtom (l, "\"" ^ show_sexp2 x ^ "\"");
          ] )
      |> compile |> with_context
  (* Vector *)
  | SList (_, SAtom (_, "vector") :: xs) ->
      xs |> List.map compile
      |> List.reduce_opt (Printf.sprintf "%s, %s")
      |> Option.value ~default:"" |> Printf.sprintf "[%s]" |> with_context
  (* *)
  | SList (_, [ SAtom (_, "ns"); SList (_, [ SAtom (_, "quote*"); SList (_, _ :: depencencies) ]) ]) ->
      depencencies
      |> List.map (function
           | SList (_, SAtom (_, ":require") :: requiries) ->
               requiries
               |> List.map (function
                    | SList (_, [ SAtom (_, package); SAtom (_, ":as"); SAtom (_, alias) ]) ->
                        let target =
                          if String.starts_with ~prefix:"\"" package then
                            String.sub package 1 (String.length package - 2) ^ ".js"
                          else if String.starts_with package ~prefix:"js." then
                            String.sub package 3 (String.length package - 3)
                            |> String.map (function '.' -> '/' | ch -> ch)
                          else Printf.sprintf "./%s.js" package
                        in
                        Printf.sprintf "import * as %s from '%s';" alias target
                    | _ -> failsexp __LOC__ requiries)
               |> List.reduce __LOC__ (Printf.sprintf "%s\n%s")
           | x -> failsexp __LOC__ [ x ])
      |> List.reduce_opt (Printf.sprintf "%s\n%s")
      |> Option.value ~default:"" |> with_context
  | SList (_, SAtom (_, "__raw_template") :: args) ->
      args |> List.map compile
      |> List.mapi (fun i x -> if i mod 2 = 0 then unpack_string x else x)
      |> List.reduce __LOC__ ( ^ ) |> with_context
  | SList (_, [ SAtom (_, "assoc"); map; key; value ]) ->
      Printf.sprintf "{ ...%s, [%s]: %s }" (compile map) (compile key) (compile value) |> with_context
  | SList (_, SAtom (_, "try") :: body) ->
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
         |> List.filter (function SList (_, SAtom (_, "catch") :: _) -> false | _ -> true)
         |> to_string_with_returns
       in
       let e_name, catch_body =
         body
         |> List.find_map (function
              | SList (_, SAtom (_, "catch") :: _err_type :: SAtom (_, e) :: body) ->
                  Some (e, to_string_with_returns body)
              | _ -> None)
         |> Option.get
       in
       Printf.sprintf "(function() { try { %s } catch (%s) { %s } })()" try_body e_name catch_body)
      |> with_context
  (* Functions *)
  | SList (_, [ SAtom (l, "def*"); SAtom (mn, fname); SList (m2, SAtom (_, "fn*") :: SList (m3, args) :: body) ]) ->
      let modifier = match mn.symbol with ":private" -> "" | _ -> "export " in
      let fn = SList (m2, SAtom (l, "fn*") :: SList (m3, args) :: body) in
      Printf.sprintf "%sconst %s = %s;" modifier fname (compile fn) |> with_context
  (* Constants *)
  | SList (_, [ SAtom (dm, "def*"); SAtom (sm, name); body ]) ->
      (match (dm.symbol, sm.symbol) with
      | _, ":private" -> Printf.sprintf "const %s = %s;" name (compile body)
      | "export", _ -> Printf.sprintf "export const %s = %s;" name (compile body)
      | _ -> Printf.sprintf "export const %s = %s;" name (compile body))
      |> with_context
  (* Object literal *)
  | SList (_, SAtom (_, "hash-map") :: xs) ->
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
  | SList (_, SAtom (_, "while") :: condition :: body) ->
      Printf.sprintf "while (%s) {%s}" (compile condition)
        (body |> List.map compile |> List.reduce __LOC__ (Printf.sprintf "%s;%s"))
      |> with_context
  (* Lambda *)
  | SList (_, SAtom (_, "fn*") :: SList (_, args) :: body) ->
      let rec loop_args = function
        | SAtom (_, "&") :: SAtom (_, x) :: _ -> Printf.sprintf "...%s" x
        | SAtom (_, x) :: [] -> x
        | SAtom (_, x) :: xs -> Printf.sprintf "%s, %s" x (loop_args xs)
        | [] -> ""
        | n -> failsexp __LOC__ n
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
  | SList (_, [ SAtom (_, "."); target; SAtom (_, field) ]) when String.starts_with ~prefix:":-" field ->
      Printf.sprintf "%s.%s" (compile target) (String.sub field 2 (String.length field - 2)) |> with_context
  (* Interop method *)
  | SList (_, SAtom (_, ".") :: target :: SAtom (_, mname) :: args) ->
      let sargs =
        match args with [] -> "" | args -> args |> List.map compile |> List.reduce __LOC__ (Printf.sprintf "%s, %s")
      in
      Printf.sprintf "%s.%s(%s)" (compile target) (unpack_symbol mname) sargs |> with_context
  (* Constructor *)
  | SList (_, SAtom (_, "new") :: SAtom (_, cnst_name) :: args) ->
      (if List.length args = 0 then "" else args |> List.map compile |> List.reduce __LOC__ (Printf.sprintf "%s, %s"))
      |> Printf.sprintf "new %s(%s)" (unpack_string cnst_name)
      |> with_context
  (* Function call *)
  | SList (_, head :: args) ->
      (let sargs =
         if List.length args = 0 then "" else args |> List.map compile |> List.reduce __LOC__ (Printf.sprintf "%s, %s")
       in
       let fname =
         match head with
         | SList (_, SAtom (_, "fn*") :: _) -> "(" ^ compile head ^ ")"
         | SAtom (_, fname) -> String.map (function '/' -> '.' | x -> x) fname
         | _ -> compile head
       in
       fname ^ "(" ^ sargs ^ ")")
      |> with_context
  | x -> failsexp __LOC__ [ x ]

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
  |> try_log "Parse_and_simplify             ->" log
  |> Stage_normalize_bracket.invoke
  |> try_slog "Stage_normalize_bracket (SEXP) ->" log
  |> Stage_simplify_let.invoke
  |> try_slog "Stage_simplify_let             ->" log
  |> Stage_linter.invoke ctx prelude_sexp
  (* *)
  |> Stage_convert_if_to_statment.invoke
  |> try_slog "Stage_normalize_if             ->" log
  |> compile_ ctx |> snd |> String.trim
