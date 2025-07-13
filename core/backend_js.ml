open Common
module Ng = Common.NameGenerator

type context = { filename : string; root_dir : string; namespace : string }

let compile_fn do_compile args body =
  let vararg =
    args |> List.rev |> function
    | SAtom (_, name) :: SAtom (_, "&") :: _ :: _ -> ",..." ^ name
    | SAtom (_, name) :: SAtom (_, "&") :: _ -> "..." ^ name
    | _ -> ""
  in
  let args =
    List.map (function SAtom (_, x) -> x | _ -> failsexp __LOC__ args) args
    |> List.take_while (( <> ) "&")
    |> String.concat ","
  in
  let last_body = SexpUtil.last body |> do_compile in
  let body =
    SexpUtil.butlast body |> List.map do_compile
    |> List.map (Printf.sprintf "%s;\n")
    |> String.concat ""
  in
  Printf.sprintf "((%s%s) => {\n%sreturn %s;\n})" args vararg body last_body

let rec do_compile (ctx : context) = function
  | SAtom (_, "nil") -> "null"
  | SAtom (_, x) when String.starts_with ~prefix:":" x ->
      "\"" ^ unpack_symbol x ^ "\""
  | SAtom (_, x) -> x
  (* TODO: move to macro *)
  | SList (_, [ SAtom (_, "<="); a; b ]) ->
      Printf.sprintf "(%s<=%s)" (do_compile ctx a) (do_compile ctx b)
  | SList (_, [ SAtom (_, "<"); a; b ]) ->
      Printf.sprintf "(%s<%s)" (do_compile ctx a) (do_compile ctx b)
  | SList (_, [ SAtom (_, ">"); a; b ]) ->
      Printf.sprintf "(%s>%s)" (do_compile ctx a) (do_compile ctx b)
  | SList (_, [ SAtom (_, ">="); a; b ]) ->
      Printf.sprintf "(%s>=%s)" (do_compile ctx a) (do_compile ctx b)
  | SList (_, SAtom (_, "+") :: args) ->
      List.map (do_compile ctx) args
      |> String.concat " + " |> Printf.sprintf "(%s)"
  | SList (_, SAtom (_, "-") :: args) ->
      List.map (do_compile ctx) args
      |> String.concat " - " |> Printf.sprintf "(%s)"
  | SList (_, SAtom (_, "*") :: args) ->
      List.map (do_compile ctx) args
      |> String.concat " * " |> Printf.sprintf "(%s)"
  | SList (_, SAtom (_, "/") :: args) ->
      List.map (do_compile ctx) args
      |> String.concat " / " |> Printf.sprintf "(%s)"
  | SList (_, SAtom (_, "%") :: args) ->
      List.map (do_compile ctx) args
      |> String.concat " % " |> Printf.sprintf "(%s)"
  | SList (_, [ SAtom (_, "export-default"); x ]) ->
      Printf.sprintf "export default %s" (do_compile ctx x)
  (* TODO: /end *)
  | SList
      ( _,
        [
          SAtom (_, "def*");
          SAtom (_, "__ns_aliases");
          SList (_, [ SAtom (_, "quote*"); SList (_, items) ]);
        ] ) ->
      items |> List.split_into_pairs
      |> List.map (function
           | SAtom (_, name), SList (_, [ _; SAtom (_, path) ])
             when String.starts_with ~prefix:"." path ->
               Printf.sprintf "import * as %s from '%s.js'" name path
           | SAtom (_, name), SList (_, [ _; SAtom (_, path) ]) ->
               Printf.sprintf "import * as %s from '%s'" name path
           | k, v -> failsexp __LOC__ [ k; v ])
      |> String.concat ";\n"
  (* not *)
  | SList (_, [ SAtom (_, "not"); x ]) ->
      Printf.sprintf "!(%s)" (do_compile ctx x)
  (* hash-map *)
  | SList (_, SAtom (_, "hash-map") :: args) ->
      List.map (do_compile ctx) args
      |> List.split_into_pairs
      |> List.map (fun (k, v) -> Printf.sprintf "%s:%s" k v)
      |> String.concat "," |> Printf.sprintf "{%s}"
  (* = *)
  | SList (_, [ SAtom (_, "="); target; value ]) ->
      Printf.sprintf "(%s===%s)" (do_compile ctx target) (do_compile ctx value)
  (* get *)
  | SList (_, [ SAtom (_, "get"); target; key ]) ->
      Printf.sprintf "%s[%s]" (do_compile ctx target) (do_compile ctx key)
  (* vector *)
  | SList (_, SAtom (_, "vector") :: args) ->
      List.map (do_compile ctx) args
      |> String.concat "," |> Printf.sprintf "[%s]"
  (* assoc! *)
  | SList (_, [ SAtom (_, "assoc!"); name; key; value ]) ->
      Printf.sprintf "%s[%s]=%s" (do_compile ctx name) (do_compile ctx key)
        (do_compile ctx value)
  (* assoc *)
  | SList (_, [ SAtom (_, "assoc"); map; key; value ]) ->
      Printf.sprintf "{ ...%s, [%s]: %s }" (do_compile ctx map)
        (do_compile ctx key) (do_compile ctx value)
  | SList (_, [ SAtom (m, "def*"); name; value ]) ->
      let export = if m.symbol = "private" then "" else "export " in
      Printf.sprintf "%sconst %s=%s" export (do_compile ctx name)
        (do_compile ctx value)
  (* let *)
  | SList (_, [ SAtom (_, "let*"); name; value ]) ->
      Printf.sprintf "const %s=%s" (do_compile ctx name) (do_compile ctx value)
  | SList (_, [ SAtom (_, "let*"); name ]) ->
      Printf.sprintf "let %s" (do_compile ctx name)
  (* if *)
  | SList (_, [ SAtom (_, "if*"); cond; then_; else_ ]) ->
      Printf.sprintf "if (%s) {\n%s\n} else {\n%s\n}" (do_compile ctx cond)
        (do_compile ctx then_) (do_compile ctx else_)
  (* throw *)
  | SList (_, [ SAtom (_, "throw"); e ]) ->
      Printf.sprintf "throw %s" (do_compile ctx e)
  (* set! *)
  | SList (_, [ SAtom (_, "set!"); name; value ]) ->
      Printf.sprintf "%s=%s" (do_compile ctx name) (do_compile ctx value)
  | SList (_, [ SAtom (_, "type"); x ]) ->
      Printf.sprintf "typeof %s" (do_compile ctx x)
  (* Function *)
  | SList (_, [ SAtom (_, "fn*"); SList (_, args); body ]) ->
      compile_fn (do_compile ctx) args body
  | SList (_, SAtom (_, "do*") :: body) ->
      let body = List.map (do_compile ctx) body in
      String.concat ";\n" body
  (* new *)
  | SList (_, SAtom (_, "new") :: SAtom (_, type_) :: args) ->
      let args = List.map (do_compile ctx) args in
      Printf.sprintf "new %s(%s)" type_ (String.concat "," args)
  (* Interop field read *)
  | SList (_, [ SAtom (_, "."); target; SAtom (_, field) ])
    when String.starts_with ~prefix:"-" field ->
      Printf.sprintf "%s.%s" (do_compile ctx target)
        (String.sub field 1 (String.length field - 1))
  (* Interop method call *)
  | SList (_, SAtom (_, ".") :: target :: SAtom (_, mname) :: args) ->
      let args = List.map (do_compile ctx) args in
      Printf.sprintf "%s.%s(%s)" (do_compile ctx target) mname
        (String.concat "," args)
  (* Function call *)
  | SList (_, SAtom (_, fn) :: args) ->
      let args = List.map (do_compile ctx) args in
      let fn = fn |> String.map (function '/' -> '.' | c -> c) in
      Printf.sprintf "%s(%s)" fn (String.concat "," args)
  | SList (_, fn :: args) ->
      let args = List.map (do_compile ctx) args in
      let fn = do_compile ctx fn in
      Printf.sprintf "%s(%s)" fn (String.concat "," args)
  | node -> failsexp ~show_pos:true __LOC__ [ node ]

(* *)

let get_macro node =
  let ctx = Backend_eval.eval1 "" node |> fst in
  Backend_eval.get_all_functions ctx

let compile ~log ~filename code =
  let root_dir = "" in
  let namespace = "" in
  Ng.with_scope (fun () ->
      (* prerr_endline @@ "LOG: code: " ^ code; *)
      code
      |> Frontent_simplify.do_simplify get_macro
           {
             log;
             macro = Prelude.prelude_js_macro;
             filename;
             root_dir;
             compile =
               (fun _ -> SList (meta_empty, [ SAtom (meta_empty, "do") ]));
           }
      |> Stage_convert_if_to_statment.invoke
      |> log_stage log "if_to_statement "
      |> Stage_flat_do.invoke
      |> log_stage log "Stage_flat_do"
      |> do_compile { filename; root_dir; namespace })
