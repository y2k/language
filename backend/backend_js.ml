open Core__.Common
open Core__
open Stage__
module Ng = Common.NameGenerator

type context = {
  filename : string;
  root_dir : string;
  namespace : string;
  prelude_path : string;
}
[@@deriving show]

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

let escape_string_for_js s =
  (* Only escape actual control characters, not existing escape sequences *)
  let buf = Buffer.create (String.length s) in
  String.iter
    (function
      | '\n' -> Buffer.add_string buf "\\n"
      | '\r' -> Buffer.add_string buf "\\r"
      | '\t' -> Buffer.add_string buf "\\t"
      | c -> Buffer.add_char buf c)
    s;
  Buffer.contents buf

let rec do_compile (ctx : context) = function
  | SAtom (_, "nil") -> "null"
  | SAtom (_, x) when String.starts_with ~prefix:":" x ->
      "\"" ^ unpack_symbol x ^ "\""
  | SAtom (_, x) when not (String.starts_with ~prefix:"\"" x) ->
      x
      |> String.map (fun x -> if x = '/' then '.' else x)
      |> Re.replace (Re.Pcre.re "_QMARK_" |> Re.compile) ~f:(Fun.const "?")
  | SAtom (_, x) ->
      let inner = unpack_string x in
      "\"" ^ escape_string_for_js inner ^ "\""
  (* Inline operators  *)
  | SList (_, SAtom (_, "__inline_op__") :: SAtom (_, op) :: args) ->
      List.map (do_compile ctx) args
      |> String.concat (" " ^ unpack_string op ^ " ")
      |> Printf.sprintf "(%s)"
  (* TODO: move to macro *)
  | SList (_, SAtom (_, "/") :: args) ->
      List.map (do_compile ctx) args
      |> String.concat " / " |> Printf.sprintf "(%s)"
  | SList (_, [ SAtom (_, "export_default"); x ]) ->
      Printf.sprintf "export default %s" (do_compile ctx x)
  (* TODO: /end *)
  | SList (_, SAtom (_, "__compiler_emit") :: args) ->
      List.map (do_compile ctx) args
      |> List.map unpack_string |> String.concat "" |> unpack_string
      |> Scanf.unescaped
  | SList (_, [ SAtom (_, "def*"); SAtom (_, "__namespace"); _ ]) ->
      (* let ns = unpack_symbol ns in
      prerr_endline @@ "LOG:NS: " ^ ns; *)
      ""
  | SList
      ( _,
        [
          SAtom (_, "def*");
          SAtom (_, ns_tag);
          SList (_, _ :: SAtom (_, ns) :: items);
        ] )
    when String.ends_with ~suffix:"__NS__" ns_tag ->
      let ns = unpack_string ns in
      items |> List.split_into_pairs
      |> List.map (function
        | SAtom (_, path), SAtom (_, name)
          when String.starts_with ~prefix:":" path ->
            let name = unpack_symbol name in
            let path =
              unpack_symbol path |> String.map (function '.' -> '/' | x -> x)
            in
            (* prerr_endline @@ "[LOG][JS]: " ^ ns ^ " | " ^ path ^ " | " ^ name; *)
            let count1 =
              ns
              |> String.fold_left
                   (fun acc x -> if x = '.' then acc + 1 else acc)
                   0
            in
            let count2 =
              path
              |> String.fold_left
                   (fun acc x -> if x = '.' then acc + 1 else acc)
                   0
            in
            let count = count1 - count2 in
            let prefix = List.init count (fun _ -> "../") |> String.concat "" in
            (* let suffix =  *)
            Printf.sprintf "import * as %s from '%s./%s.js'" name prefix path
        | SAtom (_, path), SAtom (_, name)
          when String.starts_with ~prefix:"\"" path ->
            let name = unpack_symbol name in
            let path = unpack_string path in
            Printf.sprintf "import * as %s from '%s'" name path
        | _ -> failwith __LOC__)
      |> String.concat ";\n"
  | SList (_, [ SAtom (_, "def*"); SAtom (_, "__ns_aliases"); _ ]) -> ""
  (* not *)
  | SList (_, [ SAtom (_, "not"); x ]) ->
      Printf.sprintf "!(%s)" (do_compile ctx x)
  (* hash-map *)
  | SList (_, SAtom (_, "hash_map") :: args) ->
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
  | SList (_, [ SAtom (_, "assoc_BANG_"); name; key; value ]) ->
      Printf.sprintf "%s[%s]=%s" (do_compile ctx name) (do_compile ctx key)
        (do_compile ctx value)
  (* assoc *)
  | SList (_, [ SAtom (_, "assoc"); map; key; value ]) ->
      Printf.sprintf "{ ...%s, [%s]: %s }" (do_compile ctx map)
        (do_compile ctx key) (do_compile ctx value)
  | SList (_, [ SAtom (m, "def*"); SAtom (_, name); value ]) ->
      let export = if m.symbol = "private" then "" else "export " in
      Printf.sprintf "%sconst %s=%s" export name (do_compile ctx value)
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
  | SList (_, [ SAtom (_, "set_BANG_"); name; value ]) ->
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
      let fn =
        fn
        |> String.map (function '/' -> '.' | c -> c)
        |> String.map (fun x -> if x = '-' then '_' else x)
      in
      Printf.sprintf "%s(%s)" fn (String.concat "," args)
  | SList (_, fn :: args) ->
      let args = List.map (do_compile ctx) args in
      let fn = do_compile ctx fn in
      Printf.sprintf "%s(%s)" fn (String.concat "," args)
  | node -> failsexp ~show_pos:true __LOC__ [ node ]

let get_macro ~builtin_macro node =
  Backend_eval.eval_ (Backend_eval.create_prelude_context ~builtin_macro) node
  |> fst |> Backend_eval.get_all_functions

let compile ~builtin_macro ~log ~filename ~prelude_path code =
  let root_dir = "" in
  let namespace = "" in
  Ng.with_scope (fun () ->
      (* prerr_endline @@ "LOG: code: " ^ code; *)
      code
      |> Frontend_simplify.do_simplify ~builtin_macro (get_macro ~builtin_macro)
           { log; macro = Prelude.prelude_js_macro; filename }
      |> Stage_convert_if_to_statment.invoke
      |> log_stage log "if_to_statement "
      |> Stage_flat_do.invoke
      |> log_stage log "Stage_flat_do"
      |> Stage_escape_names.invoke
      |> log_stage log "Stage_escape_names"
      |> do_compile { filename; root_dir; namespace; prelude_path }
      |> fun code ->
      if prelude_path = "" then Printf.sprintf "\"use strict\";\n%s" code
      else
        Printf.sprintf "\"use strict\";\nimport * as prelude from '%s';\n%s"
          prelude_path code)
