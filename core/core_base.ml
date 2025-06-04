open Lib__.Common

module Utils = struct
  module OUtils = Lib__.Backend_interpreter.Functions

  let failobj loc x =
    Printf.sprintf "%s %s" loc (OUtils.obj_to_string x) |> failwith

  let rec obj_to_sexp = function
    (* *)
    | OInt (m, x) -> SAtom (m, string_of_int x)
    | OString (m, x) -> SAtom (m, "\"" ^ x ^ "\"")
    | OList (m, xs) -> SList (m, List.map obj_to_sexp xs)
    | OQuote (_, x) -> x
    | ONil m -> SAtom (m, "nil")
    | OVector (m, xs) ->
        SList (m, SAtom (m, "vector") :: List.map obj_to_sexp xs)
    | x -> failobj __LOC__ x
end

module NamespaceUtils = struct
  let mangle_name (ns : string) (name : string) : string =
    Printf.sprintf "G%i%s%i%s" (String.length ns) ns (String.length name) name

  let unmangle_symbol x =
    if String.starts_with ~prefix:"G" x then
      let nstr =
        Str.string_match (Str.regexp "G[0-9]+") x 0 |> ignore;
        let s = Str.matched_string x in
        String.sub s 1 (String.length s - 1)
      in
      (* prerr_endline @@ "LOG: '" ^ x ^ "' -> '" ^ a ^ "'"; *)
      let l1 = int_of_string nstr in
      (* let l1 = String.get x 1 |> String.make 1 |> int_of_string in *)
      let ns = String.sub x (1 + String.length nstr) l1 in
      (* let ns = "|" ^ ns ^ "|" in *)
      let name =
        (* let n = String.length nstr in *)
        if not (Str.string_match (Str.regexp "[0-9]+") x (l1 + 3)) then
          failwith (x ^ "|" ^ string_of_int l1) |> ignore;
        let l2str = Str.matched_string x in
        (* let l2 = l2str |> int_of_string in *)
        let start = l1 + 3 + String.length l2str in
        (* String.sub x (l1 + 2 + n) (String.length x - l1 - 2 - n) *)
        String.sub x start (String.length x - start)
      in
      (* let name = "|" ^ name ^ "|" in *)
      (ns, name)
    else ("", x)

  let path_to_namespace name path =
    let path = unpack_string path in
    (* prerr_endline @@ "LOG: path=" ^ path; *)
    let path = Str.global_replace (Str.regexp "\\.\\./") "" path in
    (* prerr_endline @@ "LOG: path=" ^ path; *)
    let path = String.map (fun x -> if x = '/' then '.' else x) path in
    (* prerr_endline @@ "LOG: path=" ^ path; *)
    let path = mangle_name path name in
    path
end

module ResolveImport : sig
  val do_resolve : string -> string -> sexp -> sexp
end = struct
  type resolve_ctx = {
    links : (string * string) list;
    aliases : (string * string) list; (* filename : string; *)
  }

  let rec resolve (ctx : resolve_ctx) node =
    match node with
    | SAtom (m, name) -> (
        match ctx.links |> List.assoc_opt name with
        | Some x -> (ctx, SAtom (m, x))
        | None -> (ctx, SAtom (m, name)))
    | SList
        ( _,
          SAtom (_, "def*")
          :: SAtom (_, "__ns_aliases")
          :: SList (_, [ SAtom (_, "quote*"); SList (_, _ :: items) ])
          :: _ ) ->
        let items =
          items
          |> List.map (function
               | SAtom (_, x) -> x
               | x -> failsexp __LOC__ [ x ])
          |> List.split_into_pairs
        in
        let ctx = { ctx with aliases = items } in
        (ctx, SList (meta_empty, [ SAtom (meta_empty, "do*") ]))
    | SList
        ( _,
          [
            SAtom (_, "def*");
            SAtom (_, name);
            SList (_, [ SAtom (_, "quote*"); SAtom (_, value) ]);
          ] ) ->
        let ctx = { ctx with links = (name, value) :: ctx.links } in
        let node = SList (meta_empty, [ SAtom (meta_empty, "do*") ]) in
        (ctx, node)
    | SList (m, [ (SAtom (_, "def*") as def_); name; value ]) ->
        let _, value = resolve ctx value in
        (ctx, SList (m, [ def_; name; value ]))
    | SList (m, [ (SAtom (_, "fn*") as fn_); args; body ]) ->
        let _, body = resolve ctx body in
        (ctx, SList (m, [ fn_; args; body ]))
    | SList (m, (SAtom (_, "do*") as do_) :: body) ->
        let ctx, body =
          List.fold_left_map (fun ctx x -> resolve ctx x) ctx body
        in
        let body =
          body
          |> List.filter_map (function
               | SList (_, [ SAtom (_, "do*") ]) -> None
               | x -> Some x)
        in
        (ctx, SList (m, do_ :: body))
    | SList (m, (SAtom (_, "let*") as let_) :: name :: value) ->
        let value = value |> List.map (fun x -> resolve ctx x |> snd) in
        (ctx, SList (m, let_ :: name :: value))
    | SList (m, (SAtom (_, "if*") as if_) :: args) ->
        let args = args |> List.map (fun x -> resolve ctx x |> snd) in
        (ctx, SList (m, if_ :: args))
    (* Function call *)
    | SList (m, SAtom (_, fun_name) :: args)
      when not (String.ends_with ~suffix:"*" fun_name) ->
        let _, args =
          List.fold_left_map (fun ctx x -> resolve ctx x) ctx args
        in
        let fun_name =
          if String.contains fun_name '/' then
            let alias_name = String.split_on_char '/' fun_name |> List.hd in
            ctx.aliases |> List.assoc_opt alias_name
            |> Option.map (fun x ->
                   let fun_name =
                     String.split_on_char '/' fun_name
                     |> List.tl |> String.concat "/"
                   in
                   NamespaceUtils.path_to_namespace fun_name x)
            |> Option.value ~default:fun_name
          else fun_name
        in
        (ctx, SList (m, SAtom (m, fun_name) :: args))
    | SList (m, fn :: args) ->
        let _, fn = resolve ctx fn in
        let args = List.map (fun x -> resolve ctx x |> snd) args in
        (ctx, SList (m, fn :: args))
    | x -> failsexp __LOC__ [ x ]

  let do_resolve _filename _root_dir node =
    let ctx = { links = []; aliases = [] } in
    resolve ctx node |> snd
end

module Simplify : sig
  type simplify_opt = {
    log : bool;
    macro : string;
    filename : string;
    root_dir : string;
  }

  val do_simplify :
    (sexp -> (string * (obj list -> obj)) list) -> simplify_opt -> sexp -> sexp
end = struct
  type simplify_opt = {
    log : bool;
    macro : string;
    filename : string;
    root_dir : string;
  }

  type simplify_ctx = {
    otp : simplify_opt;
    log : bool;
    get_macro : (string * (obj list -> obj)) list;
  }

  let sexp_to_obj = function
    | SAtom (m, _) as x -> OQuote (m, x)
    | SList (m, _) as x -> OQuote (m, x)

  let log_stage (opt : simplify_opt) title node =
    (if opt.log then
       let padding = String.make (max 0 (30 - String.length title)) ' ' in
       prerr_endline @@ "* " ^ title ^ padding ^ " -> "
       ^ debug_show_sexp [ node ]);
    node

  let rec simplify (ctx : simplify_ctx) (sexp : sexp) : sexp =
    let get_macro name = ctx.get_macro |> List.assoc_opt ("macro_" ^ name) in
    if ctx.log && false then
      prerr_endline @@ "SIMPLIFY: " ^ debug_show_sexp [ sexp ];
    match sexp with
    | SAtom _ as x -> x
    | SList (_, []) as x -> x
    | SList (m, SAtom (_, "ns") :: _ :: args) ->
        args
        |> Macro_ns.invoke m
             { root_dir = ctx.otp.root_dir; filename = ctx.otp.filename }
        |> simplify ctx
    | SList (m, SAtom (mif, "if") :: if_args) ->
        SList (m, SAtom (mif, "if*") :: List.map (simplify ctx) if_args)
    | SList (m, SAtom (mdo, "do") :: body) ->
        SList (m, SAtom (mdo, "do*") :: List.map (simplify ctx) body)
    | SList (_, SAtom (_, "let") :: SList (_, SAtom (_, "vector") :: _) :: _) ->
        Macro_let.invoke (simplify ctx) ctx sexp
    | SList (m, SAtom (ml, "let") :: name :: value) ->
        let value = List.map (simplify ctx) value in
        SList (m, SAtom (ml, "let*") :: name :: value)
    | SList (m, [ SAtom (md, "def-"); name; value ]) ->
        SList (m, [ SAtom ({ md with symbol = "private" }, "def"); name; value ])
        |> simplify ctx
    | SList (_, SAtom (_, "->") :: body) ->
        body
        |> List.reduce __LOC__ (fun acc x ->
               match x with
               | SAtom (l, z) -> SList (l, [ SAtom (l, z); acc ])
               | SList (m, a :: bs) -> SList (m, a :: acc :: bs)
               | xs -> failsexp __LOC__ [ xs ])
        |> simplify ctx
    | SList (_, SAtom (_, "->>") :: body) ->
        body
        |> List.reduce __LOC__ (fun acc x ->
               match x with
               | SAtom (l, z) -> SList (l, [ acc; SAtom (l, z) ])
               | SList (m, a :: bs) -> SList (m, (a :: bs) @ [ acc ])
               | xs -> failsexp __LOC__ [ xs ])
        |> simplify ctx
    | SList (m, SAtom (mq, "quote") :: x) -> SList (m, SAtom (mq, "quote*") :: x)
    | SList (m, SAtom (md, "defn") :: name :: args :: body) ->
        SList
          ( m,
            [
              SAtom (md, "def");
              name;
              SList (m, SAtom (md, "fn") :: args :: body);
            ] )
        |> simplify ctx
    | SList (_, SAtom (_, "fn") :: _) as node ->
        node
        |> log_stage ctx.otp "[Macro fn BEFORE]"
        |> Macro_fn.invoke (simplify ctx)
        |> log_stage ctx.otp "[Macro fn AFTER]"
    | SList (m, [ SAtom (dm, "def"); k; v ]) ->
        SList (m, [ SAtom (dm, "def*"); k; simplify ctx v ])
    (* Macro call *)
    | SList (_, SAtom (_, name) :: args) when get_macro name <> None ->
        let f =
          match get_macro name with Some x -> x | None -> failwith __LOC__
        in
        let args = args |> List.map sexp_to_obj in
        let result = f args in
        let result = Utils.obj_to_sexp result in
        prerr_endline @@ "MACRO RESULT: " ^ debug_show_sexp [ result ];
        let result = simplify ctx result in
        prerr_endline @@ "MACRO RESULT(SIMPLE): " ^ debug_show_sexp [ result ];
        (* prerr_endline @@ "LOG2: " ^ debug_show_sexp [ result ]; *)
        (* let result = Lib__.Stage_convert_if_to_statment.invoke result in *)
        (* prerr_endline @@ "LOG3: " ^ debug_show_sexp [ result ]; *)
        (* compile ctx result *)
        result
    (* Constructor *)
    | SList (m, SAtom (mn, clazz) :: args)
      when String.ends_with ~suffix:"." clazz && clazz <> "." ->
        let clazz = String.sub clazz 0 (String.length clazz - 1) in
        let args = List.map (simplify ctx) args in
        SList (m, SAtom (meta_empty, "new") :: SAtom (mn, clazz) :: args)
    (* Interop method call *)
    | SList (m, SAtom (mn, name) :: instance :: args)
      when String.starts_with ~prefix:"." name && name <> "." ->
        let name = String.sub name 1 (String.length name - 1) in
        SList
          (m, SAtom (meta_empty, ".") :: instance :: SAtom (mn, name) :: args)
        |> simplify ctx
    (* Handle special forms *)
    | SList (_, SAtom (_, n) :: _) as x
      when n = "def*" || n = "do*" || n = "fn*" || n = "let*" || n = "if*" ->
        x
    | SList (_, SAtom (_, name) :: _) when String.ends_with ~suffix:"*" name ->
        failsexp __LOC__ [ sexp ]
    (* *)
    | SList (_, SAtom (_, "gen-class") :: args) -> Macro_gen_class.invoke args
    (* Invoke keyword *)
    | SList (m, [ (SAtom (_, name) as k); arg ])
      when String.starts_with ~prefix:":" name ->
        SList (m, [ SAtom (meta_empty, "get"); arg; k ])
    (* Function call *)
    | SList (m, fn :: args) ->
        let args = List.map (simplify ctx) args in
        let fn = simplify ctx fn in
        SList (m, fn :: args)

  let do_simplify eval_macro (opt : simplify_opt) (node : sexp) : sexp =
    let macro =
      Parser.parse_text opt.macro
      |> simplify { log = false; otp = opt; get_macro = [] }
    in
    let do_simplify_inner type_ macro node =
      node
      |> log_stage opt (type_ ^ "Parse ")
      |> simplify { log = opt.log; otp = opt; get_macro = macro }
      |> log_stage opt (type_ ^ "Simplify ")
      |> Lib__.Stage_convert_if_to_statment.invoke
      |> log_stage opt (type_ ^ "Convert if to statement ")
      |> ResolveImport.do_resolve opt.filename opt.root_dir
      |> log_stage opt (type_ ^ "Resolve import")
    in
    let macro_fn_list = eval_macro (do_simplify_inner "[MACRO]" [] macro) in
    node |> do_simplify_inner "[CODE]" macro_fn_list
end

module JavaCompiler : sig
  type compile_opt = {
    filename : string;
    root_dir : string;
    namespace : string;
  }

  val do_compile : compile_opt -> sexp -> string
end = struct
  type compile_opt = {
    filename : string;
    root_dir : string;
    namespace : string;
  }

  type complie_context = unit

  let convert_namespace_to_class_name (ns : string) =
    let parts = String.split_on_char '.' ns in
    let pkg = parts |> List.rev |> List.tl |> List.rev in
    let cls = parts |> List.rev |> List.hd |> String.capitalize_ascii in
    String.concat "." (pkg @ [ cls ])

  let rec compile (ctx : complie_context) sexp =
    (* prerr_endline @@ "COMPILE: " ^ debug_show_sexp [ sexp ]; *)
    match sexp with
    | SAtom (_, x) when String.starts_with ~prefix:":" x ->
        "\"" ^ unpack_symbol x ^ "\""
    | SAtom (_, x) -> x
    (* Operators *)
    | SList (_, SAtom (_, op) :: args)
      when op = "+" || op = "-" || op = "*" || op = "/" ->
        List.map (compile ctx) args
        |> List.map (Printf.sprintf "((int)%s)")
        |> String.concat op |> Printf.sprintf "(%s)"
    | SList (_, SAtom (_, "do*") :: body) ->
        body |> List.map (compile ctx) |> String.concat ";\n"
    | SList (_, [ SAtom (_, "let*"); SAtom (_, name) ]) ->
        Printf.sprintf "Object %s" name
    | SList (_, [ SAtom (_, "let*"); SAtom (_, name); value ]) ->
        Printf.sprintf "Object %s=%s" name (compile ctx value)
    | SList (_, [ SAtom (_, "set!"); SAtom (_, name); value ]) ->
        Printf.sprintf "%s=%s" name (compile ctx value)
    | SList (_, [ SAtom (m, "def*"); SAtom (_, name); value ]) ->
        let visibility = if m.symbol = "private" then "private" else "public" in
        Printf.sprintf "%s static Object %s=%s" visibility name
          (compile ctx value)
    | SList (_, [ SAtom (_, "fn*"); SList (_, args); body ]) ->
        let args =
          List.map
            (function SAtom (_, x) -> x | x -> failsexp __LOC__ [ x ])
            args
        in
        let body = unwrap_sexp_do body in
        let last_body = body |> List.rev |> List.hd |> compile ctx in
        let body =
          body |> List.rev |> List.tl |> List.rev
          |> List.map (compile ctx)
          |> List.map (fun x -> x ^ ";")
          |> String.concat "\n"
        in
        let sargs = String.concat "," args in
        Printf.sprintf "y2k.RT.fn((%s)->{%s\nreturn %s;\n})" sargs body
          last_body
    | SList (_, [ SAtom (_, "quote*"); SAtom (_, value) ]) -> value
    | SList (_, [ SAtom (_, "__compiler_emit"); SAtom (_, value) ]) ->
        unpack_string value
    | SList (_, [ SAtom (_, "if*"); cond; then_; else_ ]) ->
        let cond = compile ctx cond in
        let then_ = compile ctx then_ in
        let else_ = compile ctx else_ in
        Printf.sprintf "if (%s) {\n%s;\n} else {\n%s;\n}" cond then_ else_
    | SList (_, [ SAtom (_, "cast"); SAtom (_, type_); value ]) ->
        let value = compile ctx value in
        Printf.sprintf "((%s)%s)" type_ value
    (* instanceof *)
    | SList (_, [ SAtom (_, "instance?"); SAtom (_, type_); instance ]) ->
        let instance = compile ctx instance in
        Printf.sprintf "(%s instanceof %s)" instance type_
    (* Constructor *)
    | SList (_, SAtom (_, "new") :: SAtom (_, clazz) :: args) ->
        let args = List.map (compile ctx) args in
        Printf.sprintf "new %s(%s)" clazz (String.concat "," args)
    (* Interop call *)
    | SList (_, SAtom (_, ".") :: instance :: SAtom (_, method_) :: args) ->
        let instance = compile ctx instance in
        let args = List.map (compile ctx) args |> String.concat "," in
        Printf.sprintf "%s.%s(%s)" instance method_ args
    | SList (_, SAtom (_, name) :: _) as x
      when String.ends_with ~suffix:"*" name ->
        failsexp __LOC__ [ x ]
    (* Function call *)
    | SList (_, fn :: args) -> (
        let args = List.map (compile ctx) args in
        let args = String.concat "," args in
        match fn with
        | SAtom (_, name) -> (
            if String.starts_with ~prefix:"G" name then
              (* prerr_endline @@ "LOG1: " ^ name; *)
              let ns, name = NamespaceUtils.unmangle_symbol name in
              (* prerr_endline @@ "LOG2: " ^ ns ^ ", " ^ name; *)
              let cls = convert_namespace_to_class_name ns in
              Printf.sprintf "y2k.RT.invoke(%s.%s,%s)" cls name args
            else if String.contains name '.' then
              Printf.sprintf "%s(%s)" name args
            else if String.contains name '/' then
              let name =
                String.map (fun x -> if x = '/' then '.' else x) name
              in
              Printf.sprintf "%s(%s)" name args
            else
              match args with
              | "" -> Printf.sprintf "y2k.RT.invoke(%s)" name
              | _ -> Printf.sprintf "y2k.RT.invoke(%s,%s)" name args)
        | x -> (
            let fn = compile ctx x in
            match args with
            | "" -> Printf.sprintf "y2k.RT.invoke(%s)" fn
            | _ -> Printf.sprintf "y2k.RT.invoke(%s,%s)" fn args))
    | x -> failsexp __LOC__ [ x ]

  let do_compile (opt : compile_opt) sexp =
    (* let pkg =
      let n = String.length opt.root_dir + 1 in
      String.sub opt.filename n (String.length opt.filename - n)
      |> Str.global_replace (Str.regexp "/[^/]+\\.clj") ""
      |> Str.global_replace (Str.regexp "/") "." *)
    let pkg =
      let n = String.length opt.root_dir in
      String.sub opt.filename n (String.length opt.filename - n)
      |> Str.global_replace (Str.regexp "/[^/]+\\.clj") ""
      |> Str.global_replace (Str.regexp "/") "."
      |> fun x -> if x = "" then opt.namespace else opt.namespace ^ x
    in
    let clazz =
      opt.filename
      |> Str.global_replace (Str.regexp "\\.clj") ""
      |> Str.global_replace (Str.regexp ".+/") ""
      |> String.capitalize_ascii
    in
    let body = compile () sexp in
    Printf.sprintf "package %s;\n\npublic class %s {\n%s;\n}" pkg clazz body
end

let get_dir filename =
  filename |> String.split_on_char '/' |> List.rev |> List.tl |> List.rev
  |> String.concat "/"
