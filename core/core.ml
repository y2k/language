open Lib__.Common

module Prelude = struct
  let prelude_java =
    {|
(defn list [& xs] xs)

(defn macro_= [x y]
  (list 'java.util.Objects.equals x y))

(defn macro_hash-map [& xs]
  (concat
    (list 'java.util.Map.of)
    xs))

(defn macro_count [xs]
  (let* vxs (gensym))
  (list
    'do
    (list 'let vxs xs)
    (list
      'if
      (list 'instance? 'java.util.Map vxs)
      (list '. (list 'cast 'java.util.Map vxs) 'size)
      (list '. (list 'cast 'java.util.Collection vxs) 'size))))

(defn macro_get [xs i]
  (let* vxs (gensym))
  (let* vi (gensym))
  (list
    'do
    (list 'let vxs xs)
    (list 'let vi i)
    (list
      'if
      (list 'instance? 'java.util.Map vxs)
      (list '. (list 'cast 'java.util.Map vxs) 'get vi)
      (list '. (list 'cast 'java.util.List vxs) 'get (list 'cast 'int vi)))))

(defn macro_str [& xs]
  (concat
   (list
    'String.format
    (reduce (fn* [acc x] (str acc "%s")) "" xs))
   xs))

(defn macro_vector [& xs]
  (concat
   (list 'java.util.Arrays.asList)
   xs))
    |}

  let prelude_eval = {|
    (def* vector
      (fn* [& xs] xs))
    |}
end

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

module Eval : sig
  type eval_context

  val empty_eval_context : eval_context
  val show_eval_context : eval_context -> string
  val eval : sexp -> eval_context * obj
  val get_function : eval_context -> string -> (obj list -> obj) option
end = struct
  module OUtils = Lib__.Backend_interpreter.Functions

  type eval_context = {
    ns : (string * obj ref) list;
    scope : (string * obj) list;
  }
  [@@deriving show]

  let get_function (ctx : eval_context) (name : string) =
    let scope = ctx.ns |> List.map (fun (x, y) -> (x, !y)) in
    List.assoc_opt name scope
    |> Option.map (function OLambda (_, f) -> f | _ -> failwith __LOC__)

  let empty_eval_context = { ns = []; scope = [] }
  let rec_level = ref 0

  let resolve_value ctx name =
    if name = "true" then OBool (meta_empty, true)
    else if name = "false" then OBool (meta_empty, false)
    else
      let scope = ctx.scope @ (ctx.ns |> List.map (fun (x, y) -> (x, !y))) in
      match List.assoc_opt name scope with
      | Some v -> v
      | None -> failwith @@ __LOC__ ^ " - Can't find value: '" ^ name ^ "'"

  let rec eval_ (ctx : eval_context) node =
    (* if !rec_level > 10 then failwith __LOC__;
    prerr_endline @@ "LOG[" ^ string_of_int !rec_level ^ "] " ^ debug_show_sexp [ node ]; *)
    match node with
    | SAtom (m, x) when int_of_string_opt x <> None ->
        (ctx, OInt (m, int_of_string x))
    | SAtom (m, x) when String.starts_with ~prefix:"\"" x ->
        (ctx, OString (m, unpack_string x))
    | SAtom (m, x) when String.starts_with ~prefix:":" x ->
        (ctx, OString (m, unpack_symbol x))
    | SAtom (m, x) when String.starts_with ~prefix:"'" x ->
        (ctx, OQuote (m, SAtom (m, unpack_symbol x)))
    (* Resolve value *)
    | SAtom (_, name) -> (ctx, resolve_value ctx name)
    | SList (_, SAtom (_, "do*") :: body) ->
        let ctx, result =
          List.fold_left_map (fun ctx x -> eval_ ctx x) ctx body
        in
        (ctx, result |> List.rev |> List.hd)
    | SList (_, [ SAtom (_, "def*"); SAtom (_, fname); value ]) ->
        let r = ref (ONil meta_empty) in
        let ctx = { ctx with ns = (fname, r) :: ctx.ns } in
        let value = eval_ ctx value |> snd in
        r := value;
        (ctx, ONil meta_empty)
    | SList (_, [ SAtom (_, "let*"); SAtom (_, fname); value ]) ->
        let value = eval_ ctx value |> snd in
        let ctx = { ctx with scope = (fname, value) :: ctx.scope } in
        (ctx, ONil meta_empty)
    | SList (_, [ SAtom (_, "quote*"); value ]) ->
        (ctx, OQuote (meta_empty, value))
    (* if then else *)
    | SList (_, [ SAtom (_, "if*"); cond; then_; else_ ]) -> (
        match eval_ ctx cond with
        | ctx, OBool (_, true) -> eval_ ctx then_
        | ctx, OBool (_, false) -> eval_ ctx else_
        | _ -> failsexp __LOC__ [ node ])
    | SList
        ( _,
          [
            SAtom (_, "fn*");
            SList (_, [ SAtom (_, "&"); SAtom (_, args_name) ]);
            body;
          ] ) ->
        let l =
          OLambda
            ( meta_empty,
              fun args ->
                let ctx =
                  {
                    ctx with
                    scope = (args_name, OList (meta_empty, args)) :: ctx.scope;
                  }
                in
                let _, result = eval_ ctx body in
                result )
        in
        (ctx, l)
    | SList (_, [ SAtom (_, "fn*"); SList (_, args_names); body ]) ->
        let args_names =
          args_names
          |> List.map (function
               | SAtom (_, x) -> x
               | x -> failsexp __LOC__ [ x ])
        in
        let l =
          OLambda
            ( meta_empty,
              fun args ->
                let ctx =
                  { ctx with scope = List.combine args_names args @ ctx.scope }
                in
                let _, result = eval_ ctx body in
                result )
        in
        (ctx, l)
    | SList (_, SAtom (_, fname) :: args) ->
        let f = resolve_value ctx fname in
        let f =
          resolve_value ctx fname |> function
          | OLambda (_, f) -> f
          | _ -> Utils.failobj (__LOC__ ^ " | " ^ fname) f
        in
        let args = List.map (fun x -> eval_ ctx x |> snd) args in
        (ctx, f args)
    | x -> failsexp __LOC__ [ x ]

  let reg_fun name f ctx =
    {
      ctx with
      ns = (name, ref (OLambda (meta_empty, fun xs -> f xs))) :: ctx.ns;
    }

  let eval node =
    (* prerr_endline @@ "EVAL: " ^ debug_show_sexp [ x ]; *)
    rec_level := 0;
    let ctx =
      empty_eval_context
      |> reg_fun "vector" (fun xs -> OVector (meta_empty, xs))
      |> reg_fun "gensym" (fun _ ->
             OQuote
               ( meta_empty,
                 SAtom (meta_empty, Lib__.Common.NameGenerator.get_new_var ())
               ))
      |> reg_fun "concat" (fun xs ->
             xs
             |> List.map (function OList (_, x) -> x | x -> [ x ])
             |> List.flatten
             |> fun xs -> OList (meta_empty, xs))
      |> reg_fun "str" (fun xs ->
             xs
             |> List.map (function
                  | OString (_, x) -> x
                  | x -> OUtils.obj_to_string x)
             |> String.concat ""
             |> fun xs -> OString (meta_empty, xs))
      |> reg_fun "+" (function
           | [ OInt (_, x); OInt (_, y) ] -> OInt (meta_empty, x + y)
           | _ -> failwith __LOC__)
      |> reg_fun "reduce" (function
           | [ OLambda (_, f); init; OList (_, xs) ] ->
               List.fold_left (fun acc x -> f [ acc; x ]) init xs
           | [ OLambda (_, f); OList (_, xs) ] ->
               let init = List.hd xs in
               let xs = List.tl xs in
               List.fold_left (fun acc x -> f [ acc; x ]) init xs
           | _ -> failwith __LOC__)
      |> reg_fun "map" (function
           | [ OLambda (_, f); OList (_, xs) ] ->
               OList (meta_empty, List.map (fun x -> f [ x ]) xs)
           | _ -> failwith __LOC__)
    in
    node |> eval_ ctx
end

module ResolveImport : sig
  val do_resolve : string -> string -> sexp -> sexp
end = struct
  type resolve_ctx = {
    links : (string * string) list;
    aliases : (string * string) list;
    filename : string;
    root_dir : string;
  }

  let mangle_name (ns : string) (name : string) : string =
    Printf.sprintf "G%i%s%i%s" (String.length ns) ns (String.length name) name

  let path_to_namespace name path =
    let path = unpack_string path in
    prerr_endline @@ "LOG: path=" ^ path;
    let path = Str.global_replace (Str.regexp "\\.\\./") "" path in
    prerr_endline @@ "LOG: path=" ^ path;
    let path = String.map (fun x -> if x = '/' then '.' else x) path in
    prerr_endline @@ "LOG: path=" ^ path;
    let path = mangle_name path name in
    path

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
                   path_to_namespace fun_name x)
            |> Option.value ~default:alias_name
          else fun_name
        in
        (ctx, SList (m, SAtom (m, fun_name) :: args))
    | SList (m, fn :: args) ->
        let _, fn = resolve ctx fn in
        let args = List.map (fun x -> resolve ctx x |> snd) args in
        (ctx, SList (m, fn :: args))
    | x -> failsexp __LOC__ [ x ]

  let do_resolve filename root_dir node =
    let ctx = { links = []; aliases = []; filename; root_dir } in
    resolve ctx node |> snd
end

module Simplify : sig
  type simplify_opt = {
    log : bool;
    prelude : string;
    filename : string;
    root_dir : string;
  }

  val do_simplify : simplify_opt -> sexp -> sexp
end = struct
  type simplify_opt = {
    log : bool;
    prelude : string;
    filename : string;
    root_dir : string;
  }

  type simplify_ctx = {
    otp : simplify_opt;
    log : bool;
    macro : Eval.eval_context;
  }

  let sexp_to_obj = function
    | SAtom (m, _) as x -> OQuote (m, x)
    | SList (m, _) as x -> OQuote (m, x)

  let rec simplify (ctx : simplify_ctx) (sexp : sexp) : sexp =
    if ctx.log && false then
      prerr_endline @@ "SIMPLIFY: " ^ debug_show_sexp [ sexp ];
    match sexp with
    | SAtom _ as x -> x
    | SList (_, []) as x -> x
    | SList (m, SAtom (_, "ns") :: _ :: args) ->
        let args =
          args
          |> List.concat_map (function
               | SList (_, SAtom (_, ":require") :: requires) ->
                   let aliases =
                     requires
                     |> List.concat_map (function
                          | SList (_, [ _; path; _; SAtom (_, alias) ]) ->
                              [ SAtom (meta_empty, alias); path ]
                          | x -> failsexp __LOC__ [ x ])
                   in
                   [
                     SList
                       ( meta_empty,
                         [
                           SAtom (meta_empty, "def");
                           SAtom (meta_empty, "__ns_aliases");
                           SList
                             ( meta_empty,
                               [
                                 SAtom (meta_empty, "quote");
                                 SList
                                   ( meta_empty,
                                     SAtom (meta_empty, "hash-map") :: aliases
                                   );
                               ] );
                         ] );
                   ]
               | SList (_, SAtom (_, ":import") :: imports) ->
                   imports
                   |> List.concat_map (function
                        | SList (_, _ :: SAtom (_, pkg) :: classes) ->
                            classes
                            |> List.map (function
                                 | SAtom (_, class_name) ->
                                     SList
                                       ( meta_empty,
                                         [
                                           SAtom (meta_empty, "def*");
                                           SAtom (meta_empty, class_name);
                                           SList
                                             ( meta_empty,
                                               [
                                                 SAtom (meta_empty, "quote*");
                                                 SAtom
                                                   ( meta_empty,
                                                     pkg ^ "." ^ class_name );
                                               ] );
                                         ] )
                                 | x -> failsexp __LOC__ [ x ])
                        | x -> failsexp __LOC__ [ x ])
               | x -> failsexp __LOC__ [ x ])
        in
        SList (m, SAtom (meta_empty, "do") :: args) |> simplify ctx
    | SList (m, SAtom (mif, "if") :: if_args) ->
        SList (m, SAtom (mif, "if*") :: List.map (simplify ctx) if_args)
    | SList (m, SAtom (mdo, "do") :: body) ->
        SList (m, SAtom (mdo, "do*") :: List.map (simplify ctx) body)
    | SList (m, SAtom (ml, "let") :: name :: value) ->
        let value = List.map (simplify ctx) value in
        SList (m, SAtom (ml, "let*") :: name :: value)
    | SList (m, SAtom (mq, "quote") :: x) -> SList (m, SAtom (mq, "quote*") :: x)
    | SList
        ( m,
          SAtom (md, "defn")
          :: name
          :: SList (_, SAtom (_, "vector") :: args)
          :: body ) ->
        let body =
          match body with
          | [ x ] -> x
          | xs -> SList (meta_empty, SAtom (meta_empty, "do*") :: xs)
        in
        SList
          ( m,
            [
              SAtom (md, "def*");
              name;
              SList
                ( m,
                  [
                    SAtom (md, "fn*");
                    SList (meta_empty, args);
                    simplify ctx body;
                  ] );
            ] )
        |> simplify ctx
    | SList (m, SAtom (mfn, "fn") :: SList (ma, _ :: args) :: body) ->
        SList
          ( m,
            SAtom (mfn, "fn*")
            :: SList (ma, args)
            :: List.map (simplify ctx) body )
    | SList (m, [ SAtom (dm, "def"); k; v ]) ->
        SList (m, [ SAtom (dm, "def*"); k; simplify ctx v ])
    (* Macro call *)
    | SList (_, SAtom (_, name) :: args)
      when Eval.get_function ctx.macro ("macro_" ^ name) <> None ->
        let f =
          match Eval.get_function ctx.macro ("macro_" ^ name) with
          | Some x -> x
          | None ->
              failwith @@ __LOC__ ^ "\nCan't find macro '" ^ name ^ "' in:\n"
              ^ Eval.show_eval_context ctx.macro
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
      when n = "def*" || n = "do*" || n = "fn*" || n = "let*" ->
        x
    | SList (_, SAtom (_, name) :: _) when String.ends_with ~suffix:"*" name ->
        failsexp __LOC__ [ sexp ]
    (* Function call *)
    | SList (m, fn :: args) ->
        let args = List.map (simplify ctx) args in
        let fn = simplify ctx fn in
        SList (m, fn :: args)

  let log_stage (opt : simplify_opt) title node =
    (if opt.log then
       let padding = String.make (max 0 (25 - String.length title)) ' ' in
       prerr_endline @@ "* " ^ title ^ " -> " ^ padding
       ^ debug_show_sexp [ node ]);
    node

  let do_simplify (opt : simplify_opt) (node : sexp) : sexp =
    let macro =
      Parser.parse_text opt.prelude
      |> simplify { macro = Eval.empty_eval_context; log = false; otp = opt }
    in
    node |> log_stage opt "Parse"
    |> simplify { macro = Eval.eval macro |> fst; log = opt.log; otp = opt }
    |> Lib__.Stage_convert_if_to_statment.invoke
    |> log_stage opt "Convert if to statement"
    |> ResolveImport.do_resolve opt.filename opt.root_dir
    |> log_stage opt "Resolve import"
end

module JavaCompiler : sig
  type compile_opt = { filename : string; root_page : string }

  val do_compile : compile_opt -> sexp -> string
end = struct
  type compile_opt = { filename : string; root_page : string }
  type complie_context = unit

  let unmangle_symbol x =
    if String.starts_with ~prefix:"G" x then
      let l1 = String.get x 1 |> String.make 1 |> int_of_string in
      let ns = String.sub x 2 l1 in
      let name = String.sub x (l1 + 3) (String.length x - l1 - 3) in
      (ns, name)
    else ("", x)

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
    | SList (_, [ SAtom (_, "def*"); SAtom (_, name); value ]) ->
        Printf.sprintf "public static Object %s=%s" name (compile ctx value)
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
        | SAtom (_, name) ->
            if String.starts_with ~prefix:"G" name then
              let ns, name = unmangle_symbol name in
              Printf.sprintf "y2k.RT.invoke(%s.%s,%s)" ns name args
            else if String.contains name '.' then
              Printf.sprintf "%s(%s)" name args
            else if String.contains name '/' then
              let name =
                String.map (fun x -> if x = '/' then '.' else x) name
              in
              Printf.sprintf "%s(%s)" name args
            else Printf.sprintf "y2k.RT.invoke(%s,%s)" name args
        | x ->
            let fn = compile ctx x in
            Printf.sprintf "y2k.RT.invoke(%s,%s)" fn args)
    | x -> failsexp __LOC__ [ x ]

  let do_compile (opt : compile_opt) sexp =
    let pkg =
      let n = String.length opt.root_page + 1 in
      String.sub opt.filename n (String.length opt.filename - n)
      |> Str.global_replace (Str.regexp "/[^/]+\\.clj") ""
      |> Str.global_replace (Str.regexp "/") "."
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

let eval code =
  let rec serialize_to_string = function
    | SAtom (_, x) -> x
    | SList (_, xs) -> xs |> List.map serialize_to_string |> String.concat ""
  in
  Parser.parse_text (Prelude.prelude_eval ^ code)
  |> Simplify.do_simplify { log = true; prelude = Prelude.prelude_eval }
  |> Eval.eval |> snd |> Utils.obj_to_sexp |> serialize_to_string

let compile (filename : string) (root_page : string) code =
  let log x =
    prerr_endline x;
    x
  in
  Lib__.Common.NameGenerator.with_scope (fun () ->
      Parser.parse_text code
      (* *)
      |> Simplify.do_simplify { log = true; prelude = Prelude.prelude_java }
      |> JavaCompiler.do_compile { filename; root_page }
      |> log)
