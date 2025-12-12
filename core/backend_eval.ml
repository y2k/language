open Common

module Utils = struct
  let failobj loc x =
    Printf.sprintf "%s %s" loc (OUtils.obj_to_string x) |> failwith

  let rec serialize_to_string = function
    | SAtom (_, x) -> x
    | SList (_, xs) -> xs |> List.map serialize_to_string |> String.concat ""
end

type eval_context = {
  ns : (string * obj ref) list;
  scope : (string * obj) list;
  level : int;
  builtin_macro :
    (sexp -> sexp) -> Frontent_simplify.simplify_ctx -> sexp -> sexp option;
}
[@@deriving show]

let get_function (ctx : eval_context) (name : string) =
  let scope = ctx.ns |> List.map (fun (x, y) -> (x, !y)) in
  List.assoc_opt name scope
  |> Option.map (function OLambda (_, f) -> f | _ -> failwith __LOC__)

let get_all_functions (ctx : eval_context) =
  ctx.ns
  |> List.map (fun (x, y) -> (x, !y))
  |> List.filter_map (fun (x, y) ->
      match y with OLambda (_, f) -> Some (x, f) | _ -> None)

let resolve_value ctx name =
  if name = "true" then OBool (meta_empty, true)
  else if name = "false" then OBool (meta_empty, false)
  else
    let scope = ctx.scope @ (ctx.ns |> List.map (fun (x, y) -> (x, !y))) in
    match List.assoc_opt name scope with
    | Some v -> v
    | None -> failwith @@ __LOC__ ^ " - Can't find value: '" ^ name ^ "'"

let rec eval_ (ctx : eval_context) (node : sexp) =
  let ctx = { ctx with level = ctx.level + 1 } in
  (* prerr_endline @@ "[EVAL  IN] "
  ^ String.init ctx.level (Fun.const ' ')
  ^ F.sexp_to_string node; *)
  let ctx, result =
    match node with
    | SAtom (m, x) when int_of_string_opt x <> None ->
        (ctx, OInt (m, int_of_string x))
    | SAtom (m, x) when float_of_string_opt x <> None ->
        (ctx, OFloat (m, float_of_string x))
    | SAtom (m, x) when String.starts_with ~prefix:"\"" x ->
        ( ctx,
          OString
            ( m,
              unpack_string x
              |> Str.global_replace (Str.regexp "\\\\n") "\n"
              |> Str.global_replace (Str.regexp "\\\\t") "\t"
              |> Scanf.unescaped ) )
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
        | ctx, OBool (_, false) -> eval_ ctx else_
        | ctx, ONil _ -> eval_ ctx else_
        | ctx, OBool (_, true) -> eval_ ctx then_
        | _ -> eval_ ctx then_)
    | SList (_, [ SAtom (_, "fn*"); SList (_, args_names); body ]) ->
        let l =
          OLambda
            ( meta_empty,
              fun args ->
                let rec loop args_names args ctx =
                  match (args_names, args) with
                  | [ SAtom (_, "&"); SAtom (_, args_name) ], args ->
                      {
                        ctx with
                        scope =
                          (args_name, OList (meta_empty, args)) :: ctx.scope;
                      }
                  | SAtom (_, name) :: args_names, arg :: args ->
                      loop args_names args
                        { ctx with scope = (name, arg) :: ctx.scope }
                  | [], [] -> ctx
                  | _names, _ ->
                      debug_show_sexp [ node ] |> failwith |> ignore;
                      (* Obj.failobj __LOC__ args |> ignore; *)
                      failsexp __LOC__ [ node ]
                in
                let ctx = loop args_names args ctx in
                let _, result = eval_ ctx body in
                result )
        in
        (ctx, l)
    | SList (_, SAtom (_, fname) :: _)
      when String.ends_with ~suffix:"*" fname && fname <> "*" ->
        failsexp __LOC__ [ node ]
    (* Function call *)
    | SList (_, SAtom (_, fname) :: args) ->
        let f = resolve_value ctx fname in
        let f =
          resolve_value ctx fname |> function
          | OLambda (_, f) -> f
          | _ -> Utils.failobj (__LOC__ ^ " | " ^ fname) f
        in
        let args = List.map (fun x -> eval_ ctx x |> snd) args in
        (ctx, f args)
    | SList (_, f :: args) -> (
        trace __LOC__ show_sexp2 node |> ignore;
        match eval_ ctx f |> snd with
        | OLambda (_, f) ->
            let args = List.map (fun x -> eval_ ctx x |> snd) args in
            (ctx, f args)
        | x -> OUtils.failobj __LOC__ x)
    | x -> failsexp __LOC__ [ x ]
  in
  (* prerr_endline @@ "[EVAL OUT] "
  ^ String.init ctx.level (Fun.const ' ')
  ^ F.debug_obj_to_string result; *)
  (* prerr_endline @@ "[EVAL OUT] "
  ^ String.init ctx.level (Fun.const ' ')
  ^ F.sexp_to_string node ^ " -> " ^ F.obj_to_string result; *)
  ({ ctx with level = ctx.level - 1 }, result)

let reg_val name value ctx = { ctx with scope = (name, value) :: ctx.scope }

let reg_fun name f ctx =
  { ctx with ns = (name, ref (OLambda (meta_empty, fun xs -> f xs))) :: ctx.ns }

let gensym_id = Atomic.make 1

let convert_to_ns path =
  path |> String.split_on_char '/'
  |> List.filter (fun x -> x <> "" && x <> ".")
  |> String.concat "."

let rec compile (ctx : eval_context) origin_filename log get_macro type_
    root_dir filename code =
  code
  |> Frontent_simplify.do_simplify ~builtin_macro:ctx.builtin_macro get_macro
       { log; macro = Prelude.prelude_eval_macro; filename }
  |> Stage_resolve_ns.do_resolve (ctx.ns |> List.map fst) filename root_dir
  |> log_stage log (type_ ^ " Stage_resolve_ns")
  |> Stage_load_require.do_invoke (fun path ->
      let path2 =
        Filename.concat (Filename.dirname origin_filename) (path ^ ".clj")
        |> FileReader.realpath
      in
      let code = FileReader.read path2 in
      compile ctx origin_filename log get_macro "  [REQ]" "" path2 code)
  |> log_stage log (type_ ^ " Stage_load_require")
  |> Stage_flat_do.invoke
  |> log_stage log (type_ ^ " Stage_flat_do")

let empty_eval_context ~builtin_macro =
  { builtin_macro; ns = []; scope = []; level = 0 }

let create_prelude_context ~builtin_macro =
  let prelude_sexp =
    compile
      (empty_eval_context ~builtin_macro)
      "" false (Fun.const []) "[PRELUDE]" "" "prelude.clj" Prelude.prelude_eval
  in
  empty_eval_context ~builtin_macro
  |> Backend_eval_functions.attach reg_val reg_fun
  |> Fun.flip eval_ prelude_sexp
  |> fst

let invoke ~builtin_macro (log : bool) (filename : string) code =
  let get_macro node =
    eval_ (empty_eval_context ~builtin_macro) node |> fst |> get_all_functions
  in
  NameGenerator.with_scope (fun () ->
      let ctx = create_prelude_context ~builtin_macro in
      code
      |> compile ctx filename log get_macro "[EVAL]" (get_dir filename) filename
      |> eval_ ctx |> snd |> OUtils.obj_to_sexp |> Utils.serialize_to_string
      |> unpack_string)
