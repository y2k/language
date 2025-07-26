open Common
module F = Functions

module Utils = struct
  let failobj loc x = Printf.sprintf "%s %s" loc (F.obj_to_string x) |> failwith
end

type eval_context = {
  ns : (string * obj ref) list;
  scope : (string * obj) list;
}
[@@deriving show]

let get_function (ctx : eval_context) (name : string) =
  let scope = ctx.ns |> List.map (fun (x, y) -> (x, !y)) in
  List.assoc_opt name scope
  |> Option.map (function OLambda (_, f) -> f | _ -> failwith __LOC__)

let get_all_functions (ctx : eval_context) =
  let scope = ctx.ns |> List.map (fun (x, y) -> (x, !y)) in
  scope
  |> List.filter_map (fun (x, y) ->
         match y with OLambda (_, f) -> Some (x, f) | _ -> None)

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
  | SAtom (m, x) when float_of_string_opt x <> None ->
      (ctx, OFloat (m, float_of_string x))
  | SAtom (m, x) when String.starts_with ~prefix:"\"" x ->
      ( ctx,
        OString
          ( m,
            unpack_string x
            |> Str.global_replace (Str.regexp "\\\\n") "\n"
            |> Str.global_replace (Str.regexp "\\\\t") "\t" ) )
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
      | ctx, ONil _ -> eval_ ctx else_
      | _, r -> Utils.failobj __LOC__ r)
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
                      scope = (args_name, OList (meta_empty, args)) :: ctx.scope;
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

let reg_val name value ctx = { ctx with scope = (name, value) :: ctx.scope }

let reg_fun name f ctx =
  { ctx with ns = (name, ref (OLambda (meta_empty, fun xs -> f xs))) :: ctx.ns }

let re_find pattern str =
  let pattern =
    pattern
    |> Str.global_replace (Str.regexp "(") "\\("
    |> Str.global_replace (Str.regexp ")") "\\)"
  in
  let re = Str.regexp pattern in
  try
    let _result = Str.search_forward re str 0 in
    (* prerr_endline @@ "LOG: " ^ string_of_int _result; *)
    let groups =
      Seq.unfold
        (fun i ->
          try
            let r = Str.matched_group i str in
            Some (r, i + 1)
          with Invalid_argument _ -> None)
        1
      |> List.of_seq
    in
    Some (Str.matched_string str :: groups)
  with Not_found -> None

let gensym_id = Atomic.make 1

let attach_functions stdin ctx =
  ctx
  |> reg_fun "with-meta" (fun xs ->
         match xs with
         | [ o; OString (_, symbol) ] -> (
             match o with
             | ONil m -> ONil { m with symbol }
             | OVector (m, xs) -> OVector ({ m with symbol }, xs)
             | OList (m, xs) -> OList ({ m with symbol }, xs)
             | OMap (m, xs) -> OMap ({ m with symbol }, xs)
             | OString (m, s) -> OString ({ m with symbol }, s)
             | OInt (m, i) -> OInt ({ m with symbol }, i)
             | OFloat (m, f) -> OFloat ({ m with symbol }, f)
             | OBool (m, b) -> OBool ({ m with symbol }, b)
             | OLambda (m, f) -> OLambda ({ m with symbol }, f)
             | OQuote (m, n) -> OQuote ({ m with symbol }, n))
         | x -> Obj.failobj __LOC__ x)
  |> reg_fun "boolean" (function
       | [ OBool (_, x) ] -> OBool (meta_empty, x)
       | [ ONil _ ] -> OBool (meta_empty, false)
       | [ _ ] -> OBool (meta_empty, true)
       | xs -> Obj.failobj __LOC__ xs)
  |> reg_fun "vec" (function
       | [ OList (_, xs) ] -> OVector (meta_empty, xs)
       | [ (OVector _ as x) ] -> x
       | x -> Obj.failobj __LOC__ x)
  |> reg_fun "gensym" (fun _ ->
         OQuote (meta_empty, SAtom (meta_empty, NameGenerator.get_new_var ())))
  |> reg_fun "drop" (fun xs ->
         match xs with
         | [ OInt (_, n); OList (_, xs) ] ->
             OList (meta_empty, List.filteri (fun i _ -> i >= n) xs)
         | [ OInt (_, n); OVector (_, xs) ] ->
             OVector (meta_empty, List.filteri (fun i _ -> i >= n) xs)
         | x -> Obj.failobj __LOC__ x)
  |> reg_fun "map?" (fun xs ->
         match xs with
         | [ OMap _ ] -> OBool (meta_empty, true)
         | _ -> OBool (meta_empty, false))
  |> reg_fun "=" (fun xs ->
         match xs with
         | [ x; y ] -> OBool (meta_empty, Obj.equal x y)
         | x -> Obj.failobj __LOC__ x)
  |> reg_fun "vector?" (fun xs ->
         match xs with
         | [ OVector _ ] -> OBool (meta_empty, true)
         | _ -> OBool (meta_empty, false))
  |> reg_fun "list" (fun xs -> OList (meta_empty, xs))
  |> reg_fun "string/starts-with?" (fun xs ->
         match xs with
         | [ OString (_, str); OString (_, prefix) ] ->
             OBool (meta_empty, String.starts_with ~prefix str)
         | x -> Obj.failobj __LOC__ x)
  |> reg_fun "filter" (fun xs ->
         match xs with
         | [ OLambda (_, f); OList (_, xs) ] ->
             OList
               ( meta_empty,
                 List.filter
                   (fun x ->
                     match f [ x ] with OBool (_, true) -> true | _ -> false)
                   xs )
         | x -> Obj.failobj __LOC__ x)
  |> reg_fun "hash-map" (fun xs -> OMap (meta_empty, List.split_into_pairs xs))
  |> reg_fun "string/trim" (fun xs ->
         match xs with
         | [ OString (_, x) ] -> OString (meta_empty, String.trim x)
         | x -> Obj.failobj __LOC__ x)
  |> reg_fun "re-find" (fun xs ->
         match xs with
         | [ OString (_, pattern); OString (_, str) ] -> (
             match re_find pattern str with
             | Some xs ->
                 OList
                   ( meta_empty,
                     List.map (fun x -> OString (meta_empty, x)) (List.tl xs) )
             | None -> ONil meta_empty)
         | x -> Obj.failobj __LOC__ x)
  |> reg_fun "count" (fun xs ->
         match xs with
         | [ OList (_, xs) ] -> OInt (meta_empty, List.length xs)
         | [ OVector (_, xs) ] -> OInt (meta_empty, List.length xs)
         | x -> Obj.failobj __LOC__ x)
  |> reg_fun "string/split" (fun xs ->
         match xs with
         | [ OString (_, x); OString (_, sep) ] ->
             (* FIXME: This is a hack *)
             (* let sep = Str.global_replace (Str.regexp "\\\\n") "\n" sep in *)
             (* prerr_endline @@ "|" ^ sep ^ "|"; *)
             OList
               ( meta_empty,
                 List.map
                   (fun x -> OString (meta_empty, x))
                   (String.split_on_char (String.get sep 0) x) )
         | x -> Obj.failobj __LOC__ x)
  |> reg_val "STDIN" (OString (meta_empty, stdin))
  |> reg_fun "get" (fun xs ->
         match xs with
         | [ OMap (_, m); k ] ->
             List.find_opt (fun (k', _) -> Obj.equal k k') m
             |> Option.map snd
             |> Option.value ~default:(ONil meta_empty)
         | [ OList (_, xs); OInt (_, i) ] -> List.nth xs i
         | [ OVector (_, xs); OInt (_, i) ] -> List.nth xs i
         | x -> Obj.failobj __LOC__ x)
  |> reg_val "nil" (ONil meta_empty)
  |> reg_fun "vector" (fun xs -> OVector (meta_empty, xs))
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
       | [ OLambda (_, f); init; OVector (_, xs) ] ->
           List.fold_left (fun acc x -> f [ acc; x ]) init xs
       | [ OLambda (_, f); init; OMap (_, xs) ] ->
           List.fold_left
             (fun acc (k, v) -> f [ acc; OVector (meta_empty, [ k; v ]) ])
             init xs
       | [ OLambda (_, f); OList (_, xs) ] ->
           let init = List.hd xs in
           let xs = List.tl xs in
           List.fold_left (fun acc x -> f [ acc; x ]) init xs
       | x -> Obj.failobj __LOC__ x)
  |> reg_fun "map" (function
       | [ OLambda (_, f); OList (_, xs) ] ->
           OList (meta_empty, List.map (fun x -> f [ x ]) xs)
       | [ OLambda (_, f); OVector (_, xs) ] ->
           OVector (meta_empty, List.map (fun x -> f [ x ]) xs)
       | x -> Obj.failobj __LOC__ x)

let eval1 ctx (stdin : string) node =
  (* prerr_endline @@ "EVAL: " ^ debug_show_sexp [ x ]; *)
  rec_level := 0;
  let ctx = attach_functions stdin ctx in
  node |> eval_ ctx

let convert_to_ns path =
  path |> String.split_on_char '/'
  |> List.filter (fun x -> x <> "" && x <> ".")
  |> String.concat "."

let eval2 (log : bool) (filename : string) (stdin : string) code =
  let _origin_filename = filename in
  let get_macro node =
    eval1 empty_eval_context "" node |> fst |> get_all_functions
  in
  let rec serialize_to_string = function
    | SAtom (_, x) -> x
    | SList (_, xs) -> xs |> List.map serialize_to_string |> String.concat ""
  in
  let rec eval3 type_ root_dir filename code =
    code
    |> Frontent_simplify.do_simplify get_macro
         {
           log;
           macro = Prelude.prelude_eval_macro;
           filename;
           root_dir;
           compile =
             (fun path ->
               let code = FileReader.read path in
               (* prerr_endline @@ "[LOG] path: " ^ path ^ ", code: " ^ code; *)
               eval3 "  [COMPILE2]" (get_dir filename) filename code);
         }
    |> Stage_resolve_ns.do_resolve filename root_dir
    |> log_stage log (type_ ^ " Stage_resolve_ns")
    |> Stage_load_require.do_invoke (fun path ->
           let path2 =
             Filename.concat (Filename.dirname _origin_filename) (path ^ ".clj")
             |> FileReader.realpath
           in
           let code = FileReader.read path2 in
           eval3 "  [REQ]" "" path2 code)
    |> log_stage log (type_ ^ " Stage_load_require")
    |> Stage_flat_do.invoke
    |> log_stage log (type_ ^ " Stage_flat_do")
  in
  NameGenerator.with_scope (fun () ->
      code
      |> eval3 "[EVAL]" (get_dir filename) filename
      |> eval1
           (fst
              (eval_
                 (attach_functions "" empty_eval_context)
                 (eval3 "[PRELUDE]" "" "prelude.clj" Prelude.prelude_eval)))
           stdin
      |> snd |> OUtils.obj_to_sexp |> serialize_to_string |> unpack_string)
