open Lib__.Common
module OUtils = Lib__.Backend_interpreter.Functions

module Utils = struct
  let failobj loc x =
    Printf.sprintf "%s %s" loc (OUtils.obj_to_string x) |> failwith
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
      | _ -> failsexp __LOC__ [ node ])
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
                | names, _ -> failsexp __LOC__ names
              in
              let ctx = loop args_names args ctx in
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

let attach_functions stdin ctx =
  ctx
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
         | x -> Obj.failobj __LOC__ x)
  |> reg_val "nil" (ONil meta_empty)
  |> reg_fun "vector" (fun xs -> OVector (meta_empty, xs))
  |> reg_fun "gensym" (fun _ ->
         OQuote
           ( meta_empty,
             SAtom (meta_empty, Lib__.Common.NameGenerator.get_new_var ()) ))
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

let eval1 (stdin : string) node =
  (* prerr_endline @@ "EVAL: " ^ debug_show_sexp [ x ]; *)
  rec_level := 0;
  let ctx = attach_functions stdin empty_eval_context in
  node |> eval_ ctx

open Core_base

let eval code filename stdin =
  let get_macro node =
    let ctx = eval1 "" node |> fst in
    get_all_functions ctx
  in
  Parser.parse_text (Prelude.prelude_eval ^ code)
  |> Simplify.do_simplify get_macro
       {
         log = true;
         macro = Prelude.prelude_eval_macro;
         filename;
         root_dir = get_dir filename;
       }
  |> eval1 stdin
