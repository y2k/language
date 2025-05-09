open Lib__.Common

module Prelude = struct
  let prelude_java =
    {|
    (def* list
      (fn* [& xs] xs))

    (def* macro_str
      (fn* [& xs]
           (concat
            (list
             (quote* String.format)
             (reduce (fn* [acc x] (str acc "%s")) "" xs))
            xs)))

    (def* macro_vector
      (fn* [& xs]
           (concat
            (list (quote* java.util.Arrays.asList))
            xs)))
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
    | x -> failobj __LOC__ x
end

module Eval : sig
  type eval_context

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

  let eval x =
    prerr_endline @@ "EVAL: " ^ debug_show_sexp [ x ];
    rec_level := 0;
    let ctx =
      empty_eval_context
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
    eval_ ctx x
end

module JavaCompiler : sig
  val do_compile : sexp -> string
end = struct
  type complie_context = { macro : Eval.eval_context }

  let sexp_to_obj = function
    | SAtom (m, _) as x -> OQuote (m, x)
    | SList (m, _) as x -> OQuote (m, x)

  let rec compile (ctx : complie_context) sexp =
    prerr_endline @@ "COMPILE: " ^ debug_show_sexp [ sexp ];
    match sexp with
    | SAtom (_, x) -> x
    | SList (_, SAtom (_, "do*") :: body) ->
        body |> List.map (compile ctx) |> String.concat ";\n"
    | SList (_, [ SAtom (_, "def*"); SAtom (_, name); value ]) ->
        Printf.sprintf "public static Object %s=%s" name (compile ctx value)
    | SList (_, [ SAtom (_, "fn*"); SList (_, args); body ]) ->
        let args =
          List.map
            (function SAtom (_, x) -> x | x -> failsexp __LOC__ [ x ])
            args
        in
        let body = compile ctx body in
        let sargs = String.concat "," args in
        Printf.sprintf "y2k.RT.fn((%s)->{%s})" sargs body
    | SList (_, [ SAtom (_, "quote*"); SAtom (_, value) ]) -> value
    | SList (_, [ SAtom (_, "if*"); cond; then_; else_ ]) ->
        let cond = compile ctx cond in
        let then_ = compile ctx then_ in
        let else_ = compile ctx else_ in
        Printf.sprintf "if (%s) {\n%s\n} else {\n%s\n}" cond then_ else_
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
        compile ctx result
    (* Function call *)
    | SList (_, SAtom (_, name) :: args)
      when not (String.ends_with ~suffix:"*" name) ->
        let args = List.map (fun x -> compile ctx x) args in
        if String.contains name '.' then
          Printf.sprintf "%s(%s)" name (String.concat "," args)
        else Printf.sprintf "y2k.RT.invoke(%s,%s)" name (String.concat "," args)
    | x -> failsexp __LOC__ [ x ]

  let do_compile sexp =
    let macro = Parser.parse_text Prelude.prelude_java in
    let ctx = { macro = Eval.eval macro |> fst } in
    compile ctx sexp
end

module Simplify : sig
  val simplify : sexp -> sexp
end = struct
  let simplify sexp = sexp
end

let eval code =
  let rec serialize_to_string = function
    | SAtom (_, x) -> x
    | SList (_, xs) -> xs |> List.map serialize_to_string |> String.concat ""
  in
  Parser.parse_text (Prelude.prelude_eval ^ code)
  |> Simplify.simplify |> Eval.eval |> snd |> Utils.obj_to_sexp
  |> serialize_to_string

let compile code =
  Parser.parse_text code
  (* *)
  |> Simplify.simplify
  |> JavaCompiler.do_compile
