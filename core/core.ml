open Lib__.Common

let parse_text code =
  let module P = Lib__.Frontend_parser in
  let module NB = Lib__.Stage_normalize_bracket in
  P.string_to_cjexp code
  |> ( function [ x ] -> x | xs -> RBList (meta_empty, Atom (meta_empty, "do*") :: xs) )
  |> NB.invoke

let rec serialize_to_string = function
  | SAtom (_, x) -> x
  | SList (_, xs) -> xs |> List.map serialize_to_string |> String.concat ""

module Prelude = struct
  let code =
    {|
    (def* vector
      (fn* [& xs]
        (sexp
          "java.util.Arrays.asList("
          (reduce (fn* [acc x] (sexp acc ", " x)) xs)
          ")")))

    (def* def
      (fn* [name value]
        (sexpstr
          "public static Object " name "=" value ";")))
    |}
end

module Eval = struct
  module OUtils = Lib__.Backend_interpreter.Functions

  type eval_context = { ns : (string * obj) list ref; scope : (string * obj) list }

  let empty_eval_context = { ns = ref []; scope = [] }
  let rec_level = ref 0
  let failobj loc x = Printf.sprintf "%s %s" loc (OUtils.obj_to_string x) |> failwith

  let rec obj_to_sexp = function
    (* *)
    | OInt (m, x) -> SAtom (m, string_of_int x)
    | OString (m, x) -> SAtom (m, "\"" ^ x ^ "\"")
    | OList (m, xs) -> SList (m, List.map obj_to_sexp xs)
    | OQuote (_, x) -> x
    | x -> failobj __LOC__ x

  let rec eval_ (ctx : eval_context) node =
    (* if !rec_level > 10 then failwith __LOC__;
    prerr_endline @@ "LOG[" ^ string_of_int !rec_level ^ "] " ^ debug_show_sexp [ node ]; *)
    match node with
    | SAtom (m, x) when int_of_string_opt x <> None -> (ctx, OInt (m, int_of_string x))
    | SAtom (m, x) when String.starts_with ~prefix:"\"" x -> (ctx, OString (m, unpack_string x))
    | SAtom (m, x) when String.starts_with ~prefix:":" x -> (ctx, OString (m, unpack_symbol x))
    | SAtom (m, x) when String.starts_with ~prefix:"'" x -> (ctx, OQuote (m, SAtom (m, unpack_symbol x)))
    | SAtom (_, name) as x -> (
        let scope = ctx.scope @ !(ctx.ns) in
        match List.assoc_opt name scope with
        (* *)
        | Some v -> (ctx, v)
        | None -> failsexp __LOC__ [ x ])
    (* | SList (_, SAtom (_, "sexpstr") :: args) ->
        let args = List.map (fun x -> eval_ ctx x |> snd) args in
        let args = List.map (function OString (_, x) -> x | x -> OUtils.obj_to_string x) args |> String.concat "" in
        (ctx, OString (meta_empty, args)) *)
    | SList (_, SAtom (_, "sexp") :: args) ->
        (* *)
        let args = List.map (fun x -> eval_ ctx x |> snd) args in
        let args = args |> List.map obj_to_sexp in
        (*
        let args = List.map (function OString (_, x) -> x | x -> OUtils.obj_to_string x) args |> String.concat "" in *)
        (ctx, OQuote (meta_empty, SList (meta_empty, args)))
        (* *)
    | SList (_, SAtom (_, "do*") :: body) ->
        let ctx, result = List.fold_left_map (fun ctx x -> eval_ ctx x) ctx body in
        (ctx, result |> List.rev |> List.hd)
    | SList (_, [ SAtom (_, "def*"); SAtom (_, fname); value ]) ->
        let value = eval_ ctx value |> snd in
        ctx.ns := (fname, value) :: !(ctx.ns);
        (ctx, ONil meta_empty)
    | SList (_, [ SAtom (_, "fn*"); SList (_, [ SAtom (_, "&"); SAtom (_, args_name) ]); body ]) ->
        let l =
          OLambda
            ( meta_empty,
              fun args ->
                let ctx = { ctx with scope = (args_name, OList (meta_empty, args)) :: ctx.scope } in
                let _, result = eval_ ctx body in
                result )
        in
        (ctx, l)
    | SList (_, [ SAtom (_, "fn*"); SList (_, args_names); body ]) ->
        let args_names = args_names |> List.map (function SAtom (_, x) -> x | x -> failsexp __LOC__ [ x ]) in
        let l =
          OLambda
            ( meta_empty,
              fun args ->
                let ctx = { ctx with scope = List.combine args_names args @ ctx.scope } in
                let _, result = eval_ ctx body in
                result )
        in
        (ctx, l)
    (* Call a function *)
    | SList (_, SAtom (_, fname) :: args) as node -> (
        let scope = ctx.scope @ !(ctx.ns) in
        match List.assoc_opt fname scope with
        | Some f ->
            let f = f |> function OLambda (_, f) -> f | _ -> failobj (__LOC__ ^ " | " ^ fname) f in
            let args = List.map (fun x -> eval_ ctx x |> snd) args in
            (ctx, f args)
        | None -> failsexp __LOC__ [ node ])
    | x -> failsexp __LOC__ [ x ]

  let eval x =
    prerr_endline @@ "EVAL: " ^ debug_show_sexp [ x ];
    rec_level := 0;
    let ctx = empty_eval_context in
    ctx.ns :=
      ( "escape",
        OLambda
          ( meta_empty,
            function
            (* *)
            | [ OString (_, x) ] ->
                prerr_endline @@ "LOG1: " ^ x;
                OString (meta_empty, "\"" ^ x ^ "\"")
            | [ x ] ->
                prerr_endline @@ "LOG2: " ^ OUtils.obj_to_string x;
                x
            | _ -> failwith __LOC__ ) )
      :: !(ctx.ns);
    ctx.ns :=
      ( "str",
        OLambda
          ( meta_empty,
            fun xs ->
              xs |> List.map (function OString (_, x) -> x | x -> OUtils.obj_to_string x) |> String.concat ""
              |> fun xs -> OString (meta_empty, xs) ) )
      :: !(ctx.ns);
    ctx.ns :=
      ( "+",
        OLambda (meta_empty, function [ OInt (_, x); OInt (_, y) ] -> OInt (meta_empty, x + y) | _ -> failwith __LOC__)
      )
      :: !(ctx.ns);
    ctx.ns :=
      ( "map",
        OLambda
          ( meta_empty,
            function
            | [ OLambda (_, f); OList (_, xs) ] -> OList (meta_empty, List.map (fun x -> f [ x ]) xs)
            | _ -> failwith __LOC__ ) )
      :: !(ctx.ns);
    ctx.ns :=
      ( "reduce",
        OLambda
          ( meta_empty,
            function
            | [ OLambda (_, f); init; OList (_, xs) ] -> List.fold_left (fun acc x -> f [ acc; x ]) init xs
            | [ OLambda (_, f); OList (_, xs) ] ->
                let init = List.hd xs in
                let xs = List.tl xs in
                List.fold_left (fun acc x -> f [ acc; x ]) init xs
            | _ -> failwith __LOC__ ) )
      :: !(ctx.ns);
    eval_ ctx x |> snd
end

let compile code =
  parse_text (Prelude.code ^ code)
  |> Eval.eval
  (* *)
  |> Eval.obj_to_sexp
  |> serialize_to_string
  (* *)
  |> unpack_string
  |> unpack_string
