open Ast

type macro = sexpr -> sexpr option

let builtin_macros = Builtin_macros.builtin_macros

let apply_macros macros sexpr =
  let rec loop = function
    | [] -> sexpr
    | macro :: rest -> ( match macro sexpr with Some sexpr -> sexpr | None -> loop rest)
  in
  loop macros

let desugar ?(macros = builtin_macros) sexprs =
  let rec desugar_one sexpr =
    match apply_macros macros sexpr with
    | SAtom _ as atom -> atom
    | SList (meta, bracket, items) -> SList (meta, bracket, List.map desugar_one items)
  in
  List.map desugar_one sexprs
