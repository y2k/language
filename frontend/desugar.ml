open Ast

type macro = sexpr -> sexpr option

let builtin_macros = Builtin_macros.builtin_macros
let apply_macros macros sexpr = List.find_map (fun macro -> macro sexpr) macros

let desugar ?(macros = builtin_macros) sexprs =
  let rec desugar_one sexpr =
    match apply_macros macros sexpr with
    | Some sexpr -> desugar_one sexpr
    | None -> (
        match sexpr with
        | SAtom _ as atom -> atom
        | SList (meta, bracket, items) -> SList (meta, bracket, List.map desugar_one items))
  in
  List.map desugar_one sexprs
