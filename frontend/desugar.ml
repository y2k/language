open Ast

type macro = sexpr -> sexpr option

let apply_macros macros sexpr = List.find_map (fun macro -> macro sexpr) macros
let default_macros : macro list ref = ref []

let desugar ?(macros = !default_macros) sexprs =
  let rec desugar_one sexpr =
    match sexpr with SList (_, Paren, SAtom (_, "raw-code") :: _) -> sexpr | _ -> desugar_macros sexpr
  and desugar_macros sexpr =
    match apply_macros macros sexpr with
    | Some sexpr -> desugar_one sexpr
    | None -> (
        match sexpr with
        | SAtom _ as atom -> atom
        | SList (meta, bracket, items) -> SList (meta, bracket, List.map desugar_one items))
  in
  List.map desugar_one sexprs
