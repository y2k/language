include Ast
module Desugar = Desugar
module Gensym = Gensym
module Parser = Parser

let parse_string = Parser.parse_string
let desugar = Desugar.desugar
let parse_and_desugar ?macros input = Gensym.run (fun () -> parse_string input |> Result.map (desugar ?macros))
