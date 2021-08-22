module LanguageParser

open FParsec

let lbracket: Parser<string, unit> = pstring "("
let rbracket = pstring ")"

let def _ = pstring "def"
let a _ = pipe2
let defn _ = pstring "defn"

let module' =
    lbracket >>. pstring "module" >>. rbracket
