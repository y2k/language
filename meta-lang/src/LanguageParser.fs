module LanguageParser

open FParsec

let module': Parser<_, unit> = pstring "module"
let lbracket = pstring "("
let rbracket = pstring ")"

let def _ = pstring "def"
let a _ = pipe2
let defn _ = pstring "defn"

let moduleWithBracket = lbracket >>. module' >>. rbracket
