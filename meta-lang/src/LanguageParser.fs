module LanguageParser

type ExtLang =
    | ExtModule of ExtLang list
    | ExtDefn of string

type sexp =
    | Atom of string
    | List of sexp list

open FParsec

let private lbracket: Parser<_, unit> = choice [ pstring "("; pstring "[" ]
let private rbracket = choice [ pstring ")"; pstring "]" ]

let private patom =
    (many1Satisfy (fun ch -> isLetter ch || isDigit ch)
     |>> (fun name -> Atom name))

let psexp =
    let expr, exprImpl = createParserForwardedToRef ()

    exprImpl
    := between
        (spaces .>> lbracket)
        rbracket
        ((many (choice [ patom; expr ] .>> spaces))
         |>> List)

    expr
