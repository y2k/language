module LanguageParser

type ExtLang =
    | ExtModule of ExtLang list
    | ExtDefn of string

type sexp =
    | Atom of string
    | List of sexp list

open FParsec

let private patom =
    (many1Satisfy (fun ch -> isLetter ch || isDigit ch)
     |>> (fun name -> Atom name))

let psexp: Parser<_, unit> =
    let expr, exprImpl = createParserForwardedToRef ()

    exprImpl
    := between
        (spaces .>> choice [ pstring "("; pstring "[" ])
        (choice [ pstring ")"; pstring "]" ])
        ((many (choice [ patom; expr ] .>> spaces))
         |>> List)

    expr
