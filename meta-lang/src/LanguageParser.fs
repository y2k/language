module LanguageParser

type sexp =
    | Atom of string
    | List of sexp list

module private SexpParser =
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

open MetaLang

let compileFuncBody sexp = failwith "???"

let compileDefn sexp =
    match sexp with
    | List (Atom "defn" :: Atom funcName :: List argsSexp :: body) ->
        let args =
            argsSexp
            |> List.map
                (function
                | Atom argName -> argName, Unknown
                | n -> failwithf "invalide node %O" n)

        ExtDefn(funcName, args, Unknown, compileFuncBody body)
    | _ -> failwithf "invalide node %O" sexp

let compile (sexp: sexp) =
    match sexp with
    | List (Atom "module" :: methods) -> ExtModule(methods |> List.map compileDefn)
    | _ -> failwith "invalide nodes"
