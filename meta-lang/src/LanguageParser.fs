module LanguageParser

type sexp =
    | Atom of string
    | List of sexp list
    | Vector of sexp list
    | SMap of sexp list

module SexpParser =
    open FParsec

    let private patom =
        (choice
            [ (between (pchar '"') (pchar '"') (manySatisfy (fun ch -> ch <> '"'))
               |>> (sprintf "\"%s\""))
              many1Satisfy (fun ch ->
                  isLetter ch
                  || isDigit ch
                  || ch = '='
                  || ch = '?'
                  || ch = '.'
                  || ch = '-'
                  || ch = '_'
                  || ch = '+'
                  || ch = ':') ]
         |>> (fun name -> Atom name))

    let private mkCommonParse openCh closeCh toCollection expr : Parser<_, unit> =
        let discard = pstring "#_" >>. expr |>> (fun _ -> None)

        between
            (spaces .>> choice [ pstring openCh ])
            (choice [ pstring closeCh ])
            ((many (choice [ discard; patom |>> Some; expr |>> Some ] .>> spaces))
             |>> (List.choose id >> toCollection))

    let private psexp: Parser<_, unit> =
        let expr, exprImpl = createParserForwardedToRef ()

        exprImpl.Value <-
            choice
                [ mkCommonParse "(" ")" List expr
                  mkCommonParse "[" "]" Vector expr
                  mkCommonParse "{" "}" SMap expr ]

        expr

    let parse str =
        match run psexp str with
        | Success (x, _, _) -> x
        | Failure (a, b, _) -> failwithf "Cant parse (%O %O)" a b

open MetaLang
open System.Text.RegularExpressions

let rec private compileFuncBody sexp =
    match sexp with
    | List (Atom "let" :: Vector args :: body) ->
        let letArgs =
            args
            |> List.chunkBySize 2
            |> List.map (function
                | [ Atom name; sexp ] -> name, compileFuncBody sexp
                | n -> failwithf "illegal node inside let: %O" n)

        ExtLet(letArgs, body |> List.map compileFuncBody)
    | List (Atom "fn" :: Vector argsSexp :: body) ->
        let args =
            argsSexp
            |> List.map (function
                | Atom argName -> argName, Unknown
                | n -> failwithf "invalid node %O" n)

        ExtFn(args, Unknown, body |> List.map compileFuncBody)
    | List (Atom fname :: args) -> ExtCall(fname, args |> List.map compileFuncBody)
    | Atom sym ->
        if sym.StartsWith(':') then ExtConst(sym.Substring(1))
        else if sym = "true" || sym = "false" then ExtConst sym
        else if Regex.IsMatch(sym, "^[a-z].*$") then ExtSymbol sym
        else ExtConst sym
    | Vector xs -> ExtVector(xs |> List.map compileFuncBody)
    | SMap xs ->
        xs
        |> List.chunkBySize 2
        |> List.map (function
            | Atom k :: v :: [] when k.StartsWith(':') -> k.Substring(1), compileFuncBody v
            | s -> failwithf "Invalid map node %O" s)
        |> Map.ofList
        |> ExtMap
    | _ -> failwithf "invalid node %O" sexp

let private compileDefn sexp =
    match sexp with
    | List (Atom "def" :: Atom valName :: body :: []) -> ExtDef(valName, compileFuncBody body)
    | List (Atom "defn" :: Atom funcName :: Vector argsSexp :: body) ->
        let args =
            argsSexp
            |> List.map (function
                | Atom argName -> argName, Unknown
                | n -> failwithf "invalid node %O" n)

        ExtDefn(funcName, args, Unknown, body |> List.map compileFuncBody)
    | _ -> failwithf "invalid node %O" sexp

let compile str =
    match SexpParser.parse str with
    | List (Atom "module" :: methods) -> ExtModule(methods |> List.map compileDefn)
    | p -> failwithf "invalid program (%O)" p
