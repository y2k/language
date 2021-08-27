﻿module MetaLang

type Type =
    | Unknown
    | Specific of string
    | RawSexp
    | Dictionary of Map<string, Type>
    | Function of Type list * Type

type Node =
    | Const of string
    | Symbol of string
    | Call of string * (Node list)
    | Def of string * Node
    | Defn of string * ((string * Type) list) * Type * (Node list)
    | Module of ((string * string) list) * (Node list)

type ExtNode =
    | ExtConst of string
    | ExtSymbol of string
    | ExtCall of string * (ExtNode list)
    | ExtDef of string * ExtNode
    | ExtDefn of string * ((string * Type) list) * Type * (ExtNode list)
    | ExtModule of ExtNode list

let rec mapToCoreLang =
    function
    | ExtConst x -> Const x
    | ExtSymbol x -> Symbol x
    | ExtCall (a, b) -> Call(a, b |> List.map mapToCoreLang)
    | ExtDef (a, b) -> Def(a, mapToCoreLang b)
    | ExtDefn (a, b, c, d) -> Defn(a, b, c, d |> List.map mapToCoreLang)
    | ExtModule a -> Module([], a |> List.map mapToCoreLang)

let call (symbolName: string) (b: Node list) : Node = Call(symbolName, b)
let def (a: string) (c: Node) : Node = Def(a, c)

let defn (a: string) (params': string list) (c: Node list) : Node =
    let paramsWithTypes =
        params' |> List.map (fun p -> p, Unknown)

    Defn(a, paramsWithTypes, Unknown, c)

let modules (a: (string * string) list) (b: Node list) = Module(a, b)

module Environment =
    type SymbolType =
        | Func of string * string list
        | Param of string

    type private t = SymbolType list

    let init: t = []

    let register (t: t) s = s :: t

    let resolve (t: t) (name: string) : SymbolType =
        t
        |> List.find
            (fun s ->
                match s with
                | Func (n, _) -> n = name
                | Param n -> n = name)
