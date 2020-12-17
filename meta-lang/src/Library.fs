module MetaLang

type Type =
    | Unknown
    | Specific of string
    | Dictionary of Map<string, Type>

type Node =
    | Bool of bool
    | Int of int
    | String of string
    | Symbol of string
    | Cond of (Node * Node) list
    | ReadDic of string * Node
    | Dic of ((string * Node) list)
    | Bind of ((string * Node) list) * (Node list)
    | Call of string * (Node list)
    | Def of string * Node
    | Defn of string * ((string * Type) list) * (Node list)
    | Module of ((string * string) list) * (Node list)
    | IsNull of Node

let rdic (a: string) =
    let xs = a.Split "."
    ReadDic(sprintf ":%s" xs.[1], Symbol xs.[0])

let dic (a: (string * Node) list): Node = Dic a
let lets (a: (string * _) list) (b: Node list): Node = Bind(a, b)
let call (a: string) (b: Node list): Node = Call(a, b)
let def (a: string) (c: Node): Node = Def(a, c)

let defn (a: string) (params': string list) (c: Node list): Node =
    let paramsWithTypes =
        params' |> List.map (fun p -> p, Unknown)

    Defn(a, paramsWithTypes, c)

let modules (a: (string * string) list) (b: Node list) = Module(a, b)

module Environment =
    type SymbolType =
        | Func of string * string list
        | Param of string

    type private t = SymbolType list

    let init: t = []

    let register (t: t) s = s :: t

    let resolve (t: t) (name: string): SymbolType =
        t
        |> List.find
            (fun s ->
                match s with
                | Func (n, _) -> n = name
                | Param n -> n = name)
