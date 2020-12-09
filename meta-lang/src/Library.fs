module MetaLang

type Node =
    | Bool of bool
    | String of string
    | Symbol of string
    | Cond of (Node * Node) list
    | ReadDic of string * Node
    | Dic of ((string * Node) list)
    | List of Node list
    | Bind of ((string * Node) list) * (Node list)
    | Call of string * (Node list)
    | Def of string * Node
    | Defn of string * (string list) * (Node list)
    | Module of ((string * string) list) * (Node list)
    | IsNull of Node

let rdic (a: string) =
    let xs = a.Split "."
    ReadDic(sprintf ":%s" xs.[1], Symbol xs.[0])

let dic (a: (string * Node) list): Node = Dic a
let list (a: Node list): Node = List a
let lets (a: (string * _) list) (b: Node list): Node = Bind(a, b)
let call (a: string) (b: Node list): Node = Call(a, b)
let def (a: string) (c: Node): Node = Def(a, c)
let defn (a: string) (b: string list) (c: Node list): Node = Defn(a, b, c)
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
