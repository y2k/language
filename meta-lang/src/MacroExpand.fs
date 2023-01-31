module MacroExpand

open MetaLang

(*

(case
  input
  :a :result_a
  :b :result_b
  :result_def)

(let [x input]
 (if (= :a x)
    result_a)
    (if (= :b x)
      result_b
      result_def))

*)

let private expand name (args: Node list) : Node =
    match name with
    | "case" ->
        let xs =
            args
            |> Seq.skip 1
            |> Seq.chunkBySize 2
            |> fun xs ->
                Seq.foldBack
                    (fun (xs: Node array) (ac: Node) ->
                        match xs with
                        | [| a; b |] -> Call("if", [ Call("=", [ Symbol "__var__"; a ]); b; ac ])
                        | _ -> xs.[0])
                    xs
                    (Call("unreached", []))

        Let([ "__var__", args[0] ], [ xs ])
    | _ -> Call(name, args)

let rec run (node: Node) : Node =
    match node with
    | Const x -> Const x
    | Symbol x -> Symbol x
    | Call (name, args) -> expand name (args |> List.map run)
    | Def (a, b) -> Def(a, run b)
    | Defn (a, b, c, d) -> Defn(a, b, c, d |> List.map run)
    | Module (_, a) -> Module([], a |> List.map run)
    | Fn (b, c, d) -> Fn(b, c, d |> List.map run)
    | NVector xs -> NVector(xs |> List.map run)
    | NMap xs -> NMap(xs |> Map.map (fun _ x -> run x))
    | Let (xs, body) -> Let(xs |> List.map (fun (k, v) -> k, run v), body |> List.map run)
