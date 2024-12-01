open Common

type contex = { ns : string }

let try_unwrap_do = function
  | RBList [ Atom (_, "do"); body ] -> body
  | node -> node

let invoke (node : cljexp) : cljexp =
  let rec invoke (ctx : contex) = function
    | RBList [ (Atom (_, "bind*") as b); n; value ] ->
        RBList [ b; n; invoke ctx value ]
    | RBList [ (Atom (_, "def") as def); Atom (m, name); value ] ->
        RBList [ def; Atom (m, ctx.ns ^ "/" ^ name); invoke ctx value ]
    | RBList
        ((Atom (_, "do") as do_)
        :: RBList (Atom (_, "ns") :: Atom (_, ns_name) :: _)
        :: body) ->
        let ctx = { ns = ns_name } in
        let body = List.map (invoke ctx) body in
        RBList (do_ :: body) |> try_unwrap_do
    | RBList ((Atom (_, "do") as do_) :: body) ->
        let body = List.map (invoke ctx) body in
        RBList (do_ :: body) |> try_unwrap_do
    | RBList (Atom (m, fn_name) :: args) ->
        let args = List.map (invoke ctx) args in
        let prefix =
          match fn_name with
          | "fn*" | "+" | "list" | "vector" | "atom" | "deref" | "reset!"
          | "swap!" | "str" ->
              ""
          | _ -> ctx.ns ^ "/"
        in
        RBList (Atom (m, prefix ^ fn_name) :: args)
    | node -> node
  in
  invoke { ns = "user" } node
