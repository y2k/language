open Common

type ns_contex = { ns : string }

let try_unwrap_do = function RBList (_, [ Atom (_, "do*"); body ]) -> body | node -> node

let invoke (global_ctx : context) (node : cljexp) : cljexp =
  let rec invoke (ctx : ns_contex) = function
    | RBList (m, [ (Atom (_, "if") as i); cond; then_; else_ ]) ->
        RBList (m, [ i; invoke ctx cond; invoke ctx then_; invoke ctx else_ ])
    | RBList (m, (Atom (_, "let*") as l) :: SBList (m2, bindings) :: body) ->
        (* RBList [ l; bindings; RBList (List.map (invoke ctx) body) ] *)
        RBList (m, l :: SBList (m2, List.map (invoke ctx) bindings) :: List.map (invoke ctx) body)
    | RBList (m, [ (Atom (_, "if*") as i); cond; then_; else_ ]) ->
        RBList (m, [ i; invoke ctx cond; invoke ctx then_; invoke ctx else_ ])
    | RBList (m, [ (Atom (_, "bind*") as b); n ]) -> RBList (m, [ b; n ])
    | RBList (m, [ (Atom (_, "bind*") as b); n; value ]) -> RBList (m, [ b; n; invoke ctx value ])
    | RBList (m, [ (Atom (_, "bind-update*") as b); n; value ]) -> RBList (m, [ b; n; invoke ctx value ])
    | RBList (m2, [ (Atom (_, "def*") as def); Atom (m, name) ]) -> RBList (m2, [ def; Atom (m, ctx.ns ^ "/" ^ name) ])
    | RBList (m2, [ (Atom (_, "def*") as def); Atom (m, name); value ]) ->
        RBList (m2, [ def; Atom (m, ctx.ns ^ "/" ^ name); invoke ctx value ])
    | RBList (m, (Atom (_, "fn*") as fn_) :: args :: body) -> RBList (m, fn_ :: args :: List.map (invoke ctx) body)
    | RBList
        ( m,
          (Atom (_, "do*") as do_)
          :: RBList (_, [ Atom (_, "ns"); RBList (_, [ _; RBList (_, Atom (_, ns_name) :: _) ]) ])
          :: body ) ->
        let ctx = { ns = ns_name } in
        let body = List.map (invoke ctx) body in
        RBList (m, do_ :: body) |> try_unwrap_do
    | RBList (m, (Atom (_, "do*") as do_) :: body) ->
        let body = List.map (invoke ctx) body in
        RBList (m, do_ :: body) |> try_unwrap_do
    | RBList (m2, Atom (m, fn_name) :: args) ->
        let args = List.map (invoke ctx) args in
        let prefix =
          if StringMap.mem fn_name global_ctx.prelude_scope || String.ends_with ~suffix:"*" fn_name then ""
          else ctx.ns ^ "/"
        in
        RBList (m2, Atom (m, prefix ^ fn_name) :: args)
    | node -> node
  in
  invoke { ns = "user" } node
