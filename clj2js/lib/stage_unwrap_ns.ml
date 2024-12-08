open Common

type ns_contex = { ns : string }

let try_unwrap_do = function
  | RBList [ Atom (_, "do"); body ] -> body
  | node -> node

let invoke (global_ctx : context) (node : cljexp) : cljexp =
  (* global_ctx.scope
     |> Fun.flip (StringMap.fold (fun k _v a -> a ^ ", " ^ k)) ""
     |> print_endline; *)
  let rec invoke (ctx : ns_contex) = function
    | RBList [ (Atom (_, "if") as i); cond; then_; else_ ] ->
        RBList [ i; invoke ctx cond; invoke ctx then_; invoke ctx else_ ]
    | RBList ((Atom (_, "let*") as l) :: SBList bindings :: body) ->
        (* RBList [ l; bindings; RBList (List.map (invoke ctx) body) ] *)
        RBList
          (l
          :: SBList (List.map (invoke ctx) bindings)
          :: List.map (invoke ctx) body)
    | RBList [ (Atom (_, "if*") as i); cond; then_; else_ ] ->
        RBList [ i; invoke ctx cond; invoke ctx then_; invoke ctx else_ ]
    | RBList [ (Atom (_, "bind*") as b); n ] -> RBList [ b; n ]
    | RBList [ (Atom (_, "bind*") as b); n; value ] ->
        RBList [ b; n; invoke ctx value ]
    | RBList [ (Atom (_, "bind-update*") as b); n; value ] ->
        RBList [ b; n; invoke ctx value ]
    | RBList [ (Atom (_, "def") as def); Atom (m, name) ] ->
        RBList [ def; Atom (m, ctx.ns ^ "/" ^ name) ]
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
          if
            StringMap.mem fn_name global_ctx.prelude_scope
            || String.ends_with ~suffix:"*" fn_name
          then ""
          else ctx.ns ^ "/"
          (* match fn_name with
             | "fn*" | "+" | "list" | "vector" | "atom" | "deref" | "reset!"
             | "swap!" | "str" ->
                 ""
             | _ -> ctx.ns ^ "/" *)
        in
        RBList (Atom (m, prefix ^ fn_name) :: args)
    | node -> node
  in
  invoke { ns = "user" } node
