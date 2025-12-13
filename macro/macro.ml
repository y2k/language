open Core__.Common

let or_val node next = function Some _ as x -> x | None -> next node

let invoke simplify (ctx : Core__.Frontend_simplify.simplify_ctx) (node : sexp)
    : sexp option =
  Macro_case.invoke simplify node
  |> or_val node (Macro_cond.invoke simplify)
  |> or_val node (Macro_fn.invoke simplify)
  |> or_val node Macro_gen_class.invoke
  |> or_val node (Macro_if_let.invoke simplify)
  |> or_val node (Macro_let.invoke simplify)
  |> or_val node (Macro_ns.invoke ctx simplify)
