open Core__.Common

(* Macro invoke for java_v2 backend - uses gen_class_v2 with static method calls *)

let or_val node next = function Some _ as x -> x | None -> next node

let invoke simplify (ctx : Core__.Frontend_simplify.simplify_ctx) (node : sexp)
    : sexp option =
  Macro_case.invoke simplify node
  |> or_val node (Macro_cond.invoke simplify)
  |> or_val node (Macro_fn.invoke simplify)
  |> or_val node Macro_gen_class_v2.invoke
  |> or_val node (Macro_if_let.invoke simplify)
  |> or_val node (Macro_let.invoke simplify)
  |> or_val node (Macro_ns.invoke ctx simplify)
  |> or_val node (Macro_reify.invoke simplify)
