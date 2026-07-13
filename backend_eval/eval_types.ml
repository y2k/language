open Frontend

type value = Symbol of string | List of value list | HashMap of (value * value) list | Closure of closure
and closure = User of sexpr list * sexpr list * env * string | Native of builtin_fn
and builtin_fn = (value -> value list -> value) -> value list -> value
and env = (string * value) list

exception Eval_error of string
