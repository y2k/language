open Frontend

type value =
  | Nil
  | Bool of bool
  | String of string
  | Int of int
  | Float of float
  | Symbol of string
  | List of value list
  | HashMap of (value * value) list
  | Closure of closure
  | Atom of value ref
  | Regex of Re.re

and closure = User of sexpr list * sexpr list * env * string | Native of builtin_fn
and builtin_fn = (value -> value list -> value) -> value list -> value
and env = (string * value) list

exception Eval_error of string

let truthy = function Nil | Bool false -> false | _ -> true
