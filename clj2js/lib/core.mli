type location = { line : int; pos : int; symbol : string } [@@deriving show]

val unknown_location : location

type cljexp =
  | Atom of location * string
  | RBList of cljexp list
  | SBList of cljexp list
  | CBList of cljexp list
[@@deriving show]

val fail_node : cljexp list -> 'a

module StringMap : Map.S with type key = string

type context = {
  filename : string;
  loc : location;
  start_line : int;
  macros : cljexp StringMap.t;
}

val parse_and_simplify : string -> string -> context * cljexp

module List : sig
  include module type of List

  val reduce : ('a -> 'a -> 'a) -> 'a list -> 'a
  val reduce_opt : ('a -> 'a -> 'a) -> 'a list -> 'a option
end
