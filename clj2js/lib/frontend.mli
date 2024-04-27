type meta = { line : int; pos : int; symbol : string } [@@deriving show]

val unknown_location : meta

type cljexp =
  | Atom of meta * string
  | RBList of cljexp list
  | SBList of cljexp list
  | CBList of cljexp list
[@@deriving show]

val fail_node : cljexp list -> 'a
val failnode : string -> cljexp list -> 'a

module StringMap : Map.S with type key = string

type context = {
  filename : string;
  loc : meta;
  start_line : int;
  macros : cljexp StringMap.t;
  out_var : string;
}

module NameGenerator : sig
  val get_new_var : unit -> string
  val reset : unit -> unit
end

val parse_and_simplify : int -> string -> string -> context * cljexp

module List : sig
  include module type of List

  val reduce : ('a -> 'a -> 'a) -> 'a list -> 'a
  val reduce_opt : ('a -> 'a -> 'a) -> 'a list -> 'a option
end
