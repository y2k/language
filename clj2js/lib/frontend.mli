type meta = { line : int; pos : int; symbol : string } [@@deriving show]

val unknown_location : meta

type cljexp =
  | Atom of meta * string
  | RBList of cljexp list
  | SBList of cljexp list
  | CBList of cljexp list
[@@deriving show]

val failnode : string -> cljexp list -> 'a

module StringMap : Map.S with type key = string

type function_decl = { params : cljexp list; body : cljexp list }

type context = {
  filename : string;
  loc : meta;
  start_line : int;
  macros : cljexp StringMap.t;
  functions : function_decl StringMap.t;
}

val empty_context : context

module NameGenerator : sig
  val get_new_var : unit -> string
  val with_scope : (unit -> 'a) -> 'a
end

val parse_and_simplify : context -> int -> string -> string -> context * cljexp

module List : sig
  include module type of List

  val reduce : ('a -> 'a -> 'a) -> 'a list -> 'a
  val reduce_opt : ('a -> 'a -> 'a) -> 'a list -> 'a option
end
