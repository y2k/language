type bracket = Paren | Bracket | Brace
type loc = { line : int; column : int }
type meta = { loc : loc; type_annotation : string option }
type sexpr = SList of meta * bracket * sexpr list | SAtom of meta * string

let bracket_chars = function Paren -> ("(", ")") | Bracket -> ("[", "]") | Brace -> ("{", "}")

let rec pp_sexpr ppf = function
  | SAtom (_, value) -> Format.pp_print_string ppf value
  | SList (_, bracket, items) ->
      let opening, closing = bracket_chars bracket in
      Format.fprintf ppf "%s@[<hv>%a@]%s" opening pp_sexpr_items items closing

and pp_sexpr_items ppf items = Format.pp_print_list ~pp_sep:(fun ppf () -> Format.fprintf ppf "@ ") pp_sexpr ppf items

let show_sexpr sexpr = Format.asprintf "%a" pp_sexpr sexpr
