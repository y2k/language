open Common

val desugar_and_register : context -> cljexp -> context * cljexp
val parse_and_simplify : context -> string -> string -> context * cljexp
