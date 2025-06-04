open Lib__.Common
open Core_base

let get_macro node =
  let ctx = Backend_eval.eval1 "" node |> fst in
  Backend_eval.get_all_functions ctx

let eval (filename : string) (stdin : string) code =
  let rec serialize_to_string = function
    | SAtom (_, x) -> x
    | SList (_, xs) -> xs |> List.map serialize_to_string |> String.concat ""
  in
  Lib__.Common.NameGenerator.with_scope (fun () ->
      Backend_eval.eval code filename stdin
      |> snd |> Utils.obj_to_sexp |> serialize_to_string |> unpack_string)

let compile (namespace : string) (log : bool) (filename : string)
    (root_dir : string) code =
  Lib__.Common.NameGenerator.with_scope (fun () ->
      Parser.parse_text code
      (* *)
      |> Simplify.do_simplify get_macro
           { log; macro = Prelude.prelude_java_macro; filename; root_dir }
      |> JavaCompiler.do_compile { filename; root_dir; namespace })
