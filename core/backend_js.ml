open Lib__.Common
open Common
module Ng = Lib__.Common.NameGenerator

type context = { filename : string; root_dir : string; namespace : string }

let do_compile (_ctx : context) (node : sexp) = failsexp __LOC__ [ node ]

(* *)

let get_macro node =
  let ctx = Backend_eval.eval1 "" node |> fst in
  Backend_eval.get_all_functions ctx

let compile code =
  let log = false in
  let filename = "" in
  let root_dir = "" in
  let namespace = "" in
  Ng.with_scope (fun () ->
      prerr_endline @@ "LOG: code: " ^ code;
      code
      |> Frontent_simplify.do_simplify get_macro
           {
             log;
             macro = Prelude.prelude_js_macro;
             filename;
             root_dir;
             compile =
               (fun _ -> SList (meta_empty, [ SAtom (meta_empty, "do") ]));
           }
      |> Lib__.Stage_convert_if_to_statment.invoke
      |> log_stage log "if_to_statement "
      (* |> Stage_resolve_import.do_resolve filename root_dir
      |> log_stage log "Stage_resolve_import" *)
      (* |> Stage_alias_to_class.do_invoke namespace root_dir filename
      |> log_stage log "Stage_alias_to_class" *)
      (* |> Stage_fun_args_type.invoke
      |> log_stage log "Stage_fun_args_type" *)
      |> Stage_flat_do.invoke
      |> log_stage log "Stage_flat_do"
      |> do_compile { filename; root_dir; namespace })
