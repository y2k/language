type extension = unit -> Frontend.Desugar.macro list

let parse_and_desugar ?(extensions : extension list = [ Language_macros.create ]) source =
  Frontend.Gensym.run (fun () ->
      (* Instantiate handlers per source file, including dependency files. *)
      let macros = List.concat_map (fun create -> create ()) extensions @ Frontend.Desugar.builtin_macros in
      Frontend.parse_and_desugar ~macros source)

let create_context ?extensions ?env () = Eval.create_context ?env ~parse_source:(parse_and_desugar ?extensions) ()
