let run ~target input =
  Frontend.Gensym.run (fun () ->
      match Frontend.parse_and_desugar input with
      | Error message -> Error message
      | Ok sexprs -> (
          match target with
          | "eval" -> (
              try
                match List.rev (Backend_eval.Eval.with_filesystem (fun () -> Backend_eval.Eval.eval_all sexprs)) with
                | (Backend_eval.Eval.(String _ | Symbol _ | Int _ | Float _ | Bool _ | Nil) as value) :: _ ->
                    Ok (Backend_eval.Eval_stdlib.to_string value)
                | _ -> Ok ""
              with Backend_eval.Eval.Eval_error message -> Error message)
          | "js" -> Ok (Backend_compiler.Js.compile sexprs)
          | "java" -> Ok (Backend_compiler.Java.compile sexprs)
          | target -> Error ("unknown target: " ^ target)))
