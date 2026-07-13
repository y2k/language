let run ~target input =
  Frontend.Gensym.run (fun () ->
      match Frontend.parse_and_desugar input with
      | Error message -> Error message
      | Ok sexprs -> (
          match target with
          | "eval" -> (
              try
                match List.rev (Backend_eval.Eval.with_filesystem (fun () -> Backend_eval.Eval.eval_all sexprs)) with
                | Backend_eval.Eval.Symbol value :: _ -> Ok value
                | _ -> Ok ""
              with Backend_eval.Eval.Eval_error message -> Error message)
          | "js" -> Ok (Backend_compiler.Js.compile sexprs)
          | "java" -> Ok (Backend_compiler.Java.compile sexprs)
          | target -> Error ("unknown target: " ^ target)))
