open Frontend

let lower input =
  match Frontend.parse_and_desugar input with
  | Ok sexprs -> Gensym.run (fun () -> Backend_compiler.Lowering_expression_to_statement.lower sexprs)
  | Error message -> Alcotest.fail message

let rec has_symbol_only_function_bindings = function
  | SAtom _ -> true
  | SList (_, _, SAtom (_, "fn*") :: SList (_, _, params) :: body) ->
      List.for_all (function SAtom _ -> true | _ -> false) params
      && List.for_all has_symbol_only_function_bindings body
  | SList (_, _, SAtom (_, "let*") :: SAtom _ :: value :: body) ->
      has_symbol_only_function_bindings value && List.for_all has_symbol_only_function_bindings body
  | SList (_, _, items) -> List.for_all has_symbol_only_function_bindings items

let function_patterns_are_normalized () =
  Alcotest.(check bool)
    "symbol-only function parameters and bindings" true
    (lower {|(def f (fn [[a {:name n}]] (str a n)))|} |> List.for_all has_symbol_only_function_bindings)

let () =
  Alcotest.run "lowering"
    [ ("fn patterns", [ Alcotest.test_case "normalizes destructuring" `Quick function_patterns_are_normalized ]) ]
