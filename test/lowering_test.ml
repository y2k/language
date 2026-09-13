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

let instance_check () =
  let input = "(instance? (do String) value)" in
  let parsed = match Frontend.parse_and_desugar input with Ok forms -> forms | Error e -> Alcotest.fail e in
  Alcotest.(check bool) "preserves type expression and metadata" true (parsed = lower input);
  match lower "(do (instance? Object (effect)) nil)" with
  | [
   SList
     ( _,
       _,
       [
         SAtom (_, "do");
         SList
           ( _,
             _,
             [
               SAtom (_, "let*");
               SAtom (_, _);
               SList (_, _, [ SAtom (_, "instance?"); SAtom (_, "Object"); SList (_, _, [ SAtom (_, "effect") ]) ]);
             ] );
         SAtom (_, "nil");
       ] );
  ] ->
      ()
  | _ -> Alcotest.fail "discarded instance? must bind its value once"

let () =
  Alcotest.run "lowering"
    [
      ("fn patterns", [ Alcotest.test_case "normalizes destructuring" `Quick function_patterns_are_normalized ]);
      ("instance?", [ Alcotest.test_case "type preservation and discard" `Quick instance_check ]);
    ]
