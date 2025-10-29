open Core__
module StringMap = Map.Make (String)

let map_testable =
  Alcotest.testable
    (fun fmt map ->
      let map_formater =
        StringMap.bindings map
        |> List.map (fun (k, v) ->
               Fmt.Dump.field k (fun _ -> v) Fmt.Dump.string)
        |> Fmt.Dump.record
      in
      map_formater fmt map)
    (StringMap.equal String.equal)

let test () =
  let actual =
    Backend_sexp2.invoke ~builtin_macro:Macro.invoke ~log:true
      ~filename:"app/main.clj" {|(defn f [a b c] (+ a b) (+ b c))|}
  in
  let expected =
    StringMap.of_list
      [ ("G10m7308631371f", {|(
fn*
a b c
13
(
do*
(
+
a
b
)
(
+
b
c
)
)
)|}) ]
  in
  Alcotest.(check ~pos:__POS__ map_testable) "" expected actual

let tests = [ Alcotest.test_case "" `Quick test ]
