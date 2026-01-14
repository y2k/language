open Backend__
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
      ~filename:"app/main.clj"
      {|(ns g) (defn f2 [a b] (str a)) (defn f [a b c] (f2 :a "b") (+ b c))|}
  in
  let expected =
    StringMap.of_list
      [
        ("g.f", {|(
fn*
(
a
b
c
)
(
do*
(
g.f2
"a"
"b"
)
(
+
b
c
)
)
)|});
        ("g.f2", {|(
fn*
(
a
b
)
(
str
a
)
)|});
      ]
  in
  Alcotest.(check ~pos:__POS__ map_testable) "" expected actual

let test_println_global_var () =
  let actual =
    Backend_sexp2.invoke ~builtin_macro:Macro.invoke ~log:true
      ~filename:"app/main.clj" {|(ns g) (def x 42) (defn f [] (println x))|}
  in
  let expected =
    StringMap.of_list
      [ ("g.x", "42"); ("g.f", {|(
fn*
(
)
(
println
g.x
)
)|}) ]
  in
  Alcotest.(check ~pos:__POS__ map_testable) "" expected actual

let tests =
  [ Alcotest.test_case "println global var" `Quick test_println_global_var ]
