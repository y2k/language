open Core__

let test () =
  let actual =
    Backend_sexp.invoke ~builtin_macro:Macro.invoke ~log:true
      {|(defn f [a b c] (+ a b) (+ b c))|}
  in
  let expected =
    {|(
def*
G2m01f
(
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
)
)|}
  in
  Alcotest.(check string) "" expected actual

let tests = [ Alcotest.test_case __LOC__ `Quick test ]
