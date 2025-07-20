open Core__.Common

module EvalExecution : sig
  val create_tests :
    (string * string * string) list -> unit Alcotest.test_case list
end = struct
  let create_tests tests =
    tests
    |> List.map (fun (loc, input, expected) ->
           Alcotest.test_case loc `Slow (fun () ->
               let actual =
                 FileReader.with_stub_scope "(defn foo [x] x)"
                   (Core.eval true "/app/src/core/ext/user.clj" "")
                   input
               in
               Alcotest.(check string) "" expected actual))
end

let tests =
  [
    (__LOC__, {|(if (and true 1) 2 3)|}, "2");
    (__LOC__, {|(defn- f [x] x) (f 4)|}, "4");
    (__LOC__, {|(+ 2 2)|}, "4");
    (__LOC__, {|(ns _ (:require ["./lib/eff" :as e])) (e/foo 4)|}, "4");
  ]
  |> EvalExecution.create_tests
