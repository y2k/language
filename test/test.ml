open Lib__.Common
module U = Utils

let _assert_js = Utils.assert_ (Lib.main_js true)
let _assert_repl = Utils.assert_ (Lib.main_interpreter true)
let _assert_bytecode = Utils.assert_ (Lib.main_bytecode { config_default with log = true })
let _assert_bytecode_repl = Utils.assert_ (Lib.main_bytecode { config_default with log = true; no_deps = true })
let () = Alcotest.run "Tests" [ Js_test.tests; File_test.tests (* ("V2", Core_test.tests) *) ]
