let main_js log (filename : string) (prelude_macros : string) code =
  code |> Backend_js.main log filename prelude_macros

let main_js_with_strict log (filename : string) (prelude_macros : string) code =
  main_js log filename prelude_macros code
  |> Printf.sprintf "\"use strict\";\n%s"

let main_java (filename : string) (prelude_macros : string) code =
  Backend_java.main filename prelude_macros code

let main_interpreter (filename : string) (prelude_macros : string) code =
  Backend_interpreter.main filename prelude_macros code
