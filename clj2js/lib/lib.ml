let main_js (filename : string) (prelude_macros : string) code =
  code |> Backend_js.main filename prelude_macros

let main_js_with_strict (filename : string) (prelude_macros : string) code =
  main_js filename prelude_macros code |> Printf.sprintf "\"use strict\";\n%s"

let main_java (filename : string) (prelude_macros : string) code =
  Backend_java.main filename prelude_macros code

let main_interpreter (filename : string) (prelude_macros : string) code =
  Backend_interpreter.main filename prelude_macros code
