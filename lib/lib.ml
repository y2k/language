let main_js log (filename : string) code = Backend_js.main log filename Preludes.js code

let main_js_with_strict log (filename : string) code =
  Backend_js.main log filename Preludes.js code |> Printf.sprintf "\"use strict\";\n%s"

let main_java (base_ns : string) log (filename : string) code =
  Backend_java.main base_ns log filename Preludes.java code

let main_bytecode log (filename : string) code = Backend_bytecode.main log filename Preludes.bytecode code
let main_interpreter log (filename : string) code = Backend_interpreter.main log filename Preludes.interpreter code
