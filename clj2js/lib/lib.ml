let main_js (filename : string) (prelude_macros : string) code =
  (match filename with
  | "prelude" ->
      (Backend_js.prelude |> Backend_js.main "prelude" prelude_macros, "")
  | filename ->
      ( code |> Backend_js.main filename prelude_macros,
        Backend_js.prelude_imports ))
  |> fun ((ctx, code), imports) ->
  (ctx, Printf.sprintf "\"use strict\";\n%s\n%s" imports code)

let main_kt (filename : string) code = Backend_kt.main filename code
let main_sh (filename : string) code = Backend_bash.main filename code
let main_json (filename : string) code = Backend_ast.main filename code

let main_java (filename : string) (prelude_macros : string) code =
  Backend_java.main filename prelude_macros code
