let main_js (filename : string) code =
  (match filename with
  | "prelude" -> (Backend_js.prelude |> Backend_js.main "prelude", "")
  | filename -> (code |> Backend_js.main filename, Backend_js.prelude_imports))
  |> fun ((ctx, code), imports) ->
  (ctx, Printf.sprintf "\"use strict\";\n%s\n%s" imports code)

let main_kt (filename : string) code = Backend_kt.main filename code
let main_sh (filename : string) code = Backend_bash.main filename code
let main_json (filename : string) code = Backend_ast.main filename code
