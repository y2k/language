let eval (log : bool) (filename : string) (stdin : string) code =
  Backend_eval.eval2 log filename stdin code

let compile (namespace : string) (log : bool) (filename : string)
    (root_dir : string) code =
  Backend_java.compile namespace log filename root_dir code
