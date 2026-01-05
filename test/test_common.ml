let code = In_channel.(with_open_bin "../../../test/test_common.clj" input_all)

(** Tests using __POS__ format for Java backend *)
let tests = [ (__POS__, code, "0") ]

(** Tests using __LOC__ format for Eval/JS backends *)
let tests_old = [ (__LOC__, code, "0") ]
