let code = In_channel.(with_open_bin "../../../test/test_common.clj" input_all)
let tests = [ (__POS__, code, "0") ]
