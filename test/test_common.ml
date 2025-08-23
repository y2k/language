let tests =
  print_endline @@ Sys.getcwd ();
  [
    ( __LOC__,
      In_channel.(with_open_bin "../../../test/test_common.clj" input_all),
      "0" );
  ]
