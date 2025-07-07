module A = Alcotest
module Js = Core__.Backend_js

let compile code = Js.compile code

let run_code code =
  let path = Filename.temp_file "test" ".js" in
  prerr_endline @@ "Path: " ^ path;
  Out_channel.(with_open_bin path (fun f -> output_string f code));
  Sys.command (Printf.sprintf "node %s" path) |> string_of_int

let create_test =
  List.map (fun (loc, (input : string), expected) ->
      A.test_case loc `Slow (fun () ->
          let compiled = compile input in
          let actual = run_code compiled in
          A.check A.string "" expected actual))

let tests =
  ( "JS",
    [ (__LOC__, {|(defn main [] 42)|}, "42") ]
    (* *)
    |> create_test )
