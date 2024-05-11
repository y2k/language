open Lib__.Common

let compile_code compile prelude_path code =
  let prelude =
    In_channel.(
      with_open_bin ("../../../test/samples/prelude/" ^ prelude_path) input_all)
  in
  NameGenerator.with_scope (fun _ -> compile "main.clj" prelude code)

let assert_ compile prelude_path pos code expected =
  let inner_assert () =
    let actual = compile_code compile prelude_path code in
    Alcotest.(check ~pos string) "#" expected actual
  in
  let loc =
    let f, l, s, e = pos in
    Printf.sprintf "%S, line %d, characters %d-%d" f l s e
  in
  Alcotest.test_case loc `Quick inner_assert

let assert_file compile prelude_path ext p filename =
  let path = "../../../test/samples/" ^ filename in
  let code = In_channel.(with_open_bin path input_all) in
  let expected =
    try In_channel.(with_open_bin (path ^ ext) input_all)
    with _ ->
      Out_channel.with_open_bin (path ^ ext) (fun o ->
          Out_channel.output_string o (compile_code compile prelude_path code));
      In_channel.(with_open_bin (path ^ ext) input_all)
  in
  assert_ compile prelude_path p code expected

let assert_with_import compile prelude_path pos files code expected =
  let module Clj2js = Lib in
  let with_extenal_files files f =
    Lib__Linter.run_resolve
      (fun path ->
        match List.assoc_opt path files with
        | Some x -> x
        | None -> failwith @@ "file not found: " ^ path)
      f
  in
  let inner_assert () =
    let prelude =
      In_channel.with_open_bin
        ("../../../test/samples/prelude/" ^ prelude_path)
        In_channel.input_all
    in
    let actual =
      with_extenal_files files (fun () ->
          NameGenerator.with_scope (fun _ -> compile "main.clj" prelude code))
    in
    let start = 0 in
    let actual = String.sub actual start (String.length actual - start) in
    Alcotest.(check string) "1" expected actual
  in
  let loc =
    let f, l, s, e = pos in
    Printf.sprintf "%S, line %d, characters %d-%d" f l s e
  in
  Alcotest.test_case loc `Quick inner_assert
