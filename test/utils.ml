open Lib__.Common

let compile_code compile code =
  NameGenerator.with_scope (fun _ ->
      FileReader.with_stub_scope {|(ns lib) (defn f [x] x)|} (compile "app/main.clj") code)

let split_string str sep =
  let regexp_sep = Str.regexp_string sep in
  Str.split regexp_sep str

let fold_samples xs (line : string) =
  if line = "=============================" then "" :: xs
  else match List.hd xs with "" -> line :: List.tl xs | x -> (x ^ "\n" ^ line) :: List.tl xs

let make_samples_test compiler file_name =
  let output_file_name = "../../../test/samples/output/" ^ file_name ^ ".txt" in
  let samples = In_channel.with_open_bin ("../../../test/samples/" ^ file_name ^ ".txt") In_channel.input_lines in
  let expected =
    try In_channel.with_open_bin output_file_name (In_channel.fold_lines fold_samples []) |> List.rev
    with _ ->
      let compiled =
        samples
        |> List.map (fun line ->
               print_endline @@ __LOC__ ^ " " ^ line;
               let r = try compile_code compiler line with e -> Printf.sprintf "%s" (Printexc.to_string e) in
               "=============================\n" ^ r)
        |> List.reduce __LOC__ (Printf.sprintf "%s\n%s")
      in
      Out_channel.with_open_bin output_file_name (Fun.flip Out_channel.output_string compiled);
      []
  in
  if expected = [] then []
  else
    samples
    |> List.map2
         (fun expected line ->
           Alcotest.test_case line `Quick (fun () ->
               let actual : string = compile_code compiler line in
               Alcotest.(check ~pos:__POS__ string) "#" expected actual))
         expected

let assert_ compile pos code expected =
  let inner_assert () =
    let actual = compile_code compile code in
    Alcotest.(check ~pos string) "#" expected actual
  in
  let loc =
    let f, l, s, e = pos in
    Printf.sprintf "%S, line %d, characters %d-%d" f l s e
  in
  Alcotest.test_case loc `Quick inner_assert

let assert_file compile ext p filename =
  let path = "../../../test/samples/" ^ filename in
  let code = In_channel.(with_open_bin path input_all) in
  let expected =
    try In_channel.(with_open_bin (path ^ ext) input_all)
    with _ ->
      Out_channel.with_open_bin (path ^ ext) (fun o -> Out_channel.output_string o (compile_code compile code));
      In_channel.(with_open_bin (path ^ ext) input_all)
  in
  assert_ compile p code expected
