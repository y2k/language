let parse source =
  match Frontend.parse_and_desugar source with Ok forms -> forms | Error message -> Alcotest.fail message

let definitions source = Backend_compiler.Fex.definitions (parse source)
let records source = match definitions source with [ (_, records) ] -> records | _ -> Alcotest.fail "expected one RHS"
let text records = String.concat "\n" (List.map (String.concat " ") records)
let check source expected = Alcotest.(check string) source expected (text (records source))

let core () =
  check "(defn calculate [x] (+ x 1))" "fn 2 x\ncall 3 4 5\nbuiltin +\nlocal x\nint 1";
  check "(defn choose [x] (cond x true false false))" "fn 2 x\nif 3 4 5\nlocal x\ntrue\nif 6 7 8\nfalse\nfalse\nnil";
  check "(def x (let [x 1 x (+ x 1)] (fn [y] (+ x y))))"
    "let x 2 3\n\
     int 1\n\
     let x 4 8\n\
     call 5 6 7\n\
     builtin +\n\
     local x\n\
     int 1\n\
     fn 9 y\n\
     call 10 11 12\n\
     builtin +\n\
     local x\n\
     local y";
  check "(defn six [a b c d e f] (f a b c d e))"
    "fn 2 a b c d e f\ncall 3 4 5 6 7 8\nlocal f\nlocal a\nlocal b\nlocal c\nlocal d\nlocal e"

let resolution () =
  let result = definitions "(ns app) (defn recur [x] (recur x)) (def x +) (def + 1) (defn f [+] +)" in
  Alcotest.(check (list (pair string string)))
    "recursion, forward def, local shadow"
    [
      ("app.recur", "fn 2 x\ncall 3 4\nglobal app.recur\nlocal x");
      ("app.x", "global app.%2B");
      ("app.%2B", "int 1");
      ("app.f", "fn 2 +\nlocal +");
    ]
    (List.map (fun (key, rows) -> (key, text rows)) result)

let quote_and_collections () =
  check "(def x 'nil)" "string bmls";
  check "(def x '[1])" "call 2 3 4\nbuiltin list\nstring bGlzdA==\nstring MQ==";
  check "(def x '(a (nil)))" "call 2 3 4\nbuiltin list\nstring YQ==\ncall 5 6\nbuiltin list\nstring bmls";
  check "(def x [:key {\"k\" 1}])"
    "call 2 3 4\nbuiltin list\nstring a2V5\ncall 5 6 7\nbuiltin hash-map\nstring aw==\nint 1"

let destructuring () =
  let rows = records "(defn f [[a {:name n}]] (let [[b] (source)] (str a n b)))" in
  let row i = List.nth rows (int_of_string i - 1) in
  let binding i =
    match row i with [ "let"; name; value; body ] -> (name, value, body) | _ -> Alcotest.fail "expected let"
  in
  match List.hd rows with
  | [ "fn"; body; parameter ] ->
      Alcotest.(check bool) "fresh parameter" true (String.starts_with ~prefix:"G__" parameter);
      let vector, value, body = binding body in
      Alcotest.(check (list string)) "parameter used once" [ "local"; parameter ] (row value);
      let a, value, body = binding body in
      Alcotest.(check string) "first binding" "a" a;
      (match row value with
      | [ "call"; callee; target; index ] ->
          Alcotest.(check (list string)) "lookup" [ "builtin"; "get" ] (row callee);
          Alcotest.(check (list string)) "lookup vector" [ "local"; vector ] (row target);
          Alcotest.(check (list string)) "first index" [ "int"; "0" ] (row index)
      | _ -> Alcotest.fail "expected lookup");
      let map, _, body = binding body in
      Alcotest.(check bool) "fresh nested temporary" true (map <> vector && vector <> parameter);
      let n, value, _ = binding body in
      Alcotest.(check string) "nested name" "n" n;
      (match row value with
      | [ "call"; _; target; key ] ->
          Alcotest.(check (list string)) "lookup map" [ "local"; map ] (row target);
          Alcotest.(check (list string)) "decoded key" [ "string"; "bmFtZQ==" ] (row key)
      | _ -> Alcotest.fail "expected map lookup");
      Alcotest.(check int)
        "effectful RHS occurs once" 1
        (List.length (List.filter (( = ) [ "global"; "user.source" ]) rows));
      Alcotest.(check string)
        "body uses destructured locals" "builtin str\nlocal a\nlocal n\nlocal b"
        (text (List.filteri (fun i _ -> i >= List.length rows - 4) rows))
  | _ -> Alcotest.fail "expected function"

let serialization () =
  Alcotest.(check string)
    "calculate bytes" "fn 2 x\ncall 3 4 5\nbuiltin +\nlocal x\nint 1\n"
    (Backend_compiler.Fex.serialize (records "(defn calculate [x] (+ x 1))"));
  List.iter
    (fun (source, expected) -> check ("(def x " ^ source ^ ")") expected)
    [
      ("nil", "nil");
      ("true", "true");
      ("false", "false");
      ("-2147483648", "int -2147483648");
      ("2147483647", "int 2147483647");
      ("-42", "int -42");
      ("1.25", "float 1.25");
      ("1e20", "float 1e+20");
      ("\"\"", "string -");
      ("\"hello world\"", "string aGVsbG8gd29ybGQ=");
      ("\"Я\"", "string 0K8=");
      ("\"a\\n\\t\\r\\\"\\\\\"", "string YQoJDSJc");
      ("\"%0A\"", "string JTBB");
      ("\"\\q\\u0041\"", "string XHFcdTAwNDE=");
      ("(do)", "do");
      ("(fn [] (f))", "fn 2\ncall 3\nglobal user.f");
      ("(if true 1)", "if 2 3 4\ntrue\nint 1\nnil");
      ("(fn [] (f) (g))", "fn 2\ndo 3 5\ncall 4\nglobal user.f\ncall 6\nglobal user.g");
    ];
  let rows = records "(defn f [a b c d e f] (let [x (g a) y (if b (h c) (i d))] (do x (f y e))))" in
  List.iteri
    (fun index fields ->
      let refs =
        match fields with
        | "fn" :: body :: _ -> [ body ]
        | "let" :: _ :: refs | ("call" | "if" | "do") :: refs -> refs
        | _ -> []
      in
      List.iter
        (fun reference ->
          let address = int_of_string reference in
          Alcotest.(check bool) "forward child within RHS" true (address > index + 1 && address <= List.length rows))
        refs)
    rows;
  check "(def x (if (test) (then-effect 1) (else-effect 2)))"
    "if 2 4 7\n\
     call 3\n\
     global user.test\n\
     call 5 6\n\
     global user.then-effect\n\
     int 1\n\
     call 8 9\n\
     global user.else-effect\n\
     int 2"

let process program args input =
  let stdout, stdin, stderr =
    Unix.open_process_args_full program (Array.of_list (program :: args)) (Unix.environment ())
  in
  output_string stdin input;
  close_out stdin;
  let output = In_channel.input_all stdout in
  let error = In_channel.input_all stderr in
  (Unix.close_process_full (stdout, stdin, stderr), output, error)

let main = Unix.realpath (Filename.concat (Filename.dirname Sys.executable_name) "../bin/main.exe")
let cli args source = process main args source

let successful (status, output, error) =
  if status <> Unix.WEXITED 0 then Alcotest.failf "process failed: %s" error;
  Alcotest.(check string) "empty stderr" "" error;
  output

let runner ?source_file source =
  match Language_main.Runner.run ?source_file ~target:"fex" source with
  | Ok output -> output
  | Error message -> Alcotest.fail message

let bundle_cli () =
  List.iter
    (fun (source, expected) ->
      Alcotest.(check string) "runner bundle bytes" expected (runner source);
      Alcotest.(check string) "CLI bundle bytes" expected (successful (cli [ "--target"; "fex" ] source));
      Alcotest.(check string) "source filename ignored" expected (runner ~source_file:"any.clj" source))
    [
      ("", "");
      ("(ns app.logic)", "");
      ("(ns app.logic) (def answer 42)", "app.logic.answer\naW50IDQyCg==\n");
      ("(def a 42) (def b 43) (def a 43)", "user.a\naW50IDQyCg==\nuser.b\naW50IDQzCg==\nuser.a\naW50IDQzCg==\n");
    ];
  let source = "(defn calculate [x] (+ x 1)) (def text \"Я\\n\")" in
  let decoded =
    successful
      (process "node"
         [
           "-e";
           "const s=require('fs').readFileSync(0,'utf8').split('\\n'); for(let i=1;i<s.length;i+=2) \
            process.stdout.write(Buffer.from(s[i],'base64'));";
         ]
         (runner source))
  in
  Alcotest.(check string)
    "independent decoding of both Base64 layers" "fn 2 x\ncall 3 4 5\nbuiltin +\nlocal x\nint 1\nstring 0K8K\n" decoded;
  let cwd = Sys.getcwd () in
  let dir = Filename.temp_dir "fex-compiler-" "" in
  Fun.protect
    ~finally:(fun () ->
      Unix.chdir cwd;
      Unix.rmdir dir)
    (fun () ->
      Unix.chdir dir;
      ignore (successful (cli [ "--target"; "fex"; "--source-file"; "missing.clj" ] "(def answer 42)"));
      Alcotest.(check (array string)) "no code files" [||] (Sys.readdir "."));
  List.iter
    (fun (target, source) ->
      let expected =
        match Language_main.Runner.run ~target source with
        | Ok output -> output
        | Error message -> Alcotest.fail message
      in
      Alcotest.(check string) "existing CLI newline" (expected ^ "\n") (successful (cli [ "--target"; target ] source)))
    [ ("eval", "42"); ("js", "(def answer 42)"); ("java", "(def answer 42)") ]

let with_unpack_dir f =
  let root = Filename.temp_dir "fex-unpack-" "" in
  let dir = Filename.concat root "out" in
  Fun.protect
    ~finally:(fun () ->
      if Sys.file_exists dir then (
        Array.iter (fun name -> Sys.remove (Filename.concat dir name)) (Sys.readdir dir);
        Unix.rmdir dir);
      Unix.rmdir root)
    (fun () -> f dir)

let unpack dir input = cli [ "--unpack-fex"; "--output-dir"; dir ] input
let read_file path = In_channel.with_open_bin path In_channel.input_all

let unpack_bytes () =
  with_unpack_dir (fun dir ->
      let bundle = successful (cli [ "--target"; "fex" ] "(ns app.logic) (def answer 42)") in
      Alcotest.(check string) "source to files" "app.logic.answer\n" (successful (unpack dir bundle));
      Alcotest.(check string) "compiled RHS" "int 42\n" (read_file (Filename.concat dir "app.logic.answer"));
      List.iter
        (fun input ->
          Alcotest.(check string) "processed key" "app.raw\n" (successful (unpack dir input));
          Alcotest.(check string) "opaque binary RHS" "\000\255\r\nx" (read_file (Filename.concat dir "app.raw")))
        [ "app.raw\nAP8NCng=\n"; "app.raw\r\nAP8NCng=\r\n"; "app.raw\nAP8NCng=" ])

let rejected_unpack (status, output, error) =
  Alcotest.(check bool) "nonzero exit" true (match status with Unix.WEXITED n -> n <> 0 | _ -> false);
  Alcotest.(check bool) "diagnostic on stderr" true (error <> "");
  output

let unpack_names_and_errors () =
  with_unpack_dir (fun dir ->
      let keys = [ "app.foo"; "app.!foo"; "app.!f!o!o"; "app.%21foo"; "app.%D0%AF"; "app.%D1%8F" ] in
      let source = "(ns app) (def foo 42) (def Foo 42) (def FOO 42) (def !foo 42) (def Я 42) (def я 42)" in
      let bundle = successful (cli [ "--target"; "fex" ] source) in
      Alcotest.(check string) "literal canonical keys" (String.concat "\n" keys ^ "\n") (successful (unpack dir bundle));
      List.iter (fun key -> Alcotest.(check string) key "int 42\n" (read_file (Filename.concat dir key))) keys;
      List.iter
        (fun key ->
          Alcotest.(check string) "unsafe name rejected" "" (rejected_unpack (unpack dir (key ^ "\naW50IDQyCg==\n"))))
        [ "app key"; "../outside"; "/outside"; ".."; "."; "app/child"; "app\\child"; "app\000key"; "" ];
      List.iter
        (fun input -> Alcotest.(check string) "bad pair rejected" "" (rejected_unpack (unpack dir input)))
        [
          "app.bad\n";
          "app.bad";
          "app.bad\n???=\n";
          "app.bad\nYQ\n";
          "app.bad\nYR==\n";
          "app.bad\nYQ==AAAA\n";
          "app.bad\nYWJ=\n";
          "app.bad\nY Q=\n";
          "\napp.bad\nYQ==\n";
        ];
      Alcotest.(check bool) "bad payload not written" false (Sys.file_exists (Filename.concat dir "app.bad"));
      Alcotest.(check string)
        "empty payload is consumed" "app.empty\napp.next\n"
        (successful (unpack dir "app.empty\n\napp.next\nYQ==\n"));
      Alcotest.(check string) "empty bytes" "" (read_file (Filename.concat dir "app.empty"));
      let link = Filename.concat dir "app.link" in
      Unix.symlink "app.foo" link;
      Alcotest.(check string) "symlink rejected" "" (rejected_unpack (unpack dir "app.link\nYQ==\n"));
      Alcotest.(check string) "symlink target preserved" "int 42\n" (read_file (Filename.concat dir "app.foo")));
  Alcotest.(check string) "output dir required" "" (rejected_unpack (cli [ "--unpack-fex" ] "app.x\nYQ==\n"))

let unpack_updates () =
  with_unpack_dir (fun dir ->
      ignore (successful (unpack dir "app.x\nYQ==\napp.neighbor\nb2xk\n"));
      let path = Filename.concat dir "app.x" in
      Unix.utimes path 1000. 1000.;
      Alcotest.(check string) "unchanged key reported" "app.x\n" (successful (unpack dir "app.x\nYQ==\n"));
      Alcotest.(check (float 0.)) "unchanged mtime" 1000. (Unix.stat path).Unix.st_mtime;
      Alcotest.(check string)
        "all repeated keys reported" "app.x\napp.x\napp.x\n"
        (successful (unpack dir "app.x\nYg==\napp.x\nYg==\napp.x\nYw==\n"));
      Alcotest.(check string) "last key wins" "c" (read_file path);
      Alcotest.(check bool) "changed mtime" true ((Unix.stat path).Unix.st_mtime <> 1000.);
      Alcotest.(check string)
        "successful prefix reported" "app.x\n"
        (rejected_unpack (unpack dir "app.x\nZA==\napp.bad\n?\napp.later\nYQ==\n"));
      Alcotest.(check string) "prefix remains after failure" "d" (read_file path);
      Alcotest.(check bool) "later pair not processed" false (Sys.file_exists (Filename.concat dir "app.later"));
      Alcotest.(check string) "neighbor preserved" "old" (read_file (Filename.concat dir "app.neighbor"));
      Alcotest.(check string) "EOF between pairs succeeds" "" (successful (unpack dir "")))

let canonical_names () =
  List.iter
    (fun (namespace, name, expected) ->
      let source = Printf.sprintf "(ns %s) (def %s %s/%s)" namespace name namespace name in
      Alcotest.(check (list (pair string (list (list string)))))
        "same definition and global key"
        [ (expected, [ [ "global"; expected ] ]) ]
        (definitions source);
      let bundle = runner source |> String.split_on_char '\n' in
      Alcotest.(check string) "same bundle key" expected (List.hd bundle))
    [
      ("a.b", "c", "a.b.c");
      ("a", "b.c", "a.b%2Ec");
      ("app", "foo", "app.foo");
      ("app", "Foo", "app.!foo");
      ("app", "FOO", "app.!f!o!o");
      ("App", "!foo", "!app.%21foo");
      ("app", "%0A", "app.%250!a");
      ("app", "Я", "app.%D0%AF");
      ("app", "я", "app.%D1%8F");
      ("Я.App", "x-1_y", "%D0%AF.!app.x-1_y");
    ];
  check "(ns app (:require [Missing.Dep :as dep])) (def x dep/Foo)" "global !missing.!dep.!foo";
  check "(def x Other.Space/Foo)" "global !other.!space.!foo";
  check "(defn f [get] (get 1))" "fn 2 get\ncall 3 4\nlocal get\nint 1";
  check "(def x (let [+ +] +))" "let + 2 3\nbuiltin +\nlocal +";
  check "(def x unknown)" "global user.unknown";
  List.iter
    (fun name -> check ("(def x " ^ name ^ ")") ("builtin " ^ name))
    [
      "+";
      "-";
      "*";
      "=";
      "not=";
      "<";
      "<=";
      ">";
      ">=";
      "not";
      "str";
      "list";
      "hash-map";
      "get";
      "count";
      "assoc";
      "conj";
      "map";
      "reduce";
      "atom";
      "deref";
      "reset!";
      "swap!";
    ]

let rejected_forms () =
  List.iter
    (fun source ->
      (match definitions source with
      | _ -> Alcotest.failf "accepted unsupported source: %s" source
      | exception Failure message ->
          Alcotest.(check bool) "backend diagnostic" true (String.starts_with ~prefix:"fex:" message));
      let status, output, error = cli [ "--target"; "fex" ] source in
      Alcotest.(check bool) "nonzero CLI exit" true (match status with Unix.WEXITED n -> n <> 0 | _ -> false);
      Alcotest.(check string) "no partial compiler output" "" output;
      Alcotest.(check bool) "stderr diagnostic" true (String.length error > 0))
    [
      "(def x (raw-code \"1\"))";
      "(def x (. obj method))";
      "(def x (.method obj))";
      "(def x (new Thing))";
      "(def x (Thing.))";
      "(def x (set! y 1))";
      "(def x (cast String y))";
      "(defn f [^String x] x)";
      "(def x (instance? String y))";
      "(def x (gen-class :name Demo :extends Object :methods []))";
      "(gen-class :name Demo :extends Object :methods [])";
      "(def x (export-default :key 1))";
      "(ns app (:import [java.lang String])) (def x 1)";
      "(ns app (:require [\"node:path\" :as path])) (def x 1)";
      "42";
      "(str 42)";
      "(def x (def y 1))";
      "(defn f [] (if true 1 (raw-code \"bad\")))";
    ];
  List.iter
    (fun form -> ignore (definitions ("(def x '" ^ form ^ ")")))
    [ "(new Thing)"; "(.method obj)"; "(cast String x)"; "(raw-code \"x\")" ];
  let source = "(def x 42)" in
  Alcotest.(check (result string string))
    "runner source-map guard" (Error "--source-map is supported only for js")
    (Language_main.Runner.run ~source_map:true ~target:"fex" source);
  let status, output, error = cli [ "--target"; "fex"; "--source-map" ] source in
  Alcotest.(check bool) "source map CLI fails" true (status = Unix.WEXITED 1);
  Alcotest.(check string) "no output" "" output;
  Alcotest.(check string) "source map diagnostic" "--source-map is supported only for js\n" error

let integer_overflow () =
  List.iter
    (fun literal ->
      match definitions ("(def x " ^ literal ^ ")") with
      | _ -> Alcotest.failf "accepted overflowing integer %s" literal
      | exception Failure message ->
          Alcotest.(check string) "int32 diagnostic" "fex: integer outside int32 range" message)
    [
      "2147483648";
      "-2147483649";
      "4611686018427387905";
      "-4611686018427387905";
      "+4611686018427387905";
      "999999999999999999999999999999999999999999";
    ];
  check "(def x 4.6116860184273879e18)" "float 4.6116860184273879e+18"

let nested_shadowing () =
  check "(def x (let [x 1] (do (let [x 2] x) x)))" "let x 2 3\nint 1\ndo 4 7\nlet x 5 6\nint 2\nlocal x\nlocal x"

let () =
  Alcotest.run "FEX"
    [
      ( "records",
        [
          Alcotest.test_case "core and closures" `Quick core;
          Alcotest.test_case "resolution" `Quick resolution;
          Alcotest.test_case "quote and collections" `Quick quote_and_collections;
          Alcotest.test_case "nested destructuring" `Quick destructuring;
          Alcotest.test_case "serialization and addresses" `Quick serialization;
          Alcotest.test_case "canonical names and aliases" `Quick canonical_names;
          Alcotest.test_case "integer overflow" `Quick integer_overflow;
          Alcotest.test_case "nested shadowing" `Quick nested_shadowing;
        ] );
      ( "CLI",
        [
          Alcotest.test_case "bundle bytes and compatibility" `Quick bundle_cli;
          Alcotest.test_case "unsupported forms" `Quick rejected_forms;
          Alcotest.test_case "unpack exact bytes and framing" `Quick unpack_bytes;
          Alcotest.test_case "unpack names and input errors" `Quick unpack_names_and_errors;
          Alcotest.test_case "unpack updates and partial failure" `Quick unpack_updates;
        ] );
    ]
