module Eval = Backend_eval.Eval
module Stdlib_eval = Backend_eval.Eval_stdlib
open Backend_eval.Eval_types

let eval source =
  match Frontend.parse_and_desugar source with
  | Error message -> Alcotest.fail message
  | Ok forms -> List.hd (List.rev (Eval.eval_all forms))

(* Only compare expected scalar results; never structurally compare a compiled matcher. *)
let check source expected =
  let actual = eval source in
  Alcotest.(check bool) (source ^ " => " ^ Stdlib_eval.to_string actual) true (Stdlib_eval.equal_value expected actual)

let contains text part =
  let rec loop i =
    i + String.length part <= String.length text && (String.sub text i (String.length part) = part || loop (i + 1))
  in
  loop 0

let quoted text =
  "\""
  ^ (text |> String.to_seq |> List.of_seq
    |> List.map (function
      | '"' -> "\\\""
      | '\\' -> "\\\\"
      | '\n' -> "\\n"
      | '\r' -> "\\r"
      | '\t' -> "\\t"
      | c -> String.make 1 c)
    |> String.concat "")
  ^ "\""

let error operation reason source =
  match eval source with
  | exception Eval_error message ->
      Alcotest.(check bool) message true (contains message operation && contains message reason)
  | _ -> Alcotest.failf "expected Eval_error: %s" source

let runtime () =
  (match eval {|(re-pattern "x")|} with Regex _ -> () | _ -> Alcotest.fail "expected compiled regex");
  List.iter
    (fun (source, expected) -> check source expected)
    [
      ({|(str (re-pattern "x"))|}, String "#<regex>");
      ({|(str [(re-pattern "x")])|}, String "(#<regex>)");
      ({|(str {:r (re-pattern "x")})|}, String "{r #<regex>}");
      ({|(if (re-pattern "") true false)|}, Bool true);
      ({|(not (re-pattern "x"))|}, Bool false);
      ({|(assert (re-pattern "x"))|}, Bool true);
      ({|(vector? (re-pattern "x"))|}, Bool false);
      ({|(let [r (re-pattern "x")] (= r r))|}, Bool false);
      ({|(let [r (re-pattern "x")] (not= r r))|}, Bool true);
      ({|(let [r (re-pattern "x")] (= [r] [r]))|}, Bool false);
      ({|(let [r (re-pattern "x")] (= {:r r} {:r r}))|}, Bool false);
      ({|(= (re-pattern "x") "x")|}, Bool false);
      ({|(= (re-pattern "x") (re-pattern "x"))|}, Bool false);
      ({|(= (re-pattern "x"))|}, Bool true);
      ({|(not= (re-pattern "x"))|}, Bool false);
      ({|(let [r (re-pattern "x")] (get (hash-map r 1) r))|}, Nil);
      ({|(let [r (re-pattern "x")] (get-in (hash-map [r] 1) [[r]]))|}, Nil);
    ];
  error "regex" "not a function" {|((re-pattern "x"))|};
  List.iter
    (fun (operation, source) -> error operation "function" source)
    [
      ("map", {|(map (re-pattern "x") [])|});
      ("reduce", {|(reduce (re-pattern "x") [1])|});
      ("swap!", {|(swap! (atom 1) (re-pattern "x"))|});
    ];
  error "count" "expects" {|(count (re-pattern "x"))|};
  error "get" "expects" {|(get (re-pattern "x") 0)|}

let search_cases =
  [
    ("foo([0-9]+)", "xfoo42 foo7", String "foo42");
    ("x", "abc", Nil);
    ("", "abc", String "");
    ("", "", String "");
    ("a|ab", "ab", String "a");
    ("a+", "aaa", String "aaa");
    ("a+?", "aaa", String "a");
    ("foo", "FOO", Nil);
    ("^foo", "x\nfoo", Nil);
    ("foo$", "foo", String "foo");
    ("foo$", "foo\n", Nil);
    ("a.b", "a\nb", Nil);
    ("a[\\s\\S]b", "a\nb", String "a\nb");
    ("\\d+", "a42", String "42");
    ("\\.", "a.b", String ".");
    ("..", "é", String "é");
  ]

let find_source pattern text = Printf.sprintf "(re-find (re-pattern %s) %s)" (quoted pattern) (quoted text)

let search_results () =
  List.iter (fun (pattern, text, expected) -> check (find_source pattern text) expected) search_cases;
  (* Dot consumes one UTF-8 byte, not one codepoint. *)
  check (find_source "." "é") (String "\195");
  check {|(let [x (re-find (re-pattern "x") "abc")] (if x false (= x nil)))|} (Bool true);
  check {|(let [x (re-find (re-pattern "") "abc")] (if x (= x "") false))|} (Bool true);
  check {|(= (re-find (re-pattern "x") "abc") "nil")|} (Bool false)

let replacement_cases =
  [
    ("foo1 foo22", "foo[0-9]+", "X", "X X");
    ("abc", "x", "X", "abc");
    ("export function f() {}", "^export ", "", "function f() {}");
    ("aa", "(a)", "$1\\1$&", "$1\\1$&$1\\1$&");
    ("aa", "a", "aa", "aaaa");
    ("aaa", "aa", "X", "Xa");
    ("ab", "", "-", "-a-b-");
    ("", "", "-", "-");
    ("ab", "a*", "-", "-b-");
    ("a", "a*", "-", "-");
    ("ab", "^", "-", "-ab");
    ("ab", "$", "-", "ab-");
    ("", "x", "-", "");
    ("ab", "", "", "ab");
  ]

let replace_source text pattern replacement =
  Printf.sprintf "(re-replace %s (re-pattern %s) %s)" (quoted text) (quoted pattern) (quoted replacement)

let replacements () =
  List.iter
    (fun (text, pattern, replacement, expected) -> check (replace_source text pattern replacement) (String expected))
    replacement_cases

let nonword_character_class () =
  (* Re 1.14.0 regressed bracketed \W to \w; test both inclusion and exclusion. *)
  List.iter
    (fun pattern ->
      check (find_source pattern "!!!") (String "!!!");
      check (find_source pattern "abc_42") Nil;
      check (replace_source "abc_42!!!" pattern "-") (String "abc_42-"))
    [ "[\\W]+"; "\\W+"; "[^\\w]+" ];
  check (find_source "[^\\W]+" "!!!abc_42") (String "abc_42");
  check (find_source "[\\W_]+" "a_!?b") (String "_!?")

let invalid_patterns = [ "["; "(?m)^foo"; "(?s)."; "(?=foo)"; "(?<=foo)x"; "(x)\\1" ]

let pattern_errors () =
  List.iter
    (fun pattern ->
      match eval ("(re-pattern " ^ quoted pattern ^ ")") with
      | exception Eval_error message ->
          Alcotest.(check bool)
            message true
            (contains message "re-pattern" && (contains message "invalid" || contains message "unsupported"))
      | _ -> Alcotest.fail "expected pattern error")
    invalid_patterns

let passage () =
  let sources =
    [
      "r";
      "((fn [x] x) r)";
      "(get [r] 0)";
      "(get-in {:r [r]} [:r 0])";
      "(let [[x] [r]] x)";
      "(let [{:r x} {:r r}] x)";
      "((fn [[x]] x) [r])";
      "((fn [{:r x}] x) {:r r})";
      "(deref (atom r))";
      "(reset! (atom nil) r)";
      "(swap! (atom r) (fn [x] x))";
      "(cast ignored r)";
    ]
  in
  List.iter (fun source -> check ("(def r (re-pattern \"x\")) (re-find " ^ source ^ " \"x\")") (String "x")) sources;
  check {|(let [[x] []] (= x nil))|} (Bool true);
  check {|(let [{:r x} {}] (= x nil))|} (Bool true);
  check {|(str '(re-pattern "x"))|} (String "(re-pattern x)");
  check
    {|(let [r (re-pattern "x")]
              (str (re-find r "x") "|" (re-find r "abc") "|"
                   (re-replace "xx" r "z") "|" (re-find r "xx")))|}
    (String "x|nil|zz|x")

let run source = Language_main.Runner.run ~target:"eval" source

let process program args input =
  let channels = Unix.open_process_args_full program (Array.of_list (program :: args)) (Unix.environment ()) in
  let stdout, stdin, stderr = channels in
  output_string stdin input;
  close_out stdin;
  let output = In_channel.input_all stdout in
  let error = In_channel.input_all stderr in
  let status = Unix.close_process_full channels in
  (status, output, error)

let runner_cli () =
  List.iter
    (fun (source, expected) ->
      Alcotest.(check (result string string)) source (Ok expected) (run source);
      let status, output, error = process "../bin/main.exe" [ "--target"; "eval" ] source in
      Alcotest.(check bool) "successful exit" true (status = Unix.WEXITED 0);
      Alcotest.(check string) "stdout" (expected ^ "\n") output;
      Alcotest.(check string) "stderr" "" error)
    [
      ({|(re-pattern "x")|}, "");
      ({|(str (re-pattern "x"))|}, "#<regex>");
      (find_source "x" "abc", "nil");
      (find_source "" "abc", "");
      (find_source "x" "x", "x");
      (replace_source "xx" "x" "z", "zz");
    ];
  let errors =
    [ "(re-pattern)"; {|(re-find "x" "")|}; {|(re-replace "" (re-pattern "x") nil)|} ]
    @ List.map (Printf.sprintf "(re-pattern %S)") invalid_patterns
  in
  List.iter
    (fun source ->
      let message = match run source with Error message -> message | Ok _ -> Alcotest.fail "expected runner error" in
      Alcotest.(check bool) "language error" true (contains message "re-");
      let status, output, error = process "../bin/main.exe" [ "--target"; "eval" ] source in
      Alcotest.(check bool) "error exit" true (status = Unix.WEXITED 1);
      Alcotest.(check string) "no success output" "" output;
      Alcotest.(check string) "language diagnostic" (message ^ "\n") error)
    errors

let read_file path = In_channel.with_open_text path In_channel.input_all

let with_file suffix contents f =
  let path = Filename.temp_file "language-regex-" suffix in
  Fun.protect
    ~finally:(fun () -> Sys.remove path)
    (fun () ->
      Out_channel.with_open_text path (fun channel -> output_string channel contents);
      f path)

let userscript () =
  let compiled =
    match Language_main.Runner.run ~target:"js" {|(defn answer [] (+ 40 2)) (println (answer))|} with
    | Ok source -> source
    | Error message -> Alcotest.fail message
  in
  let runtime = read_file (Sys.getenv "RUNTIME_JS") in
  let header = "// ==UserScript==\n// @name regex-test\n// ==/UserScript==" in
  let builder = read_file (Filename.concat (Sys.getenv "SAMPLES_DIR") "eval/regex_userscript_build.clj") in
  with_file ".txt"
    ("discard before\n" ^ header ^ "\ndiscard after")
    (fun header_path ->
      with_file ".js" runtime (fun runtime_path ->
          with_file ".js" compiled (fun program_path ->
              let source =
                builder
                ^ Printf.sprintf "\n(assemble %s %s %s)" (quoted header_path) (quoted runtime_path)
                    (quoted program_path)
              in
              let script = match run source with Ok script -> script | Error message -> Alcotest.fail message in
              Alcotest.(check bool) "extracted header first" true (String.starts_with ~prefix:(header ^ "\n") script);
              List.iter
                (fun unwanted -> Alcotest.(check bool) ("removed " ^ unwanted) false (contains script unwanted))
                [ "discard before"; "discard after"; "import "; "export " ];
              let status, output, error = process "../bin/main.exe" [ "--target"; "eval" ] source in
              Alcotest.(check bool) "CLI success" true (status = Unix.WEXITED 0);
              Alcotest.(check string) "stdout assembly" (script ^ "\n") output;
              Alcotest.(check string) "no diagnostic" "" error;
              (* CommonJS parsing rejects leftover module directives; running checks real runtime linkage. *)
              let status, output, error = process "node" [ "--input-type=commonjs" ] output in
              Alcotest.(check string) "Node stderr" "" error;
              Alcotest.(check bool) "Node success" true (status = Unix.WEXITED 0);
              Alcotest.(check string) "assembled program result" "42\n" output)))

let () =
  Alcotest.run "eval regex"
    [
      ( "regex",
        List.map
          (fun (name, test) -> Alcotest.test_case name `Quick test)
          [
            ("opaque runtime", runtime);
            ("search and dialect", search_results);
            ("literal global replacement", replacements);
            ("non-word classes exclude word characters", nonword_character_class);
            ("invalid patterns", pattern_errors);
            ("value passage and reuse", passage);
            ("runner and CLI", runner_cli);
            ("assemble userscript and run in Node", userscript);
          ] );
    ]
