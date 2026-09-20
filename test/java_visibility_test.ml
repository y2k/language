let compile source =
  match Frontend.parse_and_desugar source with
  | Ok forms -> Backend_compiler.Java.compile forms
  | Error message -> Alcotest.fail message

let write_file path text = Out_channel.with_open_text path (fun output -> output_string output text)

let with_temp_dir f =
  let dir = Filename.temp_dir "language-java-visibility-" "" in
  let rec remove path =
    if Sys.is_directory path then (
      Array.iter (fun name -> remove (Filename.concat path name)) (Sys.readdir path);
      Unix.rmdir path)
    else Sys.remove path
  in
  Fun.protect ~finally:(fun () -> remove dir) (fun () -> f dir)

let run ?private_member program args =
  let stdout, stdin, stderr =
    Unix.open_process_args_full program (Array.of_list (program :: args)) (Unix.environment ())
  in
  close_out stdin;
  let output = In_channel.input_all stdout in
  let error = In_channel.input_all stderr in
  let status = Unix.close_process_full (stdout, stdin, stderr) in
  match (private_member, status) with
  | None, Unix.WEXITED 0 -> ()
  | Some member, Unix.WEXITED 1 ->
      let words = String.split_on_char ' ' error in
      Alcotest.(check bool)
        (member ^ " private diagnostic: " ^ error)
        true
        (List.mem "private" words && List.exists (String.starts_with ~prefix:member) words)
  | _ -> Alcotest.failf "%s unexpected status:\n%s\n%s" program output error

let visibility () =
  with_temp_dir (fun dir ->
      let source name text =
        let path = Filename.concat dir (name ^ ".java") in
        write_file path (compile text);
        path
      in
      let dictionary =
        source "serbian"
          {|(ns words.dic.serbian)
(def scalar 7)
(def items [scalar "zdravo"])
(def- secret 3)
(def- secrets [secret 4])
(defn- hidden [] secrets)
(defn internal [] [(hidden) secret])
(defn words [] [["zdravo" "привет"]])
(def counter (atom 0))
(def first-value (reset! counter 1))
(def second-value (reset! counter 2))
(defn read-count [] (deref counter))
(gen-class :name Runner :extends Object :methods [[run [] void]])
(defn- -run [this] (reset! counter secret))|}
      in
      let app =
        source "app"
          {|(ns words.app (:require [words.dic.serbian :as serbian]))
(def copied serbian/scalar)
(defn dictionary [] (serbian/words))
(defn values [] [serbian/scalar serbian/items copied])
(defn internal [] (serbian/internal))|}
      in
      let runner = Filename.concat dir "VisibilityChecks.java" in
      write_file runner
        {|import static y2k.language.language_runtime.*;
public final class VisibilityChecks {
  static void check(Object expected, Object actual) {
    if (!expected.equals(actual)) throw new AssertionError("expected " + expected + ", got " + actual);
  }
  public static void main(String[] args) throws Exception {
    check(list(list("zdravo", "привет")), words.app.dictionary());
    check(list(7, list(7, "zdravo"), 7), words.app.values());
    check(list(list(3, 4), 3), words.app.internal());
    check(1, words.dic.serbian.first_value);
    check(2, words.dic.serbian.second_value);
    check(2, words.dic.serbian.read_count());
    new words.dic.serbian.Runner().run();
    check(3, words.dic.serbian.read_count());
  }
}
|};
      run "javac" [ "-d"; dir; Sys.getenv "RUNTIME_JAVA"; dictionary; app; runner ];
      run "java" [ "-cp"; dir; "VisibilityChecks" ];
      List.iter
        (fun namespace ->
          List.iter
            (fun (member, expression) ->
              let consumer =
                source "consumer"
                  (Printf.sprintf "(ns %s (:require [words.dic.serbian :as serbian])) (defn test [] %s)" namespace
                     expression)
              in
              run ~private_member:member "javac" [ "-cp"; dir; "-d"; dir; consumer ])
            [ ("hidden", "(serbian/hidden)"); ("secret", "serbian/secret"); ("secrets", "serbian/secrets") ])
        [ "words.dic.consumer"; "external.consumer" ])

let () = Alcotest.run "java visibility" [ ("java", [ Alcotest.test_case "namespace access" `Slow visibility ]) ]
