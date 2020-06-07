module L = Language.Library.Language
open Language.Library.Language

let text_examples =
  [
    ( L.func_rec,
      "(java.lang.Integer. 0)",
      EConstructor ("java.lang.Integer", [ EInt 0 ]) );
    (L.read_field, "env.context", EReadField ("env", "context"));
    ( L.module_exp,
      {|(module App

  (defn show [env text]
    (.show 
      (android.widget.Toast/makeText env.context text 0)))

  (defn main [env]
    (show env "Hello")))|},
      EModule
        ( "App",
          [
            EDefn
              ( "show",
                [ "env"; "text" ],
                [
                  EInstanceCall
                    ( "show",
                      EStaticCall
                        ( "android.widget.Toast",
                          "makeText",
                          [
                            EReadField ("env", "context"); EConst "text"; EInt 0;
                          ] ),
                      [] );
                ] );
            EDefn
              ( "main",
                [ "env" ],
                [ ECall (("", "show"), [ EConst "env"; EString "Hello" ]) ] );
          ] ) );
    ( L.defn,
      {|(defn main [env] (.show (android.widget.Toast/makeText env.context "Hello World" 0)))|},
      EDefn
        ( "main",
          [ "env" ],
          [
            EInstanceCall
              ( "show",
                EStaticCall
                  ( "android.widget.Toast",
                    "makeText",
                    [
                      EReadField ("env", "context");
                      EString "Hello World";
                      EInt 0;
                    ] ),
                [] );
          ] ) );
    ( L.func_rec,
      "(foo model.username)",
      ECall (("", "foo"), [ EReadField ("model", "username") ]) );
    (L.func_rec, "(bar/foo)", EStaticCall ("bar", "foo", []));
    ( L.func_rec,
      "(name.space/foo a b)",
      EStaticCall ("name.space", "foo", [ EConst "a"; EConst "b" ]) );
    (L.func_rec, "(foo \"a\")", ECall (("", "foo"), [ EString "a" ]));
    ( L.func_rec,
      "(foo \"a\" b)",
      ECall (("", "foo"), [ EString "a"; EConst "b" ]) );
    (L.defn, "(defn foo [] 42)", EDefn ("foo", [], [ EInt 42 ]));
    ( L.defn,
      "(defn foo [a b c] \"bar\")",
      EDefn ("foo", [ "a"; "b"; "c" ], [ EString "bar" ]) );
    ( L.defn,
      "(defn foo [a b c] 42 )",
      EDefn ("foo", [ "a"; "b"; "c" ], [ EInt 42 ]) );
    ( L.defn,
      "(defn foo [a b c] (sum a 2) (sum b 3))",
      EDefn
        ( "foo",
          [ "a"; "b"; "c" ],
          [
            ECall (("", "sum"), [ EConst "a"; EInt 2 ]);
            ECall (("", "sum"), [ EConst "b"; EInt 3 ]);
          ] ) );
    ( L.defn,
      "(defn foo [a b c]\n  (sum a 2))",
      EDefn
        ( "foo",
          [ "a"; "b"; "c" ],
          [ ECall (("", "sum"), [ EConst "a"; EInt 2 ]) ] ) );
    (L.func_rec, "(foo)", ECall (("", "foo"), []));
    (L.func_rec, "(foo  \n\n  a  \n\n  )", ECall (("", "foo"), [ EConst "a" ]));
    (L.func_rec, "(foo a)", ECall (("", "foo"), [ EConst "a" ]));
    (L.func_rec, "(foo a b)", ECall (("", "foo"), [ EConst "a"; EConst "b" ]));
    (L.func_rec, "(sum 1 2)", ECall (("", "sum"), [ EInt 1; EInt 2 ]));
    (L.func_rec, "(foo (bar))", ECall (("", "foo"), [ ECall (("", "bar"), []) ]));
    ( L.defn,
      {|(defn view [model]
           (ui/column
             (ui/text model.username)
             (ui/button "ok")))|},
      EDefn
        ( "view",
          [ "model" ],
          [
            EStaticCall
              ( "ui",
                "column",
                [
                  EStaticCall
                    ("ui", "text", [ EReadField ("model", "username") ]);
                  EStaticCall ("ui", "button", [ EString "ok" ]);
                ] );
          ] ) );
  ]

let () =
  text_examples
  |> List.map (fun (parser, code, expected) ->
         let test_name =
           code |> String.map (function '\n' -> '_' | x -> x) |> fun x ->
           String.sub x 0 (min 30 (String.length x))
         in
         Alcotest.test_case test_name `Quick (fun _ ->
             let actual = L.eval parser code |> Result.get_ok in
             if expected = actual then ()
             else failwith ("Asserting failed with:\n\n" ^ L.show_exp actual)))
  |> fun xs -> Alcotest.run "compilation examples" [ ("test", xs) ]
