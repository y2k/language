let assert_ = Utils.assert_ Lib.main_interpreter "interpreter/src/prelude.clj"

(*  *)
let main () =
  [
    ( "Interpreter",
      [
        assert_ __POS__ {|1|} {|1|};
        assert_ __POS__ {|"a"|} {|"a"|};
        assert_ __POS__ {|:a|} {|:a|};
        assert_ __POS__ {|(- 11 7)|} {|4|};
        assert_ __POS__ {|(+ 11 7)|} {|18|};
        assert_ __POS__ {|(* 11 7)|} {|77|};
        assert_ __POS__ {|(/ 11 7)|} {|1|};
        assert_ __POS__ {|(list 1 (+ 1 1) 3)|} {|(1 2 3)|};
        assert_ __POS__ {|(vector 1 (+ 1 1) 3)|} {|[1 2 3]|};
        assert_ __POS__ {|[1 (+ 1 1) 3]|} {|[1 2 3]|};
        assert_ __POS__ {|(concat (list 1 2) (list 3 4))|} {|(1 2 3 4)|};
        assert_ __POS__ {|(quote a)|} {|'a|};
        assert_ __POS__ {|'a|} {|'a|};
        (* assert_ __POS__ {|(defn f [x] x)|} {||}; *)
      ] );
  ]
