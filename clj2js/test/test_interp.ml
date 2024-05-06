let assert_ = Utils.assert_ Lib.main_interpreter "interpreter/src/prelude.clj"

let main () =
  [
    ( "Interpreter",
      [
        assert_ __POS__ {|true|} {|true|};
        assert_ __POS__ {|false|} {|false|};
        assert_ __POS__ {|1|} {|1|};
        assert_ __POS__ {|"a"|} {|"a"|};
        assert_ __POS__ {|:a|} {|:a|};
        assert_ __POS__ {|'a|} {|a|};
        assert_ __POS__ {|(- 11 7)|} {|4|};
        assert_ __POS__ {|(+ 11 7)|} {|18|};
        assert_ __POS__ {|(* 11 7)|} {|77|};
        assert_ __POS__ {|(/ 11 7)|} {|1|};
        assert_ __POS__ {|(list 1 (+ 1 1) 3)|} {|(1 2 3)|};
        assert_ __POS__ {|(vector 1 (+ 1 1) 3)|} {|[1 2 3]|};
        assert_ __POS__ {|[1 (+ 1 1) 3]|} {|[1 2 3]|};
        assert_ __POS__ {|(concat (list 1 2) (list 3 4))|} {|(1 2 3 4)|};
        assert_ __POS__ {|(quote a)|} {|'a|};
        assert_ __POS__ {|(defn f [x] (+ x x))(f 1)|} {|2|};
        assert_ __POS__ {|(= 1 1)|} {|true|};
        assert_ __POS__ {|(= 1 2)|} {|false|};
        assert_ __POS__ {|(if true 1 2)|} {|1|};
        assert_ __POS__ {|(if false 1 2)|} {|2|};
        assert_ __POS__ {|(not true)|} {|false|};
        assert_ __POS__ {|(not false)|} {|true|};
        assert_ __POS__ {|(not= 1 1)|} {|false|};
        assert_ __POS__ {|(not= 1 2)|} {|true|};
        assert_ __POS__ {|(str "1" :2 3)|} {|"1:23"|};
        assert_ __POS__ {|(let [x (+ 1 2) y (+ 1 x)] (+ 1 y))|} {|5|};
        assert_ __POS__
          {|(defn f [a] (let [x (+ a 2) y (+ 1 x)] (+ 1 y)))(f 1)|} {|5|};
        assert_ __POS__ {|(get {:a 1 :b 2 :c 3} :b)|} {|2|};
        assert_ __POS__ {|(get [10 20 30] 1)|} {|20|};
      ] );
  ]
