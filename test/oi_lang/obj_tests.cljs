(ns oi-lang.obj-tests
  (:require [cljs.test :as test :refer [is] :refer-macros [deftest]]
            [oi-lang.obj :as oi]))

(deftest simple-parses
  (is (= (oi/parse "42") [[:number 42]]))
  (is (= (oi/parse "\"foo\"") [[:string "foo"]]))
  (is (= (oi/parse "bar") [[:message "bar"]]))
  (is (= (oi/parse "bar()") [[:message "bar" [:arglist]]]))
  (is (= (oi/parse "bar(1)") [[:message "bar" [:arglist [:number 1]]]]))
  (is (= (oi/parse "bar(1, 2)") [[:message "bar" [:arglist [:number 1] [:number 2]]]]))
  (is (= (oi/parse "bar(1, 2, 3)") [[:message "bar" [:arglist [:number 1] [:number 2] [:number 3]]]]))
  (is (= (oi/parse "foo bar") [[:send [:message "foo"] [:message "bar"]]]))
  (is (= (oi/parse "foo < bar") [[:send [:message "foo"] [:message "<" [:arglist [:message "bar"]]]]]))
  (is (= (oi/parse "x := 42") [[:message "setSlot" [:arglist [:message "x"] [:number 42]]]])))

(defn string->result* [s] (-> s oi/parse oi/ast->runtime* oi/eval*))

(deftest simple-eval-tests
  (is (oi/oi-= (string->result* "42") (oi/oi-number 42)))
  (is (oi/oi-= (string->result* "42 < 22") oi/oi-false))
  (is (oi/oi-= (string->result* "2<1") oi/oi-false))
  (is (oi/oi-= (string->result* "1<2") oi/oi-true))
  (is (oi/oi-= (string->result* "42 > 22") oi/oi-true))
  (is (oi/oi-= (string->result* "42") (oi/oi-number 42)))
  (is (oi/oi-= (string->result* "list(1, 2, 3); 42") (oi/oi-number 42)))
  (is (oi/oi-= (string->result* "list(1, 2, 3); 2<42") oi/oi-true))
  (is (oi/oi-= (string->result* "list(1, 2, 3)")
               (oi/oi-list (oi/oi-number 1) (oi/oi-number 2) (oi/oi-number 3))))
  (is (oi/oi-= (string->result* "x := list(1, 2, 3); x")
               (oi/oi-list (oi/oi-number 1) (oi/oi-number 2) (oi/oi-number 3)))))
