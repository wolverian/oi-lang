(ns oi-lang.obj-tests
  (:require [cljs.test :as test :refer [is] :refer-macros [deftest]]))

(deftest simple-parses
  (is (= (parse "42") [[:number 42]]))
  (is (= (parse "\"foo\"") [[:string "foo"]]))
  (is (= (parse "bar") [[:message "bar"]]))
  (is (= (parse "bar()") [[:message "bar" [:arglist]]]))
  (is (= (parse "bar(1)") [[:message "bar" [:arglist [:number 1]]]]))
  (is (= (parse "bar(1, 2)") [[:message "bar" [:arglist [:number 1] [:number 2]]]]))
  (is (= (parse "bar(1, 2, 3)") [[:message "bar" [:arglist [:number 1] [:number 2] [:number 3]]]]))
  (is (= (parse "foo bar") [[:send [:message "foo"] [:message "bar"]]]))
  (is (= (parse "foo < bar") [[:send [:message "foo"] [:message "<" [:arglist [:message "bar"]]]]]))
  (is (= (parse "x := 42") [[:message "setSlot" [:arglist [:message "x"] [:number 42]]]])))

(deftest simple-ast->runtime*-tests
  (is (oi-= (ast->runtime* (parse "42")) [(oi-number 42)]))
  (is (oi-= (ast->runtime* (parse "42 < 22")) [oi-false]))
  (is (oi-= (ast->runtime* (parse "2<1")) [oi-false]))
  (is (oi-= (ast->runtime* (parse "1<2")) [oi-true]))
  (is (oi-= (ast->runtime* (parse "42 > 22")) [oi-true])))

(deftest list-ast->runtime*-tests
  (is (oi-= (ast->runtime* (parse "list(1, 2, 3)")) [(oi-list (oi-number 1) (oi-number 2) (oi-number 3))]))
  (is (oi-= (ast->runtime* (parse "x := list(1, 2, 3); x")) [(oi-list (oi-number 1) (oi-number 2) (oi-number 3))])))

(deftest simple-eval-tests
  (is (oi-= (eval* (ast->runtime* (parse "42"))) [(oi-number 42)])))
