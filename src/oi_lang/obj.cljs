(ns oi-lang.obj
  (:require [instaparse.core :as insta :refer-macros [defparser]]
            [cljs.core.match :refer-macros [match]]
            [cljs.test :as test :refer [is] :refer-macros [deftest]]))

(defparser oi-program
  "<start> = (terminator | exp)*
   <exp> = literal / send / message
   <literal> = number / string

   number = #'[0-9]+'

   string = <'\"'> str-char* <'\"'>
   <str-char> = #'[^\"]'

   <identifier> = #'[a-zA-Z_]\\w*'

   message = identifier <' '*> <'('> <' '*> arglist <' '*> <')'>
           | identifier
           | op-message

   op-message = operator <' '*> exp

   arglist = '' | exp | arglist <' '*> <','> <' '*> exp

   <operator> = #'[<>+*=-]'

   send = exp <' '+> message

   <terminator> = <'\n'> | <';'>
   ")

(defn reduce-arglist [& params]
  (let [flatten-arglist
        (fn [[type & rest]]
          (if (= type :arglist)
            rest
            [(vec (cons type rest))]))]
    (vec (cons :arglist (mapcat flatten-arglist params)))))

(defn parse [source]
  (insta/transform
    {:number  (fn [number] [:number (int number)])
     :string  (fn [& chars] [:string (apply str chars)])
     :arglist reduce-arglist
     :message (fn [& args]
                (if (and (= 1 (count args)) (= (first (first args)) :op-message))
                  (let [[_ op & args] (first args)]
                    [:message op (vec (cons :arglist args))])
                  (vec (cons :message args))))}
    (oi-program source)))

(deftest simple-parses
  (is (= (parse "42") [[:number 42]]))
  (is (= (parse "\"foo\"") [[:string "foo"]]))
  (is (= (parse "bar") [[:message "bar"]]))
  (is (= (parse "bar()") [[:message "bar" [:arglist]]]))
  (is (= (parse "bar(1)") [[:message "bar" [:arglist [:number 1]]]]))
  (is (= (parse "bar(1, 2)") [[:message "bar" [:arglist [:number 1] [:number 2]]]]))
  (is (= (parse "bar(1, 2, 3)") [[:message "bar" [:arglist [:number 1] [:number 2] [:number 3]]]]))
  (is (= (parse "foo bar") [[:send [:message "foo"] [:message "bar"]]]))
  (is (= (parse "foo < bar") [[:send [:message "foo"] [:message "<" [:arglist [:message "bar"]]]]])))

(defn eval [expr]
  expr)

(defn eval* [exprs]
  (map eval exprs))

(def oi-false [:false])

(deftest eval-tests
  (is (= (eval "42") "42"))
  (is (= (eval "42 < 22") oi-false)))
