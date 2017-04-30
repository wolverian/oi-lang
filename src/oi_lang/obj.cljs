(ns oi-lang.obj
  (:require [instaparse.core :as insta :refer-macros [defparser]]
            [cljs.test :as test :refer [is] :refer-macros [deftest]]))

(defparser oi-program
  "start = (terminator | exp)*
   <exp> = literal / send / message
   <literal> = number / string

   number = #'[0-9]+'

   string = <'\"'> str-char* <'\"'>
   <str-char> = #'[^\"]'

   <identifier> = #'[a-zA-Z_]\\w*'

   message = identifier <' '*> <'('> <' '*> arglist <' '*> <')'>
           | identifier
   arglist = '' | exp | arglist <' '*> <','> <' '*> exp

   send = exp <' '+> message

   terminator = '\n' | ';'
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
     }
    (oi-program source)))

(deftest simple-parses
  (is (= (parse "42") [:start [:number 42]]))
  (is (= (parse "\"foo\"") [:start [:string "foo"]]))
  (is (= (parse "foo") [:start [:message "foo"]]))
  (is (= (parse "bar()") [:start [:message "bar" [:arglist]]]))
  (is (= (parse "bar(1)") [:start [:message "bar" [:arglist [:number 1]]]]))
  (is (= (parse "bar(1, 2)") [:start [:message "bar" [:arglist [:number 1] [:number 2]]]]))
  (is (= (parse "bar(1, 2, 3)") [:start [:message "bar" [:arglist [:number 1] [:number 2] [:number 3]]]]))
  (is (= (parse "foo bar") [:start [:send [:message "foo"] [:message "bar"]]])))
