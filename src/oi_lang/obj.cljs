(ns oi-lang.obj
  (:require [instaparse.core :as insta :refer-macros [defparser]]
            [cljs.test :as test :refer [is] :refer-macros [deftest]]))

(defparser oi-program
  "start = literal
   literal = number | string

   number = digit+
   <digit> = '0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'

   string = '\"'
   ")

(defn parse [source]
  (insta/transform
    {:number (fn [& digits] [:number (int (apply str digits))])}
    (oi-program source)))

(deftest simple-parses
  (is (= (parse "42") [:start [:literal [:number 42]]])))
