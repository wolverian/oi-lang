(ns oi-lang.obj
  (:require [instaparse.core :as insta :refer-macros [defparser]]
            [cljs.core.match :refer-macros [match]]
            [cljs.test :as test :refer [is] :refer-macros [deftest]]
            [clojure.string :as string]))

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

   op-message = operator <' '*> exp

   arglist = '' | exp | arglist <' '*> <','> <' '*> exp

   <operator> = #'[<>+*=-]'

   send = exp <' '+> message
        | exp <' '*> op-message

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
    {:number     (fn [number] [:number (int number)])
     :string     (fn [& chars] [:string (apply str chars)])
     :arglist    reduce-arglist
     :op-message (fn [op exp]
                   [:message op [:arglist exp]])}
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

(defn activate [value args]
  (if (fn? value)
    (apply value args)
    (throw (str "unsupported activation: " (or value "nil") " " (or args "nil")))))

(defn oi-= [a b]
  (and (= (:type a) (:type b))
       (= (:value a) (:value b))))

(defn oi-object []
  {:slots [] :proto nil})

(def initial (oi-object))

(def oi-false {:type  :boolean
               :value false
               :slots []
               :proto initial})

(def oi-true {:type  :boolean
              :value true
              :slots []
              :proto initial})

(defn oi-boolean [truthy]
  (if truthy oi-true oi-false))

(defn oi-number [n]
  {:type  :number
   :value n
   :slots {"<" (fn [{:keys [value]}]
                 (oi-boolean (< n value)))}
   :proto initial})

(defn eval [expr]
  (match expr
    [:number n]
    (oi-number n)
    [:send target [:message name [:arglist & args]]]
    (activate (-> (eval target) :slots (get name)) (map eval args))))

(defn eval* [exprs]
  (map eval exprs))

(defn pretty [expr]
  (match expr
    {:type :number :value value} value
    {:type :boolean :value value} value))

(defn pretty* [exprs]
  (string/join "\n" (map pretty exprs)))

(deftest eval-tests
  (is (oi-= (eval* (parse "42")) [(oi-number 42)]))
  (is (oi-= (eval* (parse "42 < 22")) [oi-false]))
  (is (oi-= (eval* (parse "2<1")) [oi-false]))
  (is (oi-= (eval* (parse "1<2")) [oi-true]))
  (is (oi-= (eval* (parse "42 > 22")) [oi-true])))
