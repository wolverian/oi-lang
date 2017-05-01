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

   <operator> = #'[<>+*=-]' | ':='

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
                   [:message op [:arglist exp]])
     :send (fn [target message]
             (match [target message]
               [target [:message ":=" [:arglist & args]]]
               [:message "setSlot" (vec (cons :arglist (vec (cons target args))))]
               [_ _] [:send target message]))}
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
  (is (= (parse "foo < bar") [[:send [:message "foo"] [:message "<" [:arglist [:message "bar"]]]]]))
  (is (= (parse "x := 42") [[:message "setSlot" [:arglist [:message "x"] [:number 42]]]])))

(defn activate [value args]
  (if (fn? value)
    (apply value args)
    (throw (str "unsupported activation: " (or value "nil") " " (or args "nil")))))

(defn oi-= [a b]
  (and (= (:type a) (:type b))
       (= (:value a) (:value b))))

(declare oi-boolean)

(defn oi-object []
  (let [self {:slots {} :proto nil}]
    (assoc self
      :slots {"=" (fn [other]
                    (and (= (:type self) (:type other))
                         (= (:value self) (:value other))))
              "setSlot" (fn [value] (throw "NEEDS MUTABLE STUFF"))})))

(def initial (oi-object))

(defn oi-list [& items]
  {:type  :list
   :value items
   :slots {}
   :proto initial})

(def lobby {:type :object
            :slots {"list" oi-list}
            :proto initial})

(def oi-false {:type  :boolean
               :value false
               :slots {}
               :proto initial})

(def oi-true {:type  :boolean
              :value true
              :slots {}
              :proto initial})

(defn oi-boolean [truthy]
  (if truthy oi-true oi-false))

(defn oi-number [n]
  {:type  :number
   :value n
   :slots {"<" (fn [{:keys [value]}]
                 (oi-boolean (< n value)))}
   :proto initial})

(defn oi-string [s]
  {:type  :string
   :value s
   :slots {}
   :proto initial})

(defn oi-message
  ([name]
   {:type :message
    :value name
    :slots {}
    :proto initial})
  ([name args]
   {:type :message
    :value name
    :slots {:args args}
    :proto initial}))

(defn oi-send [target msg]
  {:type :send
   :slots {:target target
           :message msg}
   :proto initial})

(defn ast->runtime [ast]
  (match ast
    [:number n]
    (oi-number n)
    [:string s]
    (oi-string s)
    [:send target msg]
    (oi-send target (ast->runtime msg))
    [:message name]
    (oi-message name)
    [:message name [:arglist & args]]
    (oi-message name (map ast->runtime args))))

(defn ast->runtime* [asts]
  (map ast->runtime asts))

(defn eval [env expr]
  (match expr
    {:type :number :value n} {:env env :result n}))

(defn eval*
  ([env exprs] (:result (reduce eval {:env env :result nil} exprs)))
  ([exprs] (eval* lobby exprs)))

(defn pretty [expr]
  (match expr
    {:type :number :value value} value
    {:type :string :value value} (str \" value \")
    {:type :boolean :value value} value
    {:type :list :value values} (str "list(" (string/join ", " (map pretty values)) ")")
    {:type :message :value name :slots {:args args}} (str "message(" name ", " (string/join ", " (map pretty args)) ")")
    {:type :message :value name :slots {}} (str "message(" name ")")))

(defn pretty* [exprs]
  (string/join "\n" (map pretty exprs)))

(deftest simple-eval-tests
  (is (oi-= (ast->runtime* (parse "42")) [(oi-number 42)]))
  (is (oi-= (ast->runtime* (parse "42 < 22")) [oi-false]))
  (is (oi-= (ast->runtime* (parse "2<1")) [oi-false]))
  (is (oi-= (ast->runtime* (parse "1<2")) [oi-true]))
  (is (oi-= (ast->runtime* (parse "42 > 22")) [oi-true])))

(deftest list-eval-tests
  (is (oi-= (ast->runtime* (parse "list(1, 2, 3)")) [(oi-list (oi-number 1) (oi-number 2) (oi-number 3))]))
  (is (oi-= (ast->runtime* (parse "x := list(1, 2, 3); x")) [(oi-list (oi-number 1) (oi-number 2) (oi-number 3))])))
