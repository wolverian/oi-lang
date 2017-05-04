(ns oi-lang.obj
  (:require [instaparse.core :as insta :refer-macros [defparser]]
            [cljs.core.match :refer-macros [match]]
            [clojure.string :as string]))

(defparser oi-program
  "<start> = (terminator | exp)*
   <exp> = literal / send / message
   <literal> = number / string / boolean

   number = #'[0-9]+'

   string = <'\"'> str-char* <'\"'>
   <str-char> = #'[^\"]'

   boolean = 'true' | 'false'

   <identifier> = #'[a-zA-Z_]\\w*'

   message = identifier <' '*> <'('> <' '*> arglist <' '*> <')'>
           | identifier

   op-message = operator <' '*> exp

   arglist = '' | exp | arglist <' '*> <','> <' '*> exp

   <operator> = #'[<>+*=-]' | ':='

   send = exp <' '+> message
        | exp <' '*> op-message

   <terminator> = <' '*> <'\n'> <' '*> | <' '*> <';'> <' '*>
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
     :send       (fn [target message]
                   (match [target message]
                     [target [:message ":=" [:arglist & args]]]
                     [:message "setSlot" (vec (cons :arglist (vec (cons target args))))]
                     [_ _] [:send target message]))}
    (oi-program source)))

(defn activate [env receiver slot args]
  (if (fn? slot)
    {:env env :result (apply slot receiver args)}
    (throw (str "unsupported activation: " (or slot "nil") " " (or args "nil")))))

(defn lookup-slot [obj slot-name]
  (when (nil? obj)
    (throw (str "slot not found: " slot-name)))
  (if (contains? (:slots obj) slot-name)
    (get (:slots obj) slot-name)
    (lookup-slot (:proto obj) slot-name)))

(declare oi-boolean)

(defn oi-object []
  {:slots {"="       (fn [self other]
                       (println "=type" (:type self) (:type other))
                       (println "=value" (:value self) (:value other))
                       (and (= (:type self) (:type other))
                            (= (:value self) (:value other))))
           "setSlot" (fn [value] (throw (js/Error. "NEEDS MUTABLE STUFF")))}
   :proto nil})

(def initial (oi-object))

(declare eval do-activations)

(defn oi-list [& items]
  {:type  :list
   :value items
   :slots {"=" (fn [self other-list]
                 (println "list=" items other-list)
                 (and (= (:type other-list) :list)
                      (every? (fn ([[a b]]
                                   (let [eq (lookup-slot a "=")]
                                     (eq b))))
                              (map vector items (:value other-list)))))}
   :proto initial})

(def lobby {:type  :object
            :slots {"list" (fn [self & items]
                             (apply oi-list (map (fn [item] (:result (do-activations lobby item))) items)))}
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
   :slots {"<" (fn [self param]
                 (oi-boolean (< n (:value param))))}
   :proto initial})

(defn oi-string [s]
  {:type  :string
   :value s
   :slots {}
   :proto initial})

(defn oi-message
  ([name]
   {:type  :message
    :value name
    :slots {}
    :proto initial})
  ([name args]
   {:type  :message
    :value name
    :slots {:args (vec args)}
    :proto initial}))

(defn oi-send [target msg]
  {:type  :send
   :slots {:target  target
           :message msg}
   :proto initial})

(defn ast->runtime [ast]
  (match ast
    [:number n] (oi-number n)
    [:string s] (oi-string s)
    [:boolean b] (oi-boolean (= b "true"))
    [:send target msg] (oi-send (ast->runtime target) (ast->runtime msg))
    [:message name] (oi-message name)
    [:message name [:arglist & args]] (oi-message name (map ast->runtime args))))

(defn ast->runtime* [asts]
  (vec (map ast->runtime asts)))

(defn do-activations [env expr]
  (match expr
    {:type :send :slots {:target target :message {:type :message :value slot-name :slots {:args args}}}}
    (activate env target (lookup-slot target slot-name) args)
    {:type :send :slots {:target target :message {:type :message :value slot-name}}}
    (activate env target (lookup-slot target slot-name) [])
    {:type :message :value slot-name :slots {:args args}}
    (activate env env (lookup-slot env slot-name) args)
    {:type :message :value slot-name}
    (activate env env (lookup-slot env slot-name) [])
    _ {:env env :result expr}))

; TODO: I'm pretty sure we want to not call activate here, and only in do-activations
(defn eval [{env :env} expr]
  (match expr
    {:type :number :value n} {:env env :result expr}
    {:type :string :value s} {:env env :result (str \" s \")}
    {:type :boolean :value b} {:env env :result expr}
    {:type :send :slots {:target target :message {:type :message :value slot-name :slots {:args args}}}}
    (activate env target (lookup-slot target slot-name) args)
    {:type :send :slots {:target target :message {:type :message :value slot-name}}}
    (activate env target (lookup-slot target slot-name) [])
    {:type :message :value slot-name :slots {:args args}}
    (activate env env (lookup-slot env slot-name) args)
    {:type :message :value slot-name}
    (activate env env (lookup-slot env slot-name) [])))

(defn eval*
  ([env exprs] (:result (reduce eval {:env env :result nil} exprs)))
  ([exprs] (eval* lobby exprs)))

(defn pretty [expr]
  (match expr
    nil ""
    {:type :number :value value} value
    {:type :string :value value} (str \" value \")
    {:type :boolean :value value} value
    {:type :list :value values} (str "list(" (string/join ", " (map pretty values)) ")")
    {:type :message :value name :slots {:args args}} (str "message(" name ", " (string/join ", " (map pretty args)) ")")
    {:type :message :value name :slots {}} (str "message(" name ")")))

(defn pretty* [exprs]
  (string/join "\n" (map pretty exprs)))
