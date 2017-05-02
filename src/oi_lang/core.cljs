(ns oi-lang.core
  (:require [oi-lang.obj :as obj]))

(enable-console-print!)

(defn run [evt]
  (let [code-input                      (.getElementById js/document "code" .-value)
        parse-tree                      (obj/parse code-input)
        parse-tree-output               (.getElementById js/document "parse-tree")
        runtime-representation-output   (.getElementById js/document "runtime-representation")
        evaluated-representation-output (.getElementById js/document "evaluated")
        pretty-representation-output    (.getElementById js/document "pretty-representation")]
    (if (seq? parse-tree)
      (set! (.-value parse-tree-output) parse-tree)
      (do
        (set! (.-value parse-tree-output) (str "Error: " (.stringify js/JSON (clj->js parse-tree))))
        (set! (.-value runtime-representation-output) "")
        (set! (.-value evaluated-representation-output) "")
        (set! (.-value pretty-representation-output) "")))
    (let [runtime-representation   (obj/ast->runtime* parse-tree)
          _                        (set! (.-value runtime-representation-output) runtime-representation)
          evaluated-representation (obj/eval* runtime-representation)
          _                        (set! (.-value evaluated-representation-output) evaluated-representation)
          pretty-representation    (obj/pretty evaluated-representation)
          _                        (set! (.-value pretty-representation-output) pretty-representation)])))

(defn main []
  (-> js/document
      (.getElementById "code")
      (.addEventListener "keyup" run)))

