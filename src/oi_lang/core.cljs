(ns oi-lang.core
  (:require [oi-lang.obj :as obj]))

(enable-console-print!)

(defn run [evt]
  (let [code (-> js/document (.getElementById "code") .-value)
        parse-tree (obj/parse code)
        parse-tree-output (-> js/document (.getElementById "parse-tree"))
        runtime-representation (obj/eval* parse-tree)
        runtime-representation-output (-> js/document (.getElementById "runtime-representation"))
        pretty-representation (obj/pretty* runtime-representation)
        pretty-representation-output (.getElementById js/document "pretty-representation") ]
    (set! (.-value parse-tree-output) parse-tree)
    (set! (.-value runtime-representation-output) runtime-representation)
    (set! (.-value pretty-representation-output) pretty-representation)))

(-> js/document
    (.getElementById "compile")
    (.addEventListener "click" run))

