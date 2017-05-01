(ns oi-lang.core
  (:require [oi-lang.obj :as obj]))

(enable-console-print!)

(defn run [evt]
  (let [code (-> js/document (.getElementById "code") .-value)
        parse-tree (obj/parse code)
        parse-tree-output (-> js/document (.getElementById "parse-tree"))
        _ (set! (.-value parse-tree-output) parse-tree)
        runtime-representation (obj/ast->runtime* parse-tree)
        runtime-representation-output (-> js/document (.getElementById "runtime-representation"))
        _ (set! (.-value runtime-representation-output) runtime-representation)
        evaluated-representation (obj/eval* runtime-representation)
        evaluated-representation-output (-> js/document (.getElementById "evaluated"))
        _ (set! (.-value evaluated-representation-output) evaluated-representation)
        pretty-representation (obj/pretty evaluated-representation)
        pretty-representation-output (.getElementById js/document "pretty-representation")
        _ (set! (.-value pretty-representation-output) pretty-representation)]
    ))

(-> js/document
    (.getElementById "compile")
    (.addEventListener "click" run))

