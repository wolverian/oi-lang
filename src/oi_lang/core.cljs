(ns oi-lang.core
  (:require [oi-lang.obj :as obj]))

(enable-console-print!)

(defn run [evt]
  (let [code (-> js/document (.getElementById "code") .-value)
        parse-tree (obj/parse code)
        parse-tree-output (-> js/document (.getElementById "parse-tree"))]
    (set! (.-innerText parse-tree-output) parse-tree)))

(-> js/document
    (.getElementById "compile")
    (.addEventListener "click" run))

