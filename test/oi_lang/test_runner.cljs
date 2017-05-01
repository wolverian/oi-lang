(ns oi-lang.test-runner
  (:require [cljs.test :as cljs-test :include-macros true]
            [jx.reporter.karma :as karma :include-macros true]
            [oi-lang.obj-tests]))

(defn ^:export run-karma [karma]
  (karma/run-tests
    karma
    'oi-lang.obj-tests))
