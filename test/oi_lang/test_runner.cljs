(ns oi-lang.test-runner
  (:require [doo.runner :refer-macros [doo-tests]]
            [oi-lang.obj-tests]))

(doo-tests 'oi-lang.obj-tests)
