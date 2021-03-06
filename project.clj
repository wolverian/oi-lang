(defproject oi-lang "0.1.0-SNAPSHOT"
  :description "A small programming language"
  :url "https://oi-lang.now.sh"
  :license {:name "ISC"
            :url  "http://www.isc.org/downloads/software-support-policy/isc-license/"}

  :min-lein-version "2.7.1"

  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/clojurescript "1.9.562"]
                 [org.clojure/core.async "0.3.443"
                  :exclusions [org.clojure/tools.reader]]
                 [org.clojure/core.match "0.3.0-alpha4"]
                 [instaparse "1.4.7"]]

  :source-paths ["src"]
  :test-paths ["test"]

  :cljsbuild {:builds
              [{:id           "karma"
                :source-paths ["src" "test"]
                :compiler     {:output-to     "run/compiled/karma/test.js"
                               :source-map    "run/compiled/karma/test.js.map"
                               :output-dir    "run/compiled/karma/test"
                               :main          "oi_lang.test_runner"
                               :optimizations :whitespace
                               :pretty-print  true}}
               {:id           "dev"
                :source-paths ["src"]

                ;; the presence of a :figwheel configuration here
                ;; will cause figwheel to inject the figwheel client
                ;; into your build
                :figwheel     {;;:on-jsload "oi-lang.core/on-js-reload"
                               ;; :open-urls will pop open your application
                               ;; in the default browser once Figwheel has
                               ;; started and complied your application.
                               ;; Comment this out once it no longer serves you.
                               :open-urls ["http://localhost:3449/index.html"]}

                :compiler     {:main                 oi-lang.core
                               :asset-path           "js/compiled/out"
                               :output-to            "resources/public/js/compiled/oi_lang.js"
                               :output-dir           "resources/public/js/compiled/out"
                               :source-map-timestamp true
                               ;; To console.log CLJS data-structures make sure you enable devtools in Chrome
                               ;; https://github.com/binaryage/cljs-devtools
                               :preloads             [devtools.preload]}}
               ;; This next build is an compressed minified build for
               ;; production. You can build this with:
               ;; lein cljsbuild once min
               {:id           "production"
                :source-paths ["src"]
                :compiler     {:output-to     "resources/public/js/compiled/oi_lang.js"
                               :main          oi-lang.core
                               :optimizations :advanced
                               :pretty-print  false}}]}

  :figwheel {;; :http-server-root "public" ;; default and assumes "resources"
             ;; :server-port 3449 ;; default
             ;; :server-ip "127.0.0.1"

             :css-dirs ["resources/public/css"]             ;; watch and update CSS

             ;; Start an nREPL server into the running figwheel process
             ;; :nrepl-port 7888

             ;; Server Ring Handler (optional)
             ;; if you want to embed a ring handler into the figwheel http-kit
             ;; server, this is for simple ring servers, if this

             ;; doesn't work for you just run your own server :) (see lein-ring)

             ;; :ring-handler hello_world.server/handler

             ;; To be able to open files in your editor from the heads up display
             ;; you will need to put a script on your path.
             ;; that script will have to take a file path and a line number
             ;; ie. in  ~/bin/myfile-opener
             ;; #! /bin/sh
             ;; emacsclient -n +$2 $1
             ;;
             ;; :open-file-command "myfile-opener"

             ;; if you are using emacsclient you can just use
             ;; :open-file-command "emacsclient"

             ;; if you want to disable the REPL
             ;; :repl false

             ;; to configure a different figwheel logfile path
             ;; :server-logfile "tmp/logs/figwheel-logfile.log"

             ;; to pipe all the output to the repl
             ;; :server-logfile false
             }

  :npm {:devDependencies [[karma "1.6.0"]
                          [karma-cljs-test "^0.1.0"]
                          [karma-phantomjs-launcher "^1.0.4"]
                          [karma-chrome-launcher "^2.0.0"]
                          [karma-junit-reporter "^1.2.0"]]}

  ;; setting up nREPL for Figwheel and ClojureScript dev
  ;; Please see:
  ;; https://github.com/bhauman/lein-figwheel/wiki/Using-the-Figwheel-REPL-within-NRepl
  :profiles {:dev        {:dependencies [[karma-reporter "2.1.2"]
                                         [binaryage/devtools "0.9.4"]
                                         [figwheel-sidecar "0.5.10"]
                                         [com.cemerick/piggieback "0.2.1"]
                                         [org.clojure/tools.nrepl "0.2.13"]]
                          :plugins      [[lein-ancient "0.6.10"]
                                         [lein-kibit "0.1.5"]
                                         [lein-npm "0.6.2"]
                                         [lein-figwheel "0.5.10"]
                                         [cider/cider-nrepl "0.14.0"]
                                         [lein-cljsbuild "1.1.6" :exclusions [[org.clojure/clojure]]]]
                          ;; need to add dev source path here to get user.clj loaded
                          :source-paths ["src" "dev"]
                          ;; for CIDER
                          ;; :plugins [[cider/cider-nrepl "0.12.0"]]
                          :repl-options {:nrepl-middleware [cemerick.piggieback/wrap-cljs-repl]}
                          ;; need to add the compliled assets to the :clean-targets
                          }
             :production {:source-paths ["src"]}}
  :clean-targets ^{:protect false} ["resources/public/js" "run" :target-path]
  :aliases {"karma-once" ["do" "clean," "cljsbuild" "once" "karma,"]
            "karma-auto" ["cljsbuild" "auto" "karma"]})
