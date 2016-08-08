(defproject clojure-repl "0.1.0-SNAPSHOT"
  :description "clojure implementation of SICP code and exercises"
  ;; :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
        :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [
             [org.clojure/clojure "1.8.0"]
            ]
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
