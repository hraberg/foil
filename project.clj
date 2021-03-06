(defproject foil "0.1.0-SNAPSHOT"
  :description "A small compiled and statically typed Lisp"
  :url "https://github.com/hraberg/foil"
  :license {:name "The MIT License"
            :url "http://opensource.org/licenses/MIT"}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [org.clojure/tools.reader "1.3.2"]]
  :profiles {:uberjar {:jvm-opts ["-Dclojure.compiler.direct-linking=true"
                                  "-Dclojure.spec.compile-asserts=false"
                                  "-Dclojure.spec.skip-macros=true"]}}
  :resource-paths ["resources" "src/foil"]
  :source-paths ["src/clj"]
  :test-paths ["test/clj"]
  :global-vars {*warn-on-reflection* true}
  :aot [foil.main]
  :main foil.main)
