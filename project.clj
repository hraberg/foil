(defproject foil "0.1.0-SNAPSHOT"
  :description "A layered programming language"
  :url "https://github.com/hraberg/foil"
  :license {:name "The MIT License"
            :url "http://opensource.org/licenses/MIT"}
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [org.clojure/tools.reader "1.3.2"]]
  :profiles {:uberjar {:jvm-opts ["-Dclojure.compiler.direct-linking=true"
                                  "-Dclojure.spec.compile-asserts=false"]}}
  :global-vars {*warn-on-reflection* true}
  :aot [foil.main]
  :main foil.main)
