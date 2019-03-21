(ns hello-world
  (:require [iostream]))

(defn -main []
  (println "Hello" "World" *foil-version*)
  (doseq [x *command-line-args*]
    (println x)))
