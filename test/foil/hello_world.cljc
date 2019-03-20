(ns hello-world
  (:require [iostream]))

(defn -main [args]
  (println "Hello World")
  (doseq [x args]
    (println x)))
