(ns hello-world
  (:require [iostream]))

(defn -main []
  (println "Hello World")
  (doseq [x *command-line-args*]
    (println x)))
