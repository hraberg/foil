(ns foil.hello-world
  (:require [iostream]))

(defn println ^void [s]
  (<< (<< std::cout s) std::endl))

(defn -main [args]
  (println "Hello World")
  (doseq [x args]
    (println x)))
