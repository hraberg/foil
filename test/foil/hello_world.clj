(ns foil.hello-world
  (:require [iostream]))

(defn println [s]
  (<< (<< std::cout s) std::endl))

(defn -main []
  (println "Hello World"))
