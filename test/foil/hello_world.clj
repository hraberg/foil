(ns foil.hello-world
  (:require [iostream]))

(defn println ^void [s]
  (<< (<< std::cout s) std::endl))

(defn -main []
  (println "Hello World"))
