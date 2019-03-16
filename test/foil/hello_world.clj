(ns hello-world
  (:require [iostream]))

(defn -main []
  (<< std::cout "Hello World\n"))
