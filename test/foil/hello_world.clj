(ns hello-world
  (:use [iostream]))

(defn -main []
  (<< std::cout "Hello World\n"))
