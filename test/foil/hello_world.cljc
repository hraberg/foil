(ns hello-world)

(defn -main []
  (println "Hello" "World" *foil-version*)
  (doseq [x *command-line-args*]
    (println x)))
