(ns example
  (:require [cstdio]
            [cmath]
            [algorithm]
            [iostream]))

(def foo 3.14)

(defn -main []
  (let [m ^std::string|std::string {:foo "bar"}
        s ^int #{1 2}
        l ^int (list 7 8)]

    (doseq [x m]
      (println (key x))
      (println (val x)))

    (doseq [x (keys m)]
      (println x))

    (println (contains? (set l) 7))
    (doseq [x ^int (sorted-set 8 7)]
      (println x))

    (doseq [x ^int (sorted-set-by > 8 7)]
      (println x))

    (doseq [x ^std::string|std::string (hash-map ^std::string ["foo" "bar"])]
      (println (key x))
      (println (val x)))

    (doseq [x ^"int,int" (sorted-map-by >
                                        ^int [1 2]
                                        ^int [3 4])]
      (println (key x))
      (println (val x)))

    0))
