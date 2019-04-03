(ns foil.core-test)

(deftest test-partial-and-comp
  (is (= 6 ((partial + 2) 4)))
  (is (= -6 ((comp (fn [x] (- x)) (partial + 2)) 4))))

(defn -main []
  (run-all-tests))
