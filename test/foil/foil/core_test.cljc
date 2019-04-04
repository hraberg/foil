(ns foil.core-test)

(deftest test-partial-and-comp-and-identity
  (is (= 6 ((partial + 2) 4)))
  (is (= -6 ((comp (fn [x] (- x)) (partial + 2)) 4)))
  (is (= 5 (identity 5))))

(deftest test-dates
  (let [d #inst "1980"]
    (is (= 315532800000 (inst-ms d)))))

(deftest test-sort
  (let [ss ^int [2 3 1]]
    (is (= ^int [1 2 3] (sort ss)))))

(deftest test-min-max
  (is (= 2 (min 2 3)))
  (is (= 3 (max 2 3))))

(deftest test-rc
  ^:unsafe
  (let [sp1 ^std::vector<int> (rc ^int [1 2])
        sp2 (identity sp1)
        w (weak sp2)]
    (weak w)
    (weak (weak sp2))
    (is (= 1 (aget @sp1 0)))
    (aset (* sp1) 0 0)
    (is (= @sp2 @sp1))
    (aset (* sp2) 1 3)
    (is (= @sp2 @sp1))
    (is (= 2 (.use_count sp1)))))

(deftest test-cons
  (let [l ^int '(7 8)]
    (is (= 8 (first (next l))))
    (is (= 7 (first l)))
    (is (= 8 (second l)))
    (is (= 2 (count l)))
    (is (= 6 (first (cons 6 l))))
    (is (= l ^int '(7 8)))
    (is (not= l (cons 6 l)))
    ;; (is (= 1 (count (next l))))
    ;; (is (zero? (count (next (next l)))))
    ;; (is (zero? (count (next (next (next l))))))
    (is (= 2 (second (cons 1 2))))))

(defn -main ^int []
  (run-all-tests))
