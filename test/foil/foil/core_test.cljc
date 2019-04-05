(ns foil.core-test)

(deftest test-partial-comp-identity-constantly
  (is (= 6 ((partial + 2) 4)))
  (is (= -6 ((comp (fn [x] (- x)) (partial + 2)) 4)))
  (is (= 5 (identity 5)))
  (is (= 42 ((constantly 42) 1 2))))

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
    (is (= 1 (count (next l))))
    (is (zero? (count (next (next l)))))
    (is (zero? (count (next (next (next l))))))
    (is (= 2 (second (cons 1 2))))
    (is (= ^int '(10 20) (cons 10 (cons 20 ^int ()))))))

(deftest test-optional
  ^:unsafe
  (let [l ^int '(7 8)
        f (first-opt l)]
    (is (true? (boolean f)))
    (is (= 7 @f))
    (is (false? (boolean (first-opt ^int ()))))))

(deftest test-nth
  (is (= "world" (nth ^std::string ["hello" "world"] 1 "?")))
  (is (= "?" (nth ^std::string ["hello" "world"] 3 "?"))))

(defrecord Point ^{:tpl "<typename X = int>"} [^X x ^int y])

(deftest test-c-interop
  (let [pt ^int (Point. -1 2)
        t 3
        ^:mut a ^int [2 t]]
    (is (= -1 (.-x pt)))
    ^:unsafe
    (aset a 0 4)
    ^:unsafe
    (is (= 4 (aget a 0)))
    (is (= "0.00159265" (str (sin 3.14))))
    ^:unsafe
    (is (= 1 ^int (cast 1.2)))))

(deftest test-while
  (let [^:mut x 0]
    (while (< x 10)
      (set! x (+ x 1)))
    (is (= 10 x))))

(deftest test-loop
  (is (= 3 (loop [n 0]
             (if (< n 3)
               (recur (inc n))
               n)))))

(def ^:dynamic *pi* 3.14)

(deftest test-binding
  (is (= "3.14" (str *pi*)))
  (binding [*pi* -1]
    (is (= "-1" (str *pi*))))
  (is (= "3.14" (str *pi*))))

(deftest test-maps
  (let [m ^std::string|std::string {:foo "bar"}]

    (doseq [x m]
      (is (= :foo (key x)))
      (is (= "bar" (val x))))

    (is (= ^std::string [:foo]
           ^std::string (for [x (keys m)]
                          x)))

    (let [^:mut xs ^std::string []]
      (doseq [x ^std::string|std::string (hash-map ^std::string ["foo" "bar"])]
        (conj! xs (key x))
        (conj! xs (val x)))
      (is (= ^std::string ["foo" "bar"] xs)))

    (is (= "3\n4\n1\n2\n"
           (with-out-str
             (doseq [x ^"int,int" (sorted-map-by >
                                                 ^int [1 2]
                                                 ^int [3 4])]
               (println (key x))
               (println (val x))))))))

(deftest test-sets
  (let [s ^int #{1 2}
        l ^int (list 7 8)]

    (is (not (contains? s 7)))
    (is (contains? (set l) 7))
    (is (= ^int [7 8] ^int (for [x ^int (sorted-set 8 7)]
                             x)))

    (is (= ^int [8 7] ^int (for [x ^int (sorted-set-by > 8 7)]
                             x)))))

(deftest test-if-cond
  (let [x 10]
    (is (= "ten" (if (= 10 x)
                   "ten"
                   "not ten")))

    (is (= "ten" (cond
                   (= 10 x)
                   "ten"

                   (= 20 x)
                   "not ten")))))

(deftest test-dotimes
  (let [^:mut x 0]
    (dotimes [n 3]
      (set! x (+ n x)))
    (is (= 3 x))))

(deftest test-doseq
  (let [a ^int [4 3]
        s ^int #{1 2}]
    (is (= "2 4\n2 3\n1 4\n1 3\n"
           (with-out-str
             (doseq [x s
                     y a]
               (println x y)))))))

(deftest test-for
  (is (= 1 (count ^int (for [x ^int '(7 8)
                             :when (even? x)]
                         x)))))

(deftest test-string
  (is (= "llo"(subs "hello" 2)))
  (is (= "el" (subs "hello" 1 3)))

  (is (= "2.2falsefoo" (str 2.2 false "foo")))

  (is (= "42 true" (with-out-str
                     (print 42 true))))
  (is (= 5 (count (str 42 "foo")))))

(deftest test-atom
  ^:unsafe
  (let [^:mut at (atom 2)]
    (is (= 2 @at))
    (swap! at + 3)
    (is (= 5 @at))
    (reset! at 4)
    (is (= 4 @at))))

(deftest test-stl-algorithms
  (let [a ^int [4 3]
        ^:mut x ^int []]

    (std::transform (.begin a)
                    (.end a)
                    (std::back_inserter x)
                    inc)

    (is (= ^int [5 4] x))

    (is (= 9 (std::accumulate (.begin x)
                              (.end x)
                              0
                              +)))

    (let [^:mut zz ^int []]
      (std::copy_if (.begin x)
                    (.end x)
                    (std::back_inserter zz)
                    #(= (mod % 2) 0))
      (is (= ^int [4] zz)))))

(deftest test-map-filter-reduce
  (let [l ^int '(7 8)
        a ^int [4 3]
        xs (map inc a)]

    (is (= ^int [5 4] xs))

    (is (= 9 (reduce + xs)))

    (is (= ^int [4] (->> a
                         (map inc)
                         (filter #(= (mod % 2) 0)))))

    (is (= ^int [5] (foil.core/remove even? xs)))

    (is (= ^int [7 15] (reductions + 0 l)))

    (is (= ^int [14 16] (map + l l)))

    (is (= ^int [7 8 8 9] (mapcat (fn [x]
                                    ^int [x (inc x)]) l)))

    (is (= ^int [0 1] (map-indexed
                       (fn [idx _]
                         ^:unsafe
                         ^int (cast idx))
                       l)))))

(deftest test-seq-fns
  (let [l ^int '(7 8)
        s ^int #{1 2}]
    (is (= 4 (count (concat l s))))
    (is (= ^int '(8 7) (reverse l)))

    (is (true? (every? pos? l)))
    (is (false? (every? neg? l)))
    (is (= ^int [8] (drop 1 l)))
    (is (= ^int [7] (take 1 l)))

    (is (= ^int [8] (get (group-by even? l) true)))
    (is (= ^int [7] (get (group-by even? l) false)))

    (is (= ^int [7 1 8] (interpose 1 l)))
    (is (= ^int [7 1 8 2] (interleave l ^int [1 2])))

    (is (= ^std::vector<int> [^int [1 2] ^int [3 4]] (partition 2 ^int [1 2 3 4])))
    (is (= ^std::vector<int> [^int [1 2]] (partition 2 ^int [1 2 3])))

    (is (= ^std::string|int {:foo 1 :bar 2} (zipmap ^std::string [:foo :bar] ^int [1 2])))
    (is (= ^std::string|int {:foo 1} (select-keys ^std::string|int {:foo 1 :bar 2} ^std::string [:foo])))))

(deftest test-transducers
  (is (= ^int [8 9] (transduce (map-xform inc) conj! ^int [] ^int [7 8])))
  (is (= ^int [8] (transduce (filter-xform even?) conj! ^int [] ^int [7 8])))

  (is (= ^int [9] (transduce (comp (filter-xform even?)
                                   (map-xform inc)) conj! ^int [] ^int [7 8]))))

(deftest test-empty
  (let [^:mut xs ^int [7]]
    (is (zero? (count (empty! xs))))
    (is (zero? (count xs))))

  (let [xs ^int [7]]
    (is (zero? (count (empty xs))))
    (is (= 1 (count xs))))

  (is (empty? ^int (hash-set)))
  (is (empty? ^std::string|std::string (hash-map))))

(deftest test-repeat-repeatedly
  (is (= 3 (count (repeat 3 42))))
  (is (= 42 (first (repeat 3 42))))
  (is (= ^int [42 42 42] (repeatedly 3 (constantly 42)))))

(defn -main ^int []
  (run-all-tests))
