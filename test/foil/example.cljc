(ns example
  (:require [cstdio]
            [cmath]
            [algorithm]
            [iostream]))

(defrecord Point ^{:tpl "<typename X = int>"} [^X x ^int y])

(def foo 3.14)
(def ^:dynamic *pi* 3.14)

(defn test-println
  (^void [^std::string x]
   (<< std::cout x " str" std::endl))
  (^void [x]
   (println x)))

(defn -main []
  (let [x 10
        pt ^int (Point. -1 2)
        t 3
        ^:mut a ^int [2 t]
        m ^std::string|std::string {:foo "bar"}
        s ^int #{1 2}
        l ^int (list 7 8)
        r #"\n"
        my-fun (fn [x] (printf (.c_str "hello, world %d\n") x))]

    ^:unsafe (aset a 0 4)
    ^:unsafe (printf (.c_str "%d %d %s %.2f %lu %lu\n") (.-x pt) (aget a 0) (.c_str (get m :foo)) foo (contains? s 1) (contains? s 3))

    (doseq [x m]
      (println (key x))
      (println (val x)))

    (doseq [x (keys m)]
      (println x))

    (doseq [x s
            y a]
      (printf (.c_str "%d %d\n") x y))

    (dotimes [n 3]
      (test-println n))

    (my-fun x)

    (println (count (concat l s)))
    (println (= ^int '(8 7) (reverse l)))

    (println (every? pos? l))
    (println (every? neg? l))
    (println (= ^int [8] (drop 1 l)))
    (println (= ^int [7] (take 1 l)))

    (let [^:mut xs ^int [7]]
      (println (count (empty! xs)))
      (println (count xs)))

    (println (= ^int '[8] (get (group-by even? l) true)))
    (println (= ^int '[7] (get (group-by even? l) false)))

    (let [xs ^int [7]]
      (println (count (empty xs)))
      (println (count xs)))

    (doseq [x (map-indexed (fn [idx _]
                             idx) l)]
      (println x))

    (println (count ^std::size_t (for [x (map-indexed (fn [idx _]
                                                        idx) l)
                                       :when (even? x)]
                                   x)))

    (println (empty? ^int (hash-set)))
    (println (empty? ^std::string|std::string (hash-map)))

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
