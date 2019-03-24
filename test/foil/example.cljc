(ns example
  (:require [cstdio]
            [cmath]
            [algorithm]
            [functional]
            [iostream]
            [memory]))

(defrecord Point ^{:tpl "<typename X = int>"} [^X x ^int y])

(def foo 3.14)
(def ^:dynamic *pi* 3.14)

(defn test-println
  (^void [^std::string x]
   (<< std::cout x " str" std::endl))
  (^void [x]
   (println x)))

(defn -main []
  (let [^:mut x 0
        pt ^int (Point. -1 2)
        t 3
        ^:mut a ^int [2 t]
        m ^std::string {:foo "bar"}
        s ^int #{1 2}
        l ^int (list 7 8)
        my-fun (fn [x] (printf (.c_str "hello, world %d\n") x))
        ^:mut ss ^int [2 3 1]]

    (println ((partial + 2) 4))
    (println)

    (doseq [x (sort ss)]
      (println x))

    (let [sp1 ^std::vector<int> (std::make_shared ^int [1 2])
          sp2 (identity sp1)]
      (println (aget @sp1 0))
      (aset (* sp1) 0 0)
      (println (= @sp2 @sp1))
      (aset (* sp2) 1 3)
      (println (= @sp2 @sp1))
      (println (.use_count sp1)))

    (println (identity 5))

    (println (min 3 2))

    (println (first (next l)))
    (println (first l))
    (println (second l))
    (println (count l))
    (println (first (cons 6 l)))
    (println (= l ^int '(7 8)))
    (println (= l (cons 6 l)))
    (println (count (next l)))
    (println (count (next (next l))))
    (println (count (next (next (next l)))))

    (println @(first-opt l))
    (println (boolean (first-opt ^int ())))

    (println (nth ^std::string ["hello" "world"] 1 "?"))
    (println (nth ^std::string ["hello" "world"] 3 "?"))

    (aset a 0 4)
    (printf (.c_str "%d %d %s %.2f %lu %lu\n") (.-x pt) (aget a 0) (.c_str (get m :foo)) foo (contains? s 1) (contains? s 3))

    (doseq [x m]
      (println (key x))
      (println (val x)))

    (doseq [x (keys m)]
      (println x))

    (while (< x 10)
      (set! x (+ x 1)))

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

    (let [xs ^int [7]]
      (println (count (empty xs)))
      (println (count xs)))

    (doseq [x (map-indexed (fn [idx _]
                             idx) l)]
      (println x))

    (println (contains? (set l) 7))
    (doseq [x ^int (sorted-set 8 7)]
      (println x))

    (doseq [x ^int (sorted-set-by > 8 7)]
      (println x))

    (doseq [x ^"std::string,std::string" (hash-map ^std::string ["foo" "bar"])]
      (println (key x))
      (println (val x)))

    (doseq [x ^"int,int" (sorted-map-by >
                                        ^int [1 2]
                                        ^int [3 4])]
      (println (key x))
      (println (val x)))

    (test-println *pi*)
    (binding [*pi* -1]
      (test-println *pi*))
    (test-println *pi*)

    (test-println (if (= 10 x)
                    "ten"
                    "not ten"))

    (test-println (cond
                    (= 10 x)
                    "ten"

                    (= 20 x)
                    "not ten"))

    (test-println 42)

    (let [s "foo"]
      (test-println s))

    (test-println (first l))

    (test-println (sin *pi*))

    (test-println (loop [n 0]
                    (if (< n 3)
                      (recur (inc n))
                      n)))

    (let [^:mut x ^int []
          ^:mut z ^int []]

      (std::transform (.begin a)
                      (.end a)
                      (std::back_inserter x)
                      inc)

      (std::for_each (.begin x)
                     (.end x)
                     test-println)

      (test-println (std::accumulate (.begin x)
                                     (.end x)
                                     0
                                     +))

      (let [^:mut zz ^int []]
        (std::copy_if (.begin x)
                      (.end x)
                      (std::back_inserter zz)
                      #(= (mod % 2) 0))
        (doseq [x zz]
          (test-println x))))

    (let [xs (map inc a)]
      (doseq [x xs]
        (test-println x))

      (test-println (reduce + xs))

      (doseq [x (->> a
                     (map inc)
                     (filter #(= (mod % 2) 0)))]
        (test-println x)))

    (let [^:mut ^std::atomic<int> at 2]
      (println @at)
      (swap! at + 3)
      (println @at))

    0))
