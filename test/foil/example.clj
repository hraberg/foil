(ns foil.example
  (:require [cstdio]
            [cmath]
            [algorithm]
            [functional]
            [iostream]))

(defrecord Point [^int x ^int y])

(def foo 3.14)
(def ^:dynamic *pi* 3.14)

(def ^"std::plus<>" +)
(def ^"std::minus<>" -)
(def ^"std::multiplies<>" *)
(def ^"std::divides<>" /)
(def ^"std::modulus<>" %)

(def ^"std::equal_to<>" =)
(def ^"std::not_equal_to<>" !=)
(def ^"std::greater<>" >)
(def ^"std::less<>" <)
(def ^"std::greater_equal<>" >=)
(def ^"std::less_equal<>" <=)

(def ^"std::logical_and<>" &&)
(def ^"std::logical_or<>" ||)
(def ^"std::logical_not<>" !)

(def ^"std::bit_and<>" &)
(def ^"std::bit_or<>" |)
($code "const std::bit_or<> _CARET_;")
($code "const std::bit_not<> _TILDE_;")

(defn map [^:ref f ^:ref coll]
  (let [^:mut acc ^"decltype(f(std::declval<typename __T_coll::value_type>()))" []]
    (doseq [x coll]
      (.push_back acc (f x)))
    acc))

(defn filter [^:ref pred ^:ref coll]
  (let [^:mut acc ^"typename __T_coll::value_type" []]
    (doseq [x coll]
      (when (pred x)
        (.push_back acc x)))
    acc))

(defn reduce
  ([^:ref f ^:ref coll]
   (let [^:mut acc (.front coll)
         ^:mut first true]
     (doseq [x coll]
       (if first
         (set! first false)
         (set! acc (f acc x))))
     acc))
  ([^:ref f ^:ref val ^:ref coll]
   (let [^:mut acc val]
     (doseq [x coll]
       (set! acc (f acc x)))
     acc)))

(defn println
  (^void [^std::string x]
   (<< (<< (<< std::cout x) " str") std::endl))
  (^void [x]
   (<< (<< std::cout x) std::endl)))

(defn -main []
  (let [^:mut x 0
        pt (Point. -1 2)
        t 3
        ^:mut a ^int [2 t]
        m ^std::string {:foo "bar"}
        s ^int #{1 2}
        l ^int '(7 8)
        my-fun (fn [x] (printf (.c_str "hello, world %d\n") x))]

    (aset a 0 4)
    (printf (.c_str "%d %d %s %.2f %lu %lu\n") (.-x pt) (aget a 0) (.c_str (.at m :foo)) foo (.count s 1) (.count s 3))

    (while (< x 10)
      (set! x (+ x 1)))

    (doseq [x s
            y a]
      (printf (.c_str "%d %d\n") x y))

    (dotimes [n 3]
      (println n))

    (my-fun x)

    (println *pi*)
    (binding [*pi* -1]
      (println *pi*))
    (println *pi*)

    (println (if (= 10 x)
               "ten"
               "not ten"))

    (println (cond
               (= 10 x)
               "ten"

               (= 20 x)
               "not ten"))

    (println 42)

    (let [s "foo"]
      (println s))

    (println (.front l))

    (println (sin *pi*))

    (println (loop [n 0]
               (if (< n 3)
                 (recur (inc n))
                 n)))

    (let [^:mut x ^int []
          ^:mut z ^int []]

      (std::transform (.begin a)
                      (.end a)
                      (std::back_inserter x)
                      #(inc %))


      (std::for_each (.begin x)
                     (.end x)
                     println)

      (println (std::accumulate (.begin x)
                                (.end x)
                                0
                                +))

      (let [^:mut zz ^int []]
        (std::copy_if (.begin x)
                      (.end x)
                      (std::back_inserter zz)
                      #(= (mod % 2) 0))
        (doseq [x zz]
          (println x))))

    (let [xs (map #(inc %) a)]
      (doseq [x xs]
        (println x))

      (println (reduce + xs))

      (doseq [x (filter #(= (mod % 2) 0)
                        (map #(inc %) a))]
        (println x)))
    0)
  )
