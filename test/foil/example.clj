(ns foil.example
  (:require [cstdio]
            [cmath]
            [algorithm]
            [iostream]))

(defrecord Point [^int x ^int y])

(def ^:const foo 3.14)
(def ^:dynamic *pi* 3.14)

(defn println ^void [^std::string x]
  (<< (<< (<< std::cout x) " str") std::endl))

(defn println ^void [x]
  (<< (<< std::cout x) std::endl))

(defn -main []
  (let [x 0
        pt (Point. -1 2)
        t 3
        a ^int [2 t]
        m ^std::string {:foo "bar"}
        s ^int #{1 2}
        l ^int '(7 8)
        my-fun (fn [x] (printf (.c_str "hello, world %d\n") x))]

    (aset a 0 4)
    (printf (.c_str "%d %d %s %.2f %lu %lu\n") (.-x pt) (aget a 0) (.c_str (:foo m)) foo (.count s 1) (.count s 3))

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

    (let* [x ^int []
           z ^int []]

      (std::transform (.begin a)
                      (.end a)
                      (std::back_inserter x)
                      #(inc %))

      (std::for_each (.begin x)
                     (.end x)
                     #(println %))

      (println (std::accumulate (.begin x)
                                (.end x)
                                0
                                (fn [x y] (+ x y))))

      (let [zz ^int []]
        (std::copy_if (.begin x)
                      (.end x)
                      (std::back_inserter zz)
                      #(= (mod % 2) 0))
        (doseq [x zz]
          (println x))))

    0))
