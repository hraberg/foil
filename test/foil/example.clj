(ns example
  (:use [stdio.h]))

(defrecord Point [^int x ^int y])

(def ^:const foo 3.14)

(defn main ^int []
  (let [x 0
        pt (Point. -1 2)
        a ^int [2 3]
        m ^"const char*" {:foo "bar"}
        my-fun (fn [x] (printf "hello, world %d\n" x))]

    (aset a 0 4)
    (printf "%d %d %s %.2f\n" (.-x pt) (aget a 0) (:foo m) foo)

    (while (< x 10)
      (set! x (+ x 1)))

    (my-fun x)

    (printf "%s" (if (= 10 x)
                   "ten\n"
                   "not ten\n"))

    0))
