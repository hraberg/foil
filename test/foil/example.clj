(ns example
  (:use [stdio.h]))

(defrecord Point [^int x ^int y])

(defn main ^int []
  (let [x 0
        pt (Point. -1 2)
        my-fun (fn [x] (printf "hello, world %d\n" x))]

    (printf "%d\n" (.x pt))

    (while (< x 10)
      (setq x (+ x 1)))

    (my-fun x)

    (printf "%s" (if (= 10 x)
                   "ten\n"
                   "not ten\n"))

    0))
