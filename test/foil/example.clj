(ns example
  (:use [stdio.h]))

(defn main ^int []
  (let [x 0
        my-fun (fn [x] (printf "hello, world %d\n" x))]
    (while (< x 10)
      (setq x (+ x 1)))

    (my-fun x)

    (printf "%s" (if (= 10 x)
                   "ten\n"
                   "not ten\n")))

  0)
