(ns example
  (:use [stdio.h]
        [stdint.h]))

(defn main ^int []
  (let [^int x 0]
    (while (< x 10)
      (setq x (+ x 1)))

    ((fn ^void [^int x] (printf "hello, world %d\n" x)) x))

  (return 0))
