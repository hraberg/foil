(ns example
  (:use [stdio.h]
        [stdint.h]))

(defn main ^int []
  (let [^int x 0
        "void (*f)(int)" (fn ^void [^int x] (printf "hello, world %d\n" x))]
    (while (< x 10)
      (setq x (+ x 1)))

    (f x))

  (return 0))
