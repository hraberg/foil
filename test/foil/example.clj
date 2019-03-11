(ns example
  (:use [stdio.h]
        [stdint.h]))

(defn main ^int []
  (let [^int x 0
        "void (*my_fun)(int)" (fn ^void [^int x] (printf "hello, world %d\n" x))]
    (while (< x 10)
      (setq x (+ x 1)))

    (my-fun x)

    (if (= 10 x)
      (printf "ten\n")
      (printf "not ten\n")))

  (return 0))
