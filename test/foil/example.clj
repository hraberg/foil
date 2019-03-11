(ns example
  (:use [stdio.h]
        [stdint.h]))

(defn main ^int []
  (printf "hello, world\n")
  (return 0))
