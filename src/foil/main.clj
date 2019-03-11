(ns foil.main
  (:require [clojure.java.io :as io])
  (:import java.io.PushbackReader)
  (:gen-class))

(defn- read-from [in]
  (with-open [r (PushbackReader. (io/reader in))]
    (binding [*read-eval* false]
      (->> (repeatedly #(read r false ::eof))
           (take-while #(not= ::eof %))
           (vec)))))

(defn -main [& args]
  (doseq [x (read-from *in*)]
    (prn x)))
