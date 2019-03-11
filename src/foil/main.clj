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
    (case (first x)
      ns (do (doseq [x (drop 2 x)]
               (when (= :use (first x))
                 (let [i (first (second x))]
                   (println "#include " (if (symbol? i)
                                          (str "<" i ">")
                                          (pr-str i))))))
             (println))
      defn (let [[_ f args & body] x]
             (println (str (:tag (meta args)) " " f "(void)" " {"))
             (doseq [x body]
               (when (symbol? (first x))
                 (println (str "    " (first x) (pr-str (rest x)) ";"))))
             (println "}")))))
