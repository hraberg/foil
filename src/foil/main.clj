(ns foil.main
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io])
  (:import java.io.PushbackReader)
  (:gen-class))

(defn- read-source [in]
  (let [r (PushbackReader. (io/reader in))]
    (->> (repeatedly #(edn/read {:eof ::eof} r))
         (take-while #(not= ::eof %)))))

(defn- emit-ns [[_ name & references :as form]]
  (doseq [[ref-type [lib :as lib-spec]] references]
    (case ref-type
      :use
      (println "#include " (if (symbol? lib)
                             (str "<" lib ">")
                             (pr-str lib)))))
  (println))

(def ^:private default-indent "    ")
(def ^:dynamic ^:private *indent* "")

(defn- emit-application [[f & args :as form]]
  (println (str *indent* f (pr-str args) ";")))

(defn- emit-defn [[_ f args & body :as form]]
  (println (str (:tag (meta args)) " " f "(void)" " {"))
  (binding [*indent* (str *indent* default-indent)]
    (doseq [x body]
      (assert (seq? x) (str "Unsupported form: " (pr-str x)))
      (let [head (first x)]
        (assert (and (symbol? head)
                     (not (special-symbol? head)))
                (str "Unsupported form: " (pr-str x)))
        (emit-application x))))
  (println "}"))

(defn- emit-source [in out]
  (binding [*out* out]
    (doseq [[top-level :as form] (read-source in)]
      (case top-level
        ns (emit-ns form)
        defn (emit-defn form)))))

(defn -main [& args]
  (emit-source *in* *out*))
