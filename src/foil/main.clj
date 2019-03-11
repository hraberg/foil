(ns foil.main
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:import java.io.PushbackReader)
  (:gen-class))

(defn- read-source [in]
  (let [r (PushbackReader. (io/reader in))]
    (->> (repeatedly #(edn/read {:eof ::eof} r))
         (take-while #(not= ::eof %)))))

(defn- emit-ns [[_ name & references :as form]]
  (doseq [[ref-type & lib-specs] references
          [lib :as lib-spec] lib-specs]
    (case ref-type
      :use
      (println "#include " (if (symbol? lib)
                             (str "<" lib ">")
                             (pr-str lib)))))
  (println))

(def ^:private default-indent "    ")
(def ^:dynamic ^:private *indent* "")

(defn- emit-application [[f & args :as form]]
  (println (str *indent* f "(" (str/join ", " (map pr-str args)) ");")))

(defn- emit-return [[_ value :as from]]
  (println (str *indent* "return " (pr-str value) ";")))

(defn- emit-assignment [[_ var value :as from]]
  (println (str *indent* var " = " (pr-str value) ";")))

(defn- emit-defn [[_ f args & body :as form]]
  (println (str (:tag (meta args))
                " "
                f
                (str "("
                     (->> args
                          (map #(str (:tag (meta %) "void*") " " %))
                          (str/join ", "))
                     ")")
                " {"))
  (binding [*indent* (str *indent* default-indent)]
    (doseq [x body]
      (assert (seq? x) (str "Unsupported form: " (pr-str x)))
      (let [head (first x)]
        (assert (and (symbol? head)
                     (not (special-symbol? head)))
                (str "Unsupported form: " (pr-str x)))
        (case head
          return (emit-return x)
          setq (emit-assignment x)
          (emit-application x)))))
  (println "}"))

(defn- emit-source [in out]
  (binding [*out* out]
    (doseq [[top-level :as form] (read-source in)]
      (case top-level
        ns (emit-ns form)
        defn (emit-defn form)))))

(defn -main [& args]
  (emit-source *in* *out*))
