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

(def ^:private default-tag "void*")

(defn form->tag [form]
  (:tag (meta form) default-tag))

(defn- emit-include [[_ header]]
  (println "#include " (if (symbol? header)
                         (str "<" header ">")
                         (pr-str header))))

(defn- emit-headers [[_ name & references :as form]]
  (doseq [[ref-type & lib-specs] references
          [lib :as lib-spec] lib-specs]
    (case ref-type
      :use
      (emit-include ['include lib])))
  (println))

(def ^:private default-indent "    ")
(def ^:dynamic ^:private *indent* "")

(def ^:private unary-op '{inc ++
                          dec --
                          not !
                          ! !
                          bit-not "~"})

(def ^:private binary-op '{+ +
                           - -
                           / /
                           * *
                           mod %
                           % %
                           < <
                           <= <=
                           > >
                           >= >=
                           = ==
                           == ==
                           not= !=
                           != !=
                           and &&
                           or ||
                           bit-and &
                           & &
                           bit-or |
                           | |
                           bit-xor "^"
                           bit-shift-left <<
                           << <<
                           bit-shift-right >>
                           >> >>
                           unsigned-bit-shift-right >>>
                           >>> >>>})

(defn- emit-application [[f & args :as form]]
  (cond
    (contains? unary-op f)
    (print (get unary-op f) (first args))

    (contains? binary-op f)
    (print (first args) " " (get binary-op f) " "  (second args))

    :else
    (print f "(" (str/join ", " (map pr-str args)) ")")))

(defn- emit-return [[_ value :as from]]
  (print "return " (pr-str value)))

(defn- emit-assignment [[_ var value :as from]]
  (print var " = " (pr-str value)))

(declare emit-block emit-expression)

(defn- emit-while [[_ condition & body :as form]]
  (print "while (")
  (emit-expression condition)
  (print ")")
  (emit-block body))

(defn- emit-bindings [bindings]
  (doseq [[var binding] (partition 2 bindings)]
    (println (form->tag var) " " var " " (pr-str binding) ";")
    (println ";")))

(defn- emit-let [[_ bindings & body :as form]]
  (emit-bindings bindings)
  (emit-block body))

(defn- emit-conditional [[_ condition then else :as form]]
  (emit-expression condition)
  (print " ? ")
  (emit-expression then)
  (print " : ")
  (emit-expression else))

(def ^:dynamic ^:private *loop-state*)

(defn- emit-loop-init [[_ bindings & body :as form]]
  (emit-bindings bindings)
  (binding [*loop-state* {:label (gensym "loop")
                          :vars (mapv first bindings)}]
    (println (:label *loop-state*) ":")
    (emit-block body)))

(defn- emit-goto [[_ & expressions :as form]]
  (assert *loop-state* "Not in a loop.")
  (doseq [[var expression] (map vector (:vars *loop-state*) expressions)]
    (print var " = ")
    (emit-expression expression)
    (println ";"))
  (println "goto "(:label *loop-state*) ";"))

(defn- emit-expression [[op :as form]]
  (case op
    if (emit-conditional form)
    (do (assert (and (symbol? op)
                     (not (special-symbol? op)))
                (str "Unsupported form: " (pr-str form)))
        (when-let [tag (:tag (meta form))]
          (print "(" tag ") "))
        (emit-application form))))

(defn- emit-expression-statement [[op :as form]]
  (case op
    return (emit-return form)
    setq (emit-assignment form)
    while (emit-while form)
    let (emit-let form)
    loop (emit-loop-init form)
    recur (emit-goto form)
    (emit-expression form)))

(defn- emit-block [body]
  (println "{")
  (binding [*indent* (str *indent* default-indent)]
    (doseq [x body]
      (assert (seq? x) (str "Unsupported form: " (pr-str x)))
      (print *indent*)
      (emit-expression-statement x)
      (println ";")))
  (println "}"))

(defn- emit-function [[_ f args & body :as form]]
  (print (str (form->tag args)
              " "
              f
              (str "("
                   (->> args
                        (map #(str (form->tag %) " " %))
                        (str/join ", "))
                   ") ")))
  (emit-block body))


(defn- emit-struct [[_ name fields :as form]]
  (println "typedef struct {")
  (doseq [field fields]
    (print (form->tag field) " " field ";"))
  (println "} " name ";"))

(defn- emit-source [in out]
  (binding [*out* out]
    (doseq [[top-level :as form] (read-source in)]
      (case top-level
        ns (emit-headers form)
        (include, use) (emit-include form)
        (defn, defun) (emit-function form)
        (defrecord, defstruct) (emit-struct form)))))

(defn -main [& args]
  (emit-source *in* *out*))
