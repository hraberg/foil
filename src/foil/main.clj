(ns foil.main
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.walk :as w])
  (:import java.io.PushbackReader)
  (:gen-class))

(defn- read-source [in]
  (let [r (PushbackReader. (io/reader in))]
    (->> (repeatedly #(edn/read {:eof ::eof} r))
         (take-while #(not= ::eof %)))))

(def ^:private default-tag "void*")

(defn form->tag
  ([form]
   (form->tag form default-tag))
  ([form default]
   (:tag (meta form) default)))

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

(def ^:private unary-op '{not !
                          ! !
                          bit-not "~"})

(def ^:private unary-inc-dec-op '{inc +
                                  dec -})

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

(declare emit-block emit-expression)

(defn- emit-application [[f & args :as form]]
  (cond
    (contains? unary-op f)
    (do (print (get unary-op f))
        (emit-expression (first args)))

    (contains? unary-inc-dec-op f)
    (do (emit-expression (first args))
        (print (str " " (get unary-inc-dec-op f) " 1")))

    (contains? binary-op f)
    (do (emit-expression (first args))
        (print (str " " (get binary-op f)  " "))
        (emit-expression (second args)))

    :else
    (do (emit-expression f)
        (print (str "(" (str/join ", " (map #(with-out-str
                                               (emit-expression %)) args)) ")")))))

(defn- emit-return [[_ value :as from]]
  (print (str *indent* "return "))
  (emit-expression value)
  (println ";"))

(defn- emit-assignment [[_ var value :as from]]
  (print (str *indent* var " = "))
  (emit-expression value)
  (println ";"))

(defn- emit-while [[_ condition & body :as form]]
  (print (str *indent* "while ("))
  (emit-expression condition)
  (print ")")
  (emit-block body " "))

(defn- emit-bindings [bindings]
  (doseq [[var binding] (partition 2 bindings)]
    (print (str *indent* (form->tag var) " " var " = "))
    (emit-expression binding)
    (println ";")))

(defn- emit-let [[_ bindings & body :as form]]
  (emit-bindings bindings)
  (emit-block body))

(defn- emit-conditional [[_ condition then else :as form]]
  (print *indent*)
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
    (println (str *indent* (:label *loop-state*) ":"))
    (emit-block body)))

(defn- emit-goto [[_ & expressions :as form]]
  (assert *loop-state* "Not in a loop.")
  (doseq [[var expression] (map vector (:vars *loop-state*) expressions)]
    (print (str *indent* var " = "))
    (emit-expression expression)
    (println ";"))
  (println (str *indent* "goto "(:label *loop-state*) ";")))

(def ^:dynamic ^:private *lambda-state* (atom []))

(defn- emit-lambda [form]
  (let [sym (gensym "lambda")]
    (swap! *lambda-state* conj [sym form])
    (print sym)))

(defn- emit-expression [form]
  (when-let [tag (form->tag form nil)]
    (print "(" tag ") "))
  (if (seq? form)
    (let [op (first form)]
      (case op
        if (emit-conditional form)
        (fn, lambda) (emit-lambda form)
        (do (assert (not (special-symbol? op))
                    (str "Unsupported form: " (pr-str form)))
            (emit-application form))))
    (pr form)))

(defn- emit-expression-statement [[op :as form]]
  (case op
    return (emit-return form)
    setq (emit-assignment form)
    while (emit-while form)
    let (emit-let form)
    loop (emit-loop-init form)
    recur (emit-goto form)
    (do (print *indent*)
        (emit-expression form)
        (println ";"))))

(defn- emit-body [body]
  (doseq [x body]
    (assert (seq? x) (str "Unsupported form: " (pr-str x)))
    (emit-expression-statement x)))

(defn- emit-block
  ([body]
   (emit-block body  *indent*))
  ([body initial-indent]
   (println (str initial-indent"{"))
   (binding [*indent* (str *indent* default-indent)]
     (emit-body body))
   (println (str *indent* "}"))))

(defn- needs-loop-target? [body]
  (let [recur-found? (atom false)]
    (w/prewalk #(if (seq? %)
                  (case (first %)
                    recur (do (reset! recur-found? true)
                              %)
                    loop nil
                    %)
                  %) body)
    @recur-found?))

(defn- emit-function [[op f args & body :as form]]
  (binding [*lambda-state* (atom [])]
    (let [fn-source (with-out-str
                      (print (str (form->tag args "void")
                                  " "
                                  f
                                  (str "("
                                       (->> args
                                            (map #(str (form->tag %) " " %))
                                            (str/join ", "))
                                       ") {")))
                      (binding [*indent* (str *indent* default-indent)]
                        (if (needs-loop-target? body)
                          (binding [*loop-state* {:label (gensym f)
                                                  :vars args}]
                            (println)
                            (println (str (:label *loop-state*) ":"))
                            (emit-block body))
                          (do (println)
                              (emit-body body))))
                      (println "}"))]
      (doseq [[symbol lambda] @*lambda-state*]
        (emit-function (concat ['lambda symbol]
                               (rest lambda))))
      (println fn-source))))

(defn- emit-struct [[_ name fields :as form]]
  (println "typedef struct {")
  (doseq [field fields]
    (print (form->tag field) " " field ";"))
  (println "} " name ";")
  (println))

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
