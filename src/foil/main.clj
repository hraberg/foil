(ns foil.main
  (:require [clojure.tools.reader :as r]
            [clojure.tools.reader.reader-types :as rt]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.walk :as w])
  (:gen-class))

(defn- read-source [in]
  (let [r (rt/indexing-push-back-reader (io/reader in) 2)]
    (binding [r/*read-eval* false]
      (->> (repeatedly #(r/read {:eof ::eof} r))
           (take-while #(not= ::eof %))
           (vec)))))

(def ^:private default-tag "auto")

(defn- form->tag
  ([form]
   (form->tag form default-tag))
  ([form default]
   (cond-> (:tag (meta form) default)
     (or (vector? form)
         (map? form)) (-> (name) (str/replace #"s$" "") (symbol)))))

(defn- emit-include [[_ header]]
  (println (str "#include " (if (symbol? header)
                              (str "<" header ">")
                              (pr-str header)))))

(defn- emit-headers [[_ name & references :as form]]
  (doseq [[ref-type & lib-specs] references
          [lib :as lib-spec] lib-specs]
    (case ref-type
      :use
      (emit-include ['include lib])))
  (println))

(def ^:private default-indent "  ")
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
                           set! =
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

(declare emit-block emit-expression-statement emit-expression emit-expression-in-lambda emit-function-body)

(defn- constructor? [f]
  (and (symbol? f)
       (or (str/ends-with? (name f) ".")
           (str/starts-with? (name f) "->"))))

(defn- field-access? [f]
  (and (symbol? f)
       (str/starts-with? (name f) ".-")))

(defn- method-call? [f]
  (and (symbol? f)
       (str/starts-with? (name f) ".")))

(defn- emit-array-access [[array & indexes]]
  (emit-expression array)
  (doseq [idx indexes]
    (print "[")
    (emit-expression idx)
    (print "]")))

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

    (= 'aget f)
    (emit-array-access args)

    (= 'aset f)
    (do (emit-array-access (butlast args))
        (print " = ")
        (emit-expression (last args)))

    (keyword? f)
    (emit-array-access [(first args) (name f)])

    (field-access? f)
    (do (emit-expression (first args))
        (print ".")
        (emit-expression (symbol (str/replace (name f) #"^\.-" ""))))

    (constructor? f)
    (do (emit-expression (symbol (-> (name f)
                                     (str/replace #"\.$" "")
                                     (str/replace #"^->" ""))))
        (print (str "{" (str/join ", " (map #(with-out-str
                                               (emit-expression %)) args)) "}")))

    (method-call? f)
    (do (emit-expression (first args))
        (emit-expression f)
        (print (str "(" (str/join ", " (map #(with-out-str
                                               (emit-expression %)) (rest args))) ")")))

    :else
    (do (emit-expression f)
        (print (str "(" (str/join ", " (map #(with-out-str
                                               (emit-expression %)) args)) ")")))))

(defn- emit-return [[_ value :as from]]
  (print (str *indent* "return "))
  (emit-expression value)
  (println ";"))

(defn- emit-while [[_ condition & body :as form]]
  (print (str *indent* "while ("))
  (emit-expression condition)
  (print ")")
  (emit-block body " "))

(defn- emit-var-declaration
  ([var]
   (emit-var-declaration var default-tag))
  ([var default-tag]
   (print (if (string? var)
            var
            (str (form->tag var default-tag) " " (munge var))))))

(defn- emit-conditional [[_ condition then else :as form]]
  (emit-expression condition)
  (print " ? ")
  (emit-expression-in-lambda then)
  (print " : ")
  (emit-expression-in-lambda else))

(defn- emit-assignment [[_ var value :as from]]
  (print (str (munge var) " = "))
  (emit-expression value))

(def ^:dynamic ^:private *loop-state*)

(defn- emit-goto [[_ & expressions :as form]]
  (assert *loop-state* "Not in a loop.")
  (doseq [[var expression] (map vector (:vars *loop-state*) expressions)]
    (print (str *indent* (munge var) " = "))
    (emit-expression expression)
    (println ";"))
  (println (str *indent* "goto "(:label *loop-state*) ";")))

(defn- emit-lambda [[op args & body :as form]]
  (print (str "[&] ("
              (if (empty? args)
                "void"
                (->> args
                     (map #(with-out-str
                             (emit-var-declaration %)))
                     (str/join ", ")))
              ") {"))
  (emit-function-body "lambda" args body)
  (print (str *indent* "}"))  )

(defmulti foil-macroexpand (fn [[op :as form]] (when (symbol? op)
                                                 (keyword (name op)))))

(defn- macroexpand-let [[_ bindings & body :as form]]
  (let [bindings (partition 2 bindings)]
    (with-meta
      `((~'lambda ~(mapv first bindings)
         ~@body)
        ~@(map second bindings))
      (meta form))))

(defmethod foil-macroexpand :let [form]
  (macroexpand-let form))

(defmethod foil-macroexpand :loop [form]
  (macroexpand-let form))

(defmethod foil-macroexpand :default [form])

(defn- emit-expression [form]
  (when-let [tag (and (not (or (vector? form)
                               (map? form)))
                      (form->tag form nil))]
    (print (str "(" tag ") ")))
  (cond
    (seq? form)
    (let [op (first form)]
      (case op
        if (emit-conditional form)
        (fn, lambda) (emit-lambda form)
        (if-let [macro-expansion (foil-macroexpand form)]
          (emit-expression macro-expansion)
          (emit-application form))))

    (symbol? form)
    (print (munge form))

    (map? form)
    (print (str "std::unordered_map<std::string," (form->tag form) ">"
                "{" (str/join ", " (map (fn [[k v]]
                                          (str "{"
                                               (pr-str (name k))
                                               ", "
                                               (with-out-str
                                                 (emit-expression v))
                                               "}")) form)) "}"))

    (vector? form)
    (print (str "std::vector<" (form->tag form) ">"
                "{" (str/join ", " (map #(with-out-str
                                           (emit-expression %)) form)) "}"))

    :else
    (pr form)))

(def ^:dynamic ^:private *file-name* nil)

(defn- emit-line [form]
  (when-let [line (:line (meta form))]
    (println "#line" line (or (some-> *file-name* (pr-str)) ""))))

(defn- emit-expression-statement [[op :as form]]
  (emit-line form)
  (case op
    return (emit-return form)
    while (emit-while form)
    recur (emit-goto form)
    (do (print *indent*)
        (emit-expression form)
        (println ";"))))

(defn- constant? [form]
  (not (and (seq? form)
            (not= 'quote (first form)))))

(defn- emit-expression-in-lambda [form]
  (if (constant? form)
    (emit-expression form)
    (do (println "[&] () {")
        (binding [*indent* (str *indent* default-indent)]
          (emit-expression-statement (with-meta
                                       (list 'return form)
                                       (meta form))))
        (print (str *indent* "}()")))))

(defn- emit-body
  ([body]
   (emit-body body false))
  ([body return?]
   (loop [[x & xs] body]
     (when x
       (emit-expression-statement (if (and (not xs) return?)
                                    (with-meta
                                      (list 'return x)
                                      (meta x))
                                    x))
       (recur xs)))))

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

(defn- emit-function-body [f args body]
  (binding [*indent* (str *indent* default-indent)]
    (if (needs-loop-target? body)
      (binding [*loop-state* {:label (gensym f)
                              :vars args}]
        (println)
        (println (str (:label *loop-state*) ":"))
        (emit-block body))
      (do (println)
          (emit-body body true)))))

(defn- emit-function [[op f args & body :as form]]
  (print (str (form->tag args "void")
              " "
              f
              (str "("
                   (if (empty? args)
                     "void"
                     (->> args
                          (map #(with-out-str
                                  (emit-var-declaration %)))
                          (str/join ", ")))
                   ") {")))
  (emit-function-body f args body)
  (println "}"))

(defn- emit-struct [[_ name fields :as form]]
  (println "typedef struct {")
  (binding [*indent* (str *indent* default-indent)]
    (doseq [field fields]
      (println (str *indent* (form->tag field) " " field ";"))))
  (println (str "} " name ";"))
  (println))

(defn- emit-default-includes []
  (doseq [header '[string vector unordered_map]]
    (emit-include (vector 'include header))))

(defn- emit-source [in out]
  (binding [*out* out]
    (emit-default-includes)
    (doseq [[top-level :as form] (read-source in)]
      (emit-line form)
      (case top-level
        ns (emit-headers form)
        (include, use) (emit-include form)
        (defn, defun) (emit-function form)
        (defrecord, defstruct) (emit-struct form)))))

(defn -main [& args]
  (emit-source *in* *out*))
