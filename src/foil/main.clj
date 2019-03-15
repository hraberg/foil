(ns foil.main
  (:require [clojure.tools.reader :as r]
            [clojure.tools.reader.reader-types :as rt]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.walk :as w])
  (:import java.util.regex.Pattern)
  (:gen-class))

(defn- read-source [in]
  (let [r (rt/indexing-push-back-reader (io/reader in) 2)]
    (binding [r/*read-eval* false]
      (->> (repeatedly #(r/read {:eof ::eof} r))
           (take-while #(not= ::eof %))
           (vec)))))

(def ^:private default-tag 'auto)

(defn- collection-literal? [form]
  (or (vector? form)
      (map? form)
      (set? form)
      (seq? form)))

(defn- munge-name [n]
  (str/replace (munge n) "_COLON_" ":"))

(defn- form->tag
  ([form]
   (form->tag form default-tag))
  ([form default]
   (cond-> (:tag (meta form) default)
     (collection-literal? form) (-> (name) (str/replace #"s$" "") (symbol)))))

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
  (println)
  (println (str "namespace " name  " {")))

(def ^:private default-indent "  ")
(def ^:dynamic ^:private *indent* "")

(def ^:dynamic ^:private *tail?* false)
(def ^:dynamic ^:private *expr?* false)
(def ^:dynamic ^:private *return-type* default-tag)

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

(declare emit-block emit-expression-statement emit-expression emit-expression-in-lambda emit-function-body emit-variable-definition)

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
  (binding [*tail?* false
            *expr?* true]
    (cond
      (contains? unary-op f)
      (do (print (get unary-op f))
          (emit-expression (first args)))

      (contains? unary-inc-dec-op f)
      (do (emit-expression (first args))
          (print (str " " (get unary-inc-dec-op f) " 1")))

      (contains? binary-op f)
      (do (print "(")
          (emit-expression (first args))
          (print (str " " (get binary-op f)  " "))
          (emit-expression (second args))
          (print ")"))

      (= 'aget f)
      (emit-array-access args)

      (= 'aset f)
      (do (emit-array-access (butlast args))
          (print " = ")
          (emit-expression (last args)))

      (keyword? f)
      (emit-array-access [(first args) f])

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
                                                 (emit-expression %)) args)) ")"))))))

(defn- emit-return [[_ value :as from]]
  (print (str *indent* "return "))
  (binding [*expr?* true]
    (emit-expression value))
  (println ";"))

(defn- emit-var-declaration
  ([var]
   (emit-var-declaration var default-tag))
  ([var default-tag]
   (when (:const (meta var))
     (print "const "))
   (print (if (string? var)
            var
            (str (form->tag var default-tag) " " (munge-name var))))))

(defn- emit-while [[_ condition & body :as form]]
  (binding [*tail?* false]
    (print (str *indent* "while ("))
    (emit-expression condition)
    (print ")")
    (emit-block body " ")))

(defn- emit-range-based-for [[op bindings & body]]
  (binding [*tail?* false]
    (doseq [[var binding] (partition 2 bindings)]
      (println (str *indent* "for (" (with-out-str
                                       (emit-var-declaration var "auto&")) " : "
                    (binding [*expr?* true]
                      (with-out-str
                        (emit-expression binding)))
                    ")")))
    (emit-block body)))

(defn- emit-classic-for [[op bindings & body]]
  (doseq [[var limit] (partition 2 bindings)]
    (println (str *indent* "for (int " var " = 0; " var " < " limit "; " var "++)")))
  (emit-block body))

(defn- emit-conditional [[_ condition then else :as form]]
  (binding [*expr?* true]
    (binding [*tail?* false]
      (emit-expression condition))
    (print " ? ")
    (emit-expression-in-lambda then)
    (print " : ")
    (emit-expression-in-lambda else)))

(defn- emit-if [[_ condition then else :as form]]
  (print (str *indent* "if ("))
  (binding [*tail?* false
            *expr?* true]
    (emit-expression condition))
  (println ") {")
  (binding [*indent* (str *indent* default-indent)]
    (emit-expression-statement then))
  (when else
    (println (str *indent* "} else {"))
    (binding [*indent* (str *indent* default-indent)]
      (emit-expression-statement else)))
  (println (str *indent* "}")))

(defn- emit-assignment [[_ var value :as from]]
  (print (str (munge-name var) " = "))
  (binding [*expr?* true
            *tail?* false]
    (emit-expression value)))

(def ^:dynamic ^:private *loop-state*)

(defn- emit-goto [[_ & expressions :as form]]
  (assert *loop-state* "Not in a loop.")
  (assert *tail?* "Can only recur from tail position.")
  (doseq [[var expression] (map vector (:vars *loop-state*) expressions)]
    (print (str *indent* (munge-name var) " = "))
    (binding [*expr?* true
              *tail?* false]
      (emit-expression expression))
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
  (binding [*return-type* 'auto]
    (emit-function-body "lambda" args body))
  (print (str *indent* "}"))  )

(defmulti foil-macroexpand (fn [[op :as form]] (when (symbol? op)
                                                 (keyword (name op)))))

(defn- macroexpand-let [[_ bindings & body :as form]]
  (let [bindings (partition 2 bindings)
        loop? (needs-loop-target? body)]
    (if (or *expr?* loop?)
      (with-meta
        `((~'lambda ~(mapv first bindings)
           ~@body)
          ~@(map second bindings))
        (meta form))
      (with-meta
        `(~'do
          ~@(for [[var binding] bindings]
              `(~'def ~var ~binding))
          ~@body)
        (meta form)))))

(defmethod foil-macroexpand :let [form]
  (macroexpand-let form))

(defmethod foil-macroexpand :loop [form]
  (macroexpand-let form))

(defmethod foil-macroexpand :do [[_ & body :as form]]
  `((~'lambda [] ~@body)))

(defmethod foil-macroexpand :begin [[_ & body :as form]]
  `((~'lambda [] ~@body)))

(defmethod foil-macroexpand :default [form])

(defn- literal? [form]
  (not (seq? form)))

(def ^:private ^:dynamic *quote?* false)

(defn- emit-literal [form]
  (cond
    (or (string? form)
        (keyword? form))
    (print (str "std::string(u8" (pr-str (str form)) ")"))

    (symbol? form)
    (print (munge-name form))

    (instance? Pattern form)
    (print (str "std::regex(" (pr-str (str form)) ")"))

    (inst? form)
    (print (str "std::chrono::time_point<std::chrono::system_clock>(std::chrono::milliseconds(" (inst-ms form) "))"))

    (map? form)
    (print (str "std::map<std::string," (form->tag form) ">"
                "{" (str/join ", " (map (fn [[k v]]
                                          (str "{"
                                               (with-out-str
                                                 (emit-expression k))
                                               ", "
                                               (with-out-str
                                                 (emit-expression v))
                                               "}")) form)) "}"))

    (or (seq? form)
        (set? form)
        (vector? form))
    (print (str "std::" (cond
                          (seq? form) "forward_list"
                          (set? form) "set"
                          (vector? form) "vector") "<"
                (form->tag form) ">"
                "{" (str/join ", " (map #(with-out-str
                                           (emit-expression %)) form)) "}"))

    :else
    (pr form)))

(defn- emit-expression [form]
  (when-let [tag (and (not (collection-literal? form))
                      (form->tag form nil))]
    (print (str "(" tag ") ")))
  (if (or (literal? form) *quote?*)
    (emit-literal form)
    (let [op (first form)]
      (case op
        quote (binding [*quote?* true]
                (emit-literal (with-meta
                                (second form)
                                (meta form))))
        if (emit-conditional form)
        (fn, lambda, λ) (emit-lambda form)
        (if (and (contains? '#{do begin} op)
                 (not *expr?*))
          (emit-block (rest form))
          (if-let [macro-expansion (foil-macroexpand form)]
            (emit-expression macro-expansion)
            (emit-application form)))))))

(def ^:dynamic ^:private *file-name* nil)

(defn- emit-line [form]
  (when-let [line (:line (meta form))]
    (println "#line" line (or (some-> *file-name* (pr-str)) ""))))

(defn- emit-expression-statement [form]
  (emit-line form)
  (case (if (seq? form)
          (first form)
          :literal)
    return (emit-return form)
    while (emit-while form)
    (if, when) (emit-if form)
    recur (emit-goto form)
    doseq (emit-range-based-for form)
    dotimes (emit-classic-for form)
    def (emit-variable-definition form)
    (if (and *tail?* (not= 'void *return-type*))
      (emit-return (list 'return form))
      (do (print *indent*)
          (emit-expression form)
          (println ";")))))

(defn- emit-expression-in-lambda [form]
  (if (or (literal? form) (not *expr?*))
    (emit-expression form)
    (do (println "[&] () {")
        (binding [*indent* (str *indent* default-indent)
                  *expr?* false
                  *tail?* true]
          (emit-expression-statement form))
        (print (str *indent* "}()")))))

(defn- emit-body [body]
  (loop [[x & xs] body]
    (when x
      (let [last? (not xs)]
        (binding [*expr?* false
                  *tail?* (and *tail?* last?)]
          (emit-expression-statement x)))
      (recur xs))))

(defn- emit-block
  ([body]
   (emit-block body  *indent*))
  ([body initial-indent]
   (println (str initial-indent "{"))
   (binding [*indent* (str *indent* default-indent)]
     (emit-body body))
   (println (str *indent* "}"))))

(defn- emit-function-body [f args body]
  (binding [*indent* (str *indent* default-indent)
            *tail?* true]
    (if (needs-loop-target? body)
      (binding [*loop-state* {:label (gensym f)
                              :vars args}]
        (println)
        (println (str (:label *loop-state*) ":"))
        (emit-block body))
      (do (println)
          (emit-body body)))))

(defn- emit-function [[op f args & body :as form]]
  (binding [*return-type* (form->tag args 'void)]
    (let [main? (= '-main f)
          return-template-name (if main?
                                 *return-type*
                                 (gensym "Return"))
          arg-template-names (if main?
                               (map form->tag args)
                               (repeatedly (count args) #(gensym "Arg")))]
      (when-not main?
        (println (str *indent*
                      "template <"
                      (str/join ", "
                                (for [[tn tt] (conj (vec (for [[arg-tn arg] (map vector arg-template-names args)]
                                                           [arg-tn (form->tag arg nil)]))
                                                    [return-template-name *return-type*])]
                                  (cond-> (str "typename " tn)
                                    tt (str " = " tt))))
                      ">")))
      (print (str *indent*
                  (if main?
                    *return-type*
                    return-template-name)
                  " "
                  (munge-name f)
                  (str "("
                       (->> (for [[arg-tn arg] (map vector arg-template-names args)]
                              (with-out-str
                                (emit-var-declaration (if (form->tag arg nil)
                                                        arg
                                                        (vary-meta arg assoc :tag arg-tn)))))
                            (str/join ", "))
                       ") {"))))
    (emit-function-body f args body)
    (println (str *indent* "}"))))

(defn- emit-struct [[_ name fields :as form]]
  (print *indent*)
  (println "typedef struct {")
  (binding [*indent* (str *indent* default-indent)]
    (doseq [field fields]
      (println (str *indent* (form->tag field) " " (munge-name field) ";"))))
  (println (str *indent* "} " (munge-name name) ";"))
  (println))

(defn- emit-variable-definition [[_ name value :as form]]
  (print *indent*)
  (emit-var-declaration name)
  (print " = ")
  (emit-expression value)
  (println ";"))

(defn- emit-default-includes []
  (doseq [header '[vector map set regex chrono forward_list]]
    (emit-include (vector 'include header))))

(defn- emit-source [in out]
  (binding [*out* out]
    (emit-default-includes)
    (let [ns (atom nil)
          main (atom false)]
      (doseq [[top-level :as form] (read-source in)]
        (emit-line form)
        (binding [*indent* (if @ns
                             default-indent
                             "")]
          (case top-level
            ns (do (assert (nil? @ns) "Only one namespace supported.")
                   (reset! ns (second form))
                   (emit-headers form))
            (include, use) (emit-include form)
            (def, define) (do (print *indent*)
                              (emit-variable-definition form))
            (defn, defun) (do (when (= '-main (second form))
                                (reset! main form))
                              (emit-function form))
            (defrecord, defstruct) (emit-struct form))))
      (when @ns
        (println "}"))
      (when @main
        (println "int main(int argc, char** argv) {")
        (let [main-args? (empty? (nth @main 2))]
          (when main-args?
            (println (str default-indent "std::vector<std::string> args(argv + 1, argv + argc);")))
          (println (str default-indent (format "return %s_main(%s);"
                                               (some-> @ns (str "::"))
                                               (if main-args?
                                                 ""
                                                 "args")))))
        (println "}")))))

(defn -main [& args]
  (emit-source *in* *out*))
