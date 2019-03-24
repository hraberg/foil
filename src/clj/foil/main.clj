(ns foil.main
  (:require [clojure.tools.reader :as r]
            [clojure.tools.reader.reader-types :as rt]
            [clojure.string :as str]
            [clojure.walk :as w])
  (:import java.util.regex.Pattern)
  (:gen-class))

(defn- read-source [in]
  (let [r (rt/indexing-push-back-reader in 2)]
    (binding [r/*read-eval* false]
      (->> (repeatedly #(r/read {:eof ::eof} r))
           (take-while #(not= ::eof %))
           (vec)))))

(def ^:private default-tag 'auto)

(defn- munge-name [n]
  (str/replace (munge n) "_COLON_" ":"))

(defn- munge-ns [n]
  (munge-name (str/replace n "." "::")))

(defn- form->tag
  ([form]
   (form->tag form default-tag))
  ([form default]
   (:tag (meta form) default)))

(defn- ns->ns-parts [ns]
  (str/split (str ns) #"\."))

(defn- ns->header-define [ns]
  (str (str/upper-case (munge-ns (str/join "_" (ns->ns-parts ns))))))

(defn- emit-include [[_ header]]
  (println (str "#include " (if (symbol? header)
                              (str "<" header ">")
                              (pr-str header)))))

(defn- emit-using [[_ ns]]
  (emit-include ['include (str (str/join "/" (ns->ns-parts ns)) ".hpp")])
  (println (str "using namespace " (munge-ns ns) ";")))

(defn- emit-headers [[_ ns-name & references :as form]]
  (doseq [[ref-type & lib-specs] references
          [lib :as lib-spec] lib-specs]
    (case ref-type
      (:require :include)
      (emit-include ['include lib])
      (:use :using)
      (emit-using ['using lib])))
  (println)
  (let [ns-parts (ns->ns-parts ns-name)
        ns-header-define (ns->header-define ns-name)]
    (println (str/join " "
                       (for [part ns-parts]
                         (str "namespace " (munge-ns part)  " {"))))))

(def ^:private default-indent "    ")
(def ^:dynamic ^:private *indent* "")

(def ^:dynamic ^:private *tail?* false)
(def ^:dynamic ^:private *expr?* false)
(def ^:dynamic ^:private *return-type* default-tag)

(def ^:private unary-op '{not !
                          ! !
                          bit-not "~"
                          - -
                          & &
                          * *})

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
                           >> >>})

(def ^:private fn-replacements '{clojure.core/deref deref})

(declare emit-block emit-expression-statement emit-expression emit-expression-in-lambda
         emit-function-body emit-variable-definition foil-macroexpand emit-if)

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

(defn- maybe-make-ref [p]
  (if (string? p)
    p
    (vary-meta p assoc :ref (not (:val (meta p))))))

(defn- maybe-template-params [form]
  (let [tag (form->tag form nil)]
    (if (vector? tag)
      (str/join "," tag)
      tag)))

(defn- emit-application [[f & args :as form]]
  (binding [*tail?* false
            *expr?* true]
    (cond
      (and (contains? unary-op f)
           (= 1 (count args)))
      (do (print (str "(" (get unary-op f)))
          (emit-expression (first args))
          (print ")"))

      (and (contains? unary-inc-dec-op f)
           (= 1 (count args)))
      (do (print "(")
          (emit-expression (first args))
          (print (str " " (get unary-inc-dec-op f) " 1)")))

      (contains? binary-op f)
      (do (assert (>= (count args) 2) "Requires minimum two operands.")
          (print "(")
          (print (str/join (str " " (get binary-op f)  " ")
                           (for [arg args]
                             (with-out-str
                               (emit-expression arg)))))
          (print ")"))

      (= 'aget f)
      (emit-array-access args)

      (= 'aset f)
      (do (emit-array-access (butlast args))
          (print " = ")
          (emit-expression (last args)))

      (and (keyword? f)
           (= 1 (count args)))
      (emit-array-access [(first args) f])

      (and (field-access? f)
           (= 1 (count args)))
      (do (emit-expression (first args))
          (print ".")
          (emit-expression (symbol (str/replace (name f) #"^\.-" ""))))

      (constructor? f)
      (do (emit-expression (symbol (-> (name f)
                                       (str/replace #"\.$" "")
                                       (str/replace #"^->" ""))))
          (when-let [tag (maybe-template-params form)]
            (print (str "<" tag ">")))
          (print (str "{" (str/join ", " (map #(with-out-str
                                                 (emit-expression %)) args)) "}")))

      (method-call? f)
      (do (emit-expression (first args))
          (emit-expression f)
          (when-let [tag (maybe-template-params form)]
            (print (str "<" tag ">")))
          (print (str "(" (str/join ", " (map #(with-out-str
                                                 (emit-expression %)) (rest args))) ")")))

      :else
      (let [f (get fn-replacements f f)]
        (emit-expression f)
        (when-let [tag (maybe-template-params form)]
          (when-not (re-find #"::" (str f))
            (println ".operator()"))
          (print (str "<" tag ">")))
        (print (str "(" (str/join ", " (map #(with-out-str
                                                 (emit-expression %)) args)) ")"))))))

(defn- emit-return [[_ value :as from]]
  (let [maybe-expanded (when (and (seq? value)
                                  (not *expr?*))
                         (foil-macroexpand value))]
    (if (= 'do (first maybe-expanded))
      (emit-expression maybe-expanded)
      (do (print (str *indent* "return "))
          (binding [*expr?* true]
            (emit-expression value))
          (println ";")))))

(defn- emit-var-declaration
  ([var]
   (emit-var-declaration var default-tag))
  ([var default-tag]
   (let [{:keys [const dynamic val ref & mut !]} (meta var)
         tag (form->tag var default-tag)
         ref? (and (or ref &)
                   (not val)
                   (not (re-find #"\&" (str tag))))]
     (when (= '_ var)
       (print "__attribute__((unused)) "))
     (when (or const (not (or mut ! dynamic)))
       (print "const "))
     (when dynamic
       (print "thread_local "))
     (print (if (string? var)
              var
              (str tag
                   (when ref?
                     "&")
                   " " (if (vector? var)
                         (str "[" (str/join ", " (mapv munge-name var)) "]")
                         (munge-name var))))))))

(defn- emit-if [[_ condition then else :as form]]
  (print (str *indent* "if ("))
  (binding [*expr?* true
            *tail?* false]
    (emit-expression condition))
  (println ") {")
  (binding [*indent* (str *indent* default-indent)]
    (emit-expression-statement then))
  (when-not (nil? else)
    (println (str *indent* "} else {"))
    (binding [*indent* (str *indent* default-indent)]
      (emit-expression-statement else)))
  (println (str *indent* "}")))

(defn- emit-assignment [[_ var value :as from]]
  (print (str (munge-name var) " = "))
  (binding [*expr?* true
            *tail?* false]
    (emit-expression value)))

(def ^:dynamic ^:private *loop-vars*)

(defn- emit-recur [[_ & expressions :as form]]
  (assert *loop-vars* "Not in a loop.")
  (assert *tail?* "Can only recur from tail position.")
  (doseq [[var expression] (map vector *loop-vars* expressions)]
    (print (str *indent* (munge-name var) " = "))
    (binding [*expr?* true
              *tail?* false]
      (emit-expression expression))
    (println ";")))

(defn- $code [f]
  `(~'$code
    ~(with-out-str
       (f))))

(defmulti foil-macroexpand (fn [[op :as form]] (when (symbol? op)
                                                 (keyword (name op)))))

(defmethod foil-macroexpand :fn [[_ args & body :as form]]
  ($code
   (fn []
     (print (str "[=] ("
                 (->> args
                      (map #(with-out-str
                              (emit-var-declaration %)))
                      (str/join ", "))
                 ") {"))
     (binding [*return-type* 'auto]
       (emit-function-body "lambda" args body))
     (print (str *indent* "}")))))

(defmethod foil-macroexpand :fn* [[_ args & body :as form]]
  `(fn ~args ~@body))

(defmethod foil-macroexpand :λ [[_ args & body :as form]]
  `(fn ~args ~@body))

(defmethod foil-macroexpand :let [[_ bindings & body :as form]]
  (let [bindings (partition 2 bindings)]
    (with-meta
      `(~'do
        ~@(for [[var binding] bindings]
            `(~'def ~var ~binding))
        ~@body)
      (meta form))))

(defmethod foil-macroexpand :loop [[_ bindings & body :as form]]
  (let [bindings (partition 2 bindings)]
    (with-meta
      `((~'fn ~(vec (for [[var] bindings]
                      (cond-> var
                        (not (string? var)) (vary-meta assoc :mut true))))
         ~@body)
        ~@(map second bindings))
      (meta form))))

(defmethod foil-macroexpand :binding [[op bindings & body :as form]]
  (with-meta
    `(~'do
      ~@(for [[var v-binding] (partition 2 bindings)
              :let [old-binding (gensym "__old_binding")]]
          ($code
           #(do (emit-variable-definition ['def old-binding var] "")
                (binding [*indent* (str *indent* default-indent)]
                  (print *indent*)
                  (println (format "auto %s = std::unique_ptr<decltype(%s), std::function<void(decltype(%s)*)>>(&%s, [](auto old) { %s = *old; });"
                                   (gensym "__binding_guard")
                                   old-binding
                                   old-binding
                                   old-binding
                                   (munge-name var)))
                  (print *indent*)
                  (emit-assignment ['set! var v-binding])))))
      ~@body)
    (meta form)))

(defn- macroexpand-do [[_ & body :as form]]
  (when (seq body)
    (if (= 1 (count body))
      (first body)
      `((~'fn ^:no-loop [] ~@body)))))

(defmethod foil-macroexpand :do [form]
  (macroexpand-do form))

(defmethod foil-macroexpand :if [[_ condition then else :as form]]
  ($code
   #(if *expr?*
      (binding [*expr?* true]
        (binding [*tail?* false]
          (emit-expression condition))
        (print " ? ")
        (emit-expression then)
        (print " : ")
        (emit-expression (if (nil? else)
                           'nullptr
                           else)))
      (do (println)
          (emit-if form)))))

(defmethod foil-macroexpand :if-not [[_ condition then else]]
  `(~'if (~'not ~condition) ~then ~else))

(defmethod foil-macroexpand :if-let [[_ binding then else]]
  `(~'let ~binding (~'if ~(first binding) ~then ~else)))

(defmethod foil-macroexpand :when [[_ condition & then]]
  `(~'if ~condition (~'do ~@then)))

(defmethod foil-macroexpand :when-not [[_ condition & then]]
  `(~'when (~'not ~condition) ~@then))

(defmethod foil-macroexpand :when-let [[_ binding & then]]
  `(~'let ~binding (~'when ~(first binding) ~@then)))

(defmethod foil-macroexpand :cond [[_ condition then & rest :as form]]
  (if then
    `(~'if ~(if (= :else condition)
              true
              condition)
      ~then
      (~'cond
       ~@rest))
    'nullptr))

(defmethod foil-macroexpand :case [[_ e & clauses :as form]]
  (let [e-sym (gensym 'case-e)]
    `(~'let [~e-sym ~e]
      (~'cond
       ~@(apply concat
                (for [[constant expr] (partition 2 clauses)]
                  `[~(if (seq? constant)
                       `(~'or ~@(for [constant constant]
                                  `(~'= ~'case-e ~constant)))
                       `(~'= ~'case-e ~constant))
                    ~expr]))))))

(defmethod foil-macroexpand :while [[_ condition & body :as form]]
  (if *expr?*
    `((~'fn ^:no-loop [] ~form ~'nullptr))
    ($code
     #(binding [*expr?* false
                *tail?* false]
        (print "while (")
        (binding [*expr?* true]
          (emit-expression condition))
        (print ")")
        (emit-block body " ")))))

(defmethod foil-macroexpand :dotimes [[_ bindings & body :as form]]
  (if *expr?*
    `((~'fn ^:no-loop [] ~form ~'nullptr))
    ($code
     #(binding [*expr?* false
                *tail?* false]
        (doseq [[var limit] (partition 2 bindings)]
          (println (str "for (int " var " = 0; " var " < "
                        (binding [*expr?* true]
                          (with-out-str
                            (emit-expression limit))) "; " var "++)")))
        (emit-block body)))))

(defmethod foil-macroexpand :doseq [[_ bindings & body :as form]]
  (if *expr?*
    `((~'fn ^:no-loop [] ~form ~'nullptr))
    ($code
     #(binding [*expr?* false
                *tail?* false]
        (doseq [[[var v-binding] indent] (map vector (partition 2 bindings) (cons "" (repeat *indent*)))]
          (println (str indent "for (" (with-out-str
                                         (emit-var-declaration (maybe-make-ref var) 'auto)) " : "
                        (binding [*expr?* true]
                          (with-out-str
                            (emit-expression v-binding)))
                        ")")))
        (emit-block body)))))

(defmethod foil-macroexpand :doto [[_ x & forms]]
  `(do
     ~@(for [[y & ys] forms]
         (concat [y x] ys))
     ~x))

(defmethod foil-macroexpand :-> [[_ x & forms]]
  (reduce
   (fn [x [y & ys]]
     (concat [y x] ys))
   x forms))

(defmethod foil-macroexpand :->> [[_ x & forms]]
  (reduce
   (fn [x ys]
     (concat ys [x]))
   x forms))

(defmethod foil-macroexpand :default [form]
  form)

(defn- literal? [form]
  (or (not (seq? form)) (= () form)))

(def ^:private ^:dynamic *quote?* false)

(defn- re? [x]
  (instance? Pattern x))

(defn- emit-literal [form]
  (binding [*tail?* false]
    (cond
      (or (string? form)
          (keyword? form))
      (print (str "std::string(u8" (pr-str (str form)) ")"))

      (symbol? form)
      (print (munge-name form))

      (re? Pattern)
      (print (str "std::regex(" (pr-str (str form)) ")"))

      (inst? form)
      (print (str "std::chrono::time_point<std::chrono::system_clock>(std::chrono::milliseconds(" (inst-ms form) "))"))

      (map? form)
      (let [tag (form->tag form)
            tag (cond
                  (string? tag)
                  tag

                  (vector? tag)
                  (str/join "," tag)

                  :else
                  (str "std::string," tag))]
        (print (str "std::unordered_map<" tag ">"
                    "{" (str/join ", " (map (fn [[k v]]
                                              (str "{"
                                                   (with-out-str
                                                     (emit-expression k))
                                                   ", "
                                                   (with-out-str
                                                     (emit-expression v))
                                                   "}")) form)) "}")))

      (or (seq? form)
          (set? form)
          (vector? form))
      (print (str "std::" (cond
                            (seq? form) "forward_list"
                            (set? form) "unordered_set"
                            (vector? form) "vector")
                  "<" (form->tag form) ">"
                  "{" (str/join ", " (map #(with-out-str
                                             (emit-expression %)) form)) "}"))

      :else
      (pr form))))

(defn- emit-code [[op & body]]
  (print (str/join body)))

(defn- emit-expression [form]
  (let [needs-return? (and *tail?*
                           (not= 'void *return-type*)
                           (not (and (seq? form)
                                     (= 'return (first form)))))]
    (if (or (literal? form) *quote?*)
      (do (when needs-return?
            (print "return "))
          (emit-literal form))
      (let [op (first form)]
        (cond
          (contains? '#{$code $} op)
          (emit-code form)

          (= 'quote op)
          (binding [*quote?* true]
            (when needs-return?
              (print "return "))
            (emit-literal (with-meta
                            (second form)
                            (meta form))))

          (and (= 'do op)
               (not *expr?*))
          (emit-block (next form) "")

          :else
          (let [macro-expansion (foil-macroexpand form)]
            (cond
              (= form macro-expansion)
              (do (when needs-return?
                    (print "return "))
                  (emit-application form))

              (not (nil? macro-expansion))
              (do (when (and needs-return?
                             (seq? form)
                             (= 'fn (first form)))
                    (print "return "))
                  (emit-expression macro-expansion)))))))))

(def ^:dynamic ^:private *file-name* nil)

(defn- emit-line [form]
  (when-let [line (:line (meta form))]
    (print "#line" line (or (some-> *file-name* (pr-str)) ""))
    (println (str " // " (pr-str form)))))

(defn- emit-expression-statement [form]
  (emit-line form)
  (case (if (seq? form)
          (first form)
          ::literal)
    return (emit-return form)
    recur (emit-recur form)
    def (emit-variable-definition form)
    (binding [*expr?* false]
      (print *indent*)
      (emit-expression form)
      (println ";"))))

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
    (if (and (not (:no-loop (meta args)))
             (needs-loop-target? body))
      (binding [*loop-vars* args]
        (println)
        (emit-block body (str *indent* "while (true) ")))
      (do (println)
          (emit-body body)))))

(defn- emit-template [parameters template-names]
  (if-let [template (:tpl (meta parameters))]
    (let [template (if (vector? template)
                     (str "<" (->> (for [tn template]
                                     (str "typename " tn (when-let [tag (:tag (meta tn))]
                                                           (str " = " tag))))
                                   (str/join  ", "))
                          ">")
                     template)]
      (println (str *indent* "template " template)))
    (let [template-parameters (for [[tn tt] (conj (vec (for [[tn p] (map vector template-names parameters)]
                                                         [tn (form->tag p nil)])))
                                    :when (not tt)]
                                tn)]
      (when (seq template-parameters)
        (println (str *indent*
                      "template <"
                      (str/join ", "
                                (for [tt template-parameters]
                                  (str "typename " tt)))
                      ">"))))))

(defn- maybe-add-template-name [p tn]
  (if (string? p)
    p
    (vary-meta p assoc :tag (form->tag p tn))))

(defn- emit-function-arity [[op f args & body :as form]]
  (binding [*return-type* (form->tag args)]
    (let [arg-template-names (for [arg args]
                               (symbol (munge-name (str "__T_" arg))))
          fn-name (str "__" (munge-name f))]
      (emit-template args arg-template-names)
      (print (str *indent*
                  (if (= 'auto *return-type*)
                    "decltype(auto)"
                    *return-type*)
                  " "
                  "operator()"
                  (str "("
                       (->> (for [[tn arg] (map vector arg-template-names args)]
                              (with-out-str
                                (emit-var-declaration (maybe-make-ref (maybe-add-template-name arg tn)))))
                            (str/join ", "))
                       ") const {")))
      (emit-function-body f args body)
      (println (str *indent* "}")))))

(defn- emit-function [[op f args? :as form]]
  (let [fn-name (str "__" (munge-name f))
        fn-type (str "__T_"(munge-name f))]
    (println)
    (println (str *indent* "struct " fn-type " {"))
    (binding [*indent* (str default-indent *indent*)]
      (println (str *indent* "const static " fn-type " " (munge-name f) ";")) ;
      (if (vector? args?)
        (emit-function-arity form)
        (doseq [arity (drop 2 form)]
          (emit-function-arity (concat [op f] arity)))))
    (println (str *indent* "};"))
    (println (str *indent* "const " fn-type " " (munge f) ";"))))

(defn- emit-struct [[_ name fields :as form]]
  (let [field-template-names (for [field fields]
                               (munge-name (str "__T_" field)))]
    (println)
    (emit-template fields field-template-names)
    (print *indent*)
    (println (str "struct " (munge-name name)) " {")
    (binding [*indent* (str *indent* default-indent)]
      (doseq [[tn field] (map vector field-template-names fields)]
        (print *indent*)
        (emit-var-declaration (maybe-add-template-name field tn))
        (println ";")))
    (println (str *indent* "};"))))

(defn- emit-variable-definition
  ([form]
   (emit-variable-definition form *indent*))
  ([[_ name & values :as form] initial-indent]
   (print initial-indent)
   (emit-var-declaration name)
   (when (seq values)
     (when (or (> (count values) 1)
               (re-find #"\.\.\.$" (str (first values))))
       (print " = "))
     (print "{")
     (str/join ", "
               (for [value values]
                 (emit-expression value)))
     (print "}"))
   (println ";")))

(defn- collect-extra-headers [body]
  (let [extra-headers (atom #{})]
    (w/postwalk #(do (cond
                       (re? %)
                       (swap! extra-headers conj 'regex)

                       (inst? %)
                       (swap! extra-headers conj 'chrono)

                       (and (seq? %) (= 'binding (first %)))
                       (swap! extra-headers conj 'memory))
                     %) body)
    @extra-headers))

(defn- emit-implicit-includes [forms]
  (doseq [header (collect-extra-headers forms)]
    (emit-include (vector 'include header))))

(defn- emit-main [ns main]
  (when main
    (let [[_ _ main-args] main
          tag (form->tag main-args)]
      (println)
      (println "int main(int argc, char** argv) {")
      (println (str default-indent (munge-name '*command-line-args*)
                    " = std::vector<std::string>(argv + 1, argv + argc);"))
      (println (str default-indent (str (when (= 'int tag)
                                          "return ")
                                        (some-> (munge-ns ns))
                                        "::_main();")))
      (when (not= 'int tag)
        (println (str default-indent "return 0;")))
      (println "}"))))

;; (literal, variable, call, lambda, if, and set!)

(defn- emit-source [in out]
  (binding [*out* out]
    (let [ns (atom nil)
          main (atom nil)
          forms (vec (read-source in))
          file-guard (str "FOIL_" (System/currentTimeMillis))]
      (println (str "#ifndef " file-guard))
      (println (str "#define " file-guard))
      (emit-implicit-includes forms)
      (doseq [[top-level :as form] forms]
        (emit-line form)
        (binding [*indent* (if @ns
                             default-indent
                             "")]
          (case top-level
            ns (do (assert (nil? @ns) "Only one namespace supported.")
                   (reset! ns (second form))
                   (when-not (= 'foil.core @ns)
                     (emit-using ['use 'foil.core]))
                   (emit-headers form))
            (include require) (emit-include form)
            def (emit-variable-definition form)
            defn (do (when (= '-main (second form))
                               (reset! main form))
                             (emit-function form))
            (defrecord defstruct) (emit-struct form)
            ($code $) (do (print *indent*)
                          (emit-code form)
                          (println)
                          (println)))))
      (when-let [ns @ns]
        (println (str/join " " (repeat (count (ns->ns-parts ns)) "}"))))
      (emit-main @ns @main)
      (println "#endif"))))

(defn -main [& args]
  (emit-source *in* *out*))
