(ns foil.main
  (:require [clojure.java.io :as io]
            [clojure.tools.reader :as r]
            [clojure.tools.reader.reader-types :as rt]
            [clojure.string :as str]
            [clojure.walk :as w])
  (:import java.util.regex.Pattern)
  (:gen-class))

(defn- read-source [in]
  (let [r (rt/indexing-push-back-reader in 2)]
    (binding [r/*read-eval* false]
      (->> (repeatedly #(r/read {:read-cond :allow
                                 :features #{:foil
                                             :c++}
                                 :eof ::eof} r))
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
(def ^:dynamic ^:private *unsafe?* false)
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

(def ^:private unsafe-ops '#{& * aget aset deref})

(def ^:private fn-replacements '{clojure.core/deref deref})

(def ^:private builtins '#{static_cast})

(declare emit-block emit-expression-statement emit-expression emit-expression-in-lambda
         emit-function-body emit-var-definition foil-macroexpand emit-if)

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
    (cond-> (if (vector? tag)
              (str/join "," tag)
              tag)
      (:ptr (meta form)) (str "*"))))

(defn- check-unsafe [[f :as form]]
  (assert (or *unsafe?* (not (contains? unsafe-ops f)))
          (str "Operation requires unsafe: " (pr-str form))))

(defn- emit-application [[f & args :as form]]
  (binding [*tail?* false
            *expr?* true]
    (cond
      (and (contains? unary-op f)
           (= 1 (count args)))
      (do (check-unsafe form)
          (print (str "(" (get unary-op f)))
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
      (do (check-unsafe form)
          (emit-array-access args))

      (= 'aset f)
      (do (check-unsafe form)
          (emit-array-access (butlast args))
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
        (check-unsafe form)
        (emit-expression f)
        (when-let [tag (maybe-template-params form)]
          (when-not (or (re-find #"::" (str f))
                        (contains? builtins f))
            (print ".operator()"))
          (print (str "<" tag ">")))
        (print (str "(" (str/join ", " (map #(with-out-str
                                               (emit-expression %)) args)) ")"))
        (when (:... (meta form))
          (print "..."))))))

(defn- emit-var-declaration
  ([var]
   (emit-var-declaration var default-tag))
  ([var default-tag]
   (let [{:keys [const dynamic val ref & mut ! ptr]} (meta var)
         tag (form->tag var default-tag)
         ref? (and (or ref &)
                   (not val)
                   (not ptr)
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
                   (when (or ptr (:* (meta var)))
                     "*")
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

(defn- $code [f]
  `(~'$code
    ~(with-out-str
       (f))))

(defmulti foil-macroexpand (fn [[op :as form]] (when (symbol? op)
                                                 (keyword (name op)))))

(defmethod foil-macroexpand :fn [[_ args & body :as form]]
  ($code
   (fn []
     (print (str "["
                 (if (:ref (meta args))
                   "&"
                   "=")
                 "] ("
                 (->> args
                      (map #(with-out-str
                              (emit-var-declaration %)))
                      (str/join ", "))
                 ") {"))
     (binding [*return-type* 'auto]
       (emit-function-body "lambda" args body))
     (print (str *indent* "}")))))

(defmethod foil-macroexpand :fn* [[_ args & body :as form]]
  `(~'fn ~args ~@body))

(defmethod foil-macroexpand :λ [[_ args & body :as form]]
  `(~'fn ~args ~@body))

(defmethod foil-macroexpand :let [[_ bindings & body :as form]]
  (let [bindings (partition 2 bindings)]
    (with-meta
      `(~'do
        ~@(for [[var binding] bindings]
            `(~'def ~var ~binding))
        ~@body)
      (meta form))))

(defmethod foil-macroexpand :def [form]
  (emit-var-definition form))

(defmethod foil-macroexpand :return [[_ value :as from]]
  (let [maybe-expanded (when (and (seq? value)
                                  (not *expr?*))
                         (foil-macroexpand value))]
    (if (= 'do (first maybe-expanded))
      (emit-expression maybe-expanded)
      (do (print (str *indent* "return "))
          (binding [*expr?* true]
            (emit-expression value))
          (println ";")))))

(defmethod foil-macroexpand :recur [[_ & expressions :as form]]
  (assert *loop-vars* "Not in a loop.")
  (assert *tail?* "Can only recur from tail position.")
  (doseq [[var expression] (map vector *loop-vars* expressions)]
    (print (str *indent* (munge-name var) " = "))
    (binding [*expr?* true
              *tail?* false]
      (emit-expression expression))
    (println ";")))

(defmethod foil-macroexpand :loop [[_ bindings & body :as form]]
  (let [bindings (partition 2 bindings)]
    (with-meta
      `((~'fn ~(vec (for [[var] bindings]
                      (cond-> var
                        (not (string? var)) (vary-meta assoc :mut true))))
         ~@body)
        ~@(map second bindings))
      (meta form))))

(defmethod foil-macroexpand :defer [[_ & body :as form]]
  (let [defer-sym (gensym "__defer")]
    `(~'def ~defer-sym ^"std::ostream,std::function<void(std::ostream*)>"
      (~(symbol "std::unique_ptr.") ^:unsafe (~'& ~(symbol "std::cout"))
       (~'fn ^:no-loop ^:ref [~'_]
        ~@body)))))

(defmethod foil-macroexpand :binding [[op bindings & body :as form]]
  (with-meta
    `(~'do
      ~@(->> (for [[var v-binding] (partition 2 bindings)
                   :let [old-binding (gensym "__old_binding")]]
               `[(~'def ~old-binding ~var)
                 (~'defer (~'set! ~var ~old-binding))
                 (~'set! ~var ~v-binding)])
             (apply concat))
      ~@body)
    (meta form)))

(defn- macroexpand-do [[_ & body :as form]]
  (when (seq body)
    (if (= 1 (count body))
      (first body)
      `((~'fn ^:no-loop ^:ref [] ~@body)))))

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
  (let [e-sym (gensym '__case-e)]
    `(~'let [~e-sym ~e]
      (~'cond
       ~@(apply concat
                (for [[constant expr] (partition 2 clauses)]
                  `[~(if (seq? constant)
                       `(~'or ~@(for [constant constant]
                                  `(~'= ~e-sym ~constant)))
                       `(~'= ~e-sym ~constant))
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
        (doseq [[var limit] (partition 2 bindings)
                :let [limit-sym (gensym "__limit")]]
          (println (str "for (int " var " = 0, " limit-sym " = static_cast<int>("
                        (binding [*expr?* true]
                          (with-out-str
                            (emit-expression limit))) "); " var " < " limit-sym "; " var "++)")))
        (emit-block body)))))

(defmethod foil-macroexpand :doseq* [[_ [var v-binding] & body :as form]]
  (if *expr?*
    `((~'fn ^:no-loop [] ~form ~'nullptr))
    ($code
     #(binding [*expr?* false
                *tail?* false]
        (println (str "for (" (with-out-str
                                (emit-var-declaration (maybe-make-ref var) 'auto)) " : "
                      (binding [*expr?* true]
                        (with-out-str
                          (emit-expression v-binding)))
                      ")"))
        (emit-block body)))))

(defmethod foil-macroexpand :doseq [[_ bindings & body :as form]]
  (first
   (reduce
    (fn [x [var v-binding]]
      [(cond
         (= :when var)
         `(~'when ~v-binding ~@x)

         (= :let var)
         `(~'let ~v-binding ~@x)

         (= :while var)
         `(~'if ~v-binding (~'do ~@x) (~'$code "break;"))

         :else
         `(~'doseq* [~var ~v-binding] ~@x))])
    body
    (reverse (partition 2 bindings)))))

(defmethod foil-macroexpand :for [[_ bindings body :as form]]
  (let [acc-sym (gensym "__for")]
    `(~'let [~(with-meta acc-sym {:mut true})
             ~(with-meta [] {:tag (:tag (meta form))})]
      (~'doseq ~bindings (~'conj! ~acc-sym ~body))
      ~acc-sym)))

(defmethod foil-macroexpand :doto [[_ x & forms :as form]]
  (let [x-sym (gensym "__doto")]
    `(~'let [~(with-meta x-sym {:mut true :ref true}) ~x]
      ~@(for [[y & ys] forms]
          (concat [y x-sym] ys))
      ~x-sym)))

(defmethod foil-macroexpand :with-out-str [[_ & body :as form]]
  (let [out-sym (gensym "__out")]
    `(~'let [~(with-meta out-sym {:mut true}) (~(symbol "std::ostringstream."))]
      (~'binding [~'*out*  ^:unsafe (~'& (~'<< ~out-sym ~(symbol "std::boolalpha")))]
       ~@body)
      (~'.str ~out-sym))))

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

      (re? form)
      (emit-application `(~'re-pattern ~(str form)))

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
        (emit-application (with-meta
                            (cons
                             'hash-map
                             (for [[k v] form]
                               `(~'cons ~k ~v)))
                            {:tag tag})))

      (or (seq? form)
          (set? form)
          (vector? form))
      (emit-application (with-meta
                          (cons (cond
                                  (seq? form) 'list
                                  (set? form) 'hash_set
                                  (vector? form) 'vector)
                                form)
                          {:tag (form->tag form)}))

      :else
      (pr form))))

(defn- emit-code [[op & body]]
  (print (str/join body)))

(defn- emit-expression [form]
  (binding [*unsafe?* (or (:unsafe (meta form)) *unsafe?*)]
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
                    (emit-expression (vary-meta
                                      macro-expansion
                                      update :unsafe (fn [x]
                                                       (or x (:unsafe (meta macro-expansion)))))))))))))))

(def ^:dynamic ^:private *file-name* nil)

(defn- emit-line [form]
  (when-let [line (:line (meta form))]
    (print "#line" line (or (some-> *file-name* (pr-str)) ""))
    (println (str " // " (pr-str form)))))

(defn- emit-expression-statement [form]
  (emit-line form)
  (binding [*expr?* false]
    (print *indent*)
    (emit-expression form)
    (println ";")))

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

(defn- parameter-pack? [x]
  (re-find #"\.\.\.$" (str x)))

(defn- emit-var-definition
  ([form]
   (emit-var-definition form *indent*))
  ([[_ name & values :as form] initial-indent]
   (print initial-indent)
   (emit-var-declaration name)
   (when (seq values)
     (when (or (> (count values) 1)
               (parameter-pack? (first values)))
       (print " = "))
     (print "{")
     (binding [*expr?* true
               *tail?* false]
       (print (str/join ", "
                        (for [value values]
                          (with-out-str
                            (emit-expression value))))))
     (print "}"))
   (println ";")))

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
            def (emit-var-definition form)
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
  (case (count args)
    0 (emit-source *in* *out*)
    1 (with-open [in (io/reader (io/file (first args)))]
        (binding [*file-name* (first args)]
          (emit-source in *out*)))
    2 (with-open [in (io/reader (io/file (first args)))
                  out (io/writer (io/file (second args)))]
        (binding [*file-name* (first args)]
          (emit-source in out)))))
