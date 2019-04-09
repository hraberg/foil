(ns foil.infer
  (:require [clojure.string :as str]))

(def ^:private replacements
  {java.lang.Byte 'int8_t
   java.lang.Short 'int16_t
   java.lang.Integer 'int32_t
   java.lang.Long 'int64_t
   java.lang.Character 'char
   java.lang.Float 'float
   java.lang.Double 'double
   java.lang.Boolean 'bool
   java.lang.String 'char*
   nil 'void})

(defn- logic-var? [x]
  (and (symbol? x)
       (str/starts-with? (str x) "?")))

(defn- unify [x y]
  (cond
    (logic-var? x)
    (or y x)

    (logic-var? y)
    (or x y)

    :else
    (do (assert (= x y)
                (str x " != " y))
        x)))

(defn- infer-binding-pairs [env binding-pairs]
  (reduce
   (fn [env [var binding]]
     (let [t (infer env binding)
           t (if-let [tag (:tag (meta var))]
               (unify tag t)
               t)]
       (assoc env var t)))
   env binding-pairs))

(declare ^:dynamic *default-env*)

(defn infer
  ([form]
   (infer *default-env* form))
  ([env form]
   (cond
     (seq? form)
     (case (first form)
       if (let [[_  cond then else] form]
            (and (unify 'boolean (infer env cond))
                 (if (nil? else)
                   (infer env then)
                   (unify (infer env then)
                          (infer env else)))))
       do (last (mapv (partial infer env) (rest form)))
       let (let [[_ bindings & body] form
                 env (infer-binding-pairs env (partition 2 bindings))]
             (infer env (cons 'do body)))
       fn (let [[_ args & body] form
                env (reduce
                     (fn [env arg]
                       (assoc env arg (or (:tag (meta arg))
                                          (gensym "?T"))))
                     env args)
                return-t (or (:tag (meta args)) (gensym "?R"))
                actual-return (infer env (cons 'do body))
                env (if (logic-var? actual-return)
                      (into {} (for [[k v] env]
                                 (if (= actual-return v)
                                   [k return-t]
                                   [k v])))
                      env)
                return-t (if (seq body)
                           (unify return-t actual-return)
                           return-t)]
            (with-meta (list return-t '(*) (map env args)) {:fn form
                                                            :env env}))
       set! (let [[_ var value] form]
              (unify (infer env var) (infer env value)))
       (let [f (infer env (first form))
             [return-t _ arg-ts] f
             actual-args (rest form)]
         (assert (= (count arg-ts) (count actual-args))
                 (str "arity: "(count arg-ts) " != "(count actual-args)))
         (let [[_ args & body] (:fn (meta f))
               env (infer-binding-pairs env (map vector args actual-args))
               bound-arg-ts (reduce
                             (fn [acc [arg t]]
                               (if (logic-var? t)
                                 (let [candidate (infer env (unify arg t))]
                                   (when-let [b (get acc t)]
                                     (unify b candidate))
                                   (assoc acc t candidate))
                                 acc))
                             {}
                             (zipmap args arg-ts))
               return-t (get bound-arg-ts return-t return-t)]
           (if (seq body)
             (unify return-t (infer env (cons 'do body)))
             return-t))))

     (logic-var? form)
     (get env form form)

     (symbol? form)
     (do (assert (get env form)
                 (str "unknown var: " form))
         (get env form))

     :else
     (let [t (or (:tag (meta form))
                 (class form))]
       (get replacements t t)))))

(def ^:dynamic *default-env*
  {'+ (infer {} '(fn ^?t [^?t x ^?t y]))
   '- (infer {} '(fn ^?t [^?t x ^?t y]))
   '= (infer {} '(fn ^bool [^?t x ^?t y]))})
