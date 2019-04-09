(ns foil.infer
  (:require [clojure.string :as str]))

(def replacements
  {java.lang.Long 'long
   java.lang.Double 'double
   java.lang.Boolean 'boolean
   nil 'void})

(defn logic-var? [x]
  (and (symbol? x)
       (str/starts-with? (str x) "?")))

(defn unify [x y]
  (cond
    (logic-var? x)
    y

    (logic-var? y)
    x

    :else
    (do (assert (= x y)
                (str x " != " y))
        x)))

(defn infer
  ([form]
   (infer {'+ (infer {} '(fn [^?t x ^?t y] x))} form))
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
                 env (reduce
                      (fn [env [var binding]]
                        (let [t (infer env binding)
                              t (if-let [tag (:tag (meta var))]
                                  (unify tag t)
                                  t)]
                          (assoc env var t)))
                      env (partition 2 bindings))]
             (infer env (cons 'do body)))
       fn (let [[_ args & body] form
                env (reduce
                     (fn [env arg]
                       (assoc env arg (or (:tag (meta arg))
                                          (gensym "?arg_"))))
                     env args)
                return (infer env (cons 'do body))]
            (with-meta (list return (mapv env args)) {:fn form}))
       set! (let [[_ var value] form]
              (unify (infer env var) (infer env value)))
       (let [f (infer env (first form))
             [return-t arg-ts] f
             actual-arg-ts (mapv (partial infer env) (rest form))]
         (assert (= (count arg-ts) (count actual-arg-ts)))
         (let [[_ args & body] (:fn (meta f))
               actual-arg-ts (map unify arg-ts actual-arg-ts)
               actual-return-t (infer (merge env (zipmap args actual-arg-ts))
                                      (cons 'do body))]
           (unify return-t actual-return-t))))

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
