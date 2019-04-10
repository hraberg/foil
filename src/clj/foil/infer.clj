(ns foil.infer
  (:require [clojure.walk :as w])
  (:import [clojure.lang IObj]))

;; https://eli.thegreenplace.net/2018/type-inference/
;; https://github.com/prakhar1989/type-inference/blob/master/infer.ml
;; http://dev.stephendiehl.com/fun/006_hindley_milner.html

(def ^:private replacements
  {Byte 'int8_t
   Short 'int16_t
   Integer 'int32_t
   Long 'int64_t
   Character 'char
   Float 'float
   Double 'double
   Boolean 'bool
   String 'char*
   nil 'void})

(def ^:private known-types (set (vals replacements)))
(def ^:dynamic *built-ins* '{= ((t t) -> bool)
                             < ((t t) -> bool)
                             <= ((t t) -> bool)
                             > ((t t) -> bool)
                             >= ((t t) -> bool)
                             or ((bool bool) -> bool)
                             and ((bool bool) -> bool)
                             not ((bool) -> bool)
                             inc ((t) -> t)
                             dec ((t) -> t)
                             + ((t t) -> t)
                             - ((t t) -> t)
                             * ((t t) -> t)
                             / ((t t) -> t)
                             mod ((t t) -> t)
                             set! ((t t) -> t)})

(defn- gen-type [ctx form]
  (or (get ctx form)
      (:tag (meta form))
      (gensym "t")))

(defn assign-types
  ([form]
   (assign-types *built-ins* form))
  ([ctx form]
   (if (instance? IObj form)
     (let [t (gen-type ctx form)
           form (if (seq? form)
                  (case (first form)
                    if (let [[_ cond then else] form]
                         (list 'if
                               (assign-types ctx cond)
                               (assign-types ctx then)
                               (assign-types ctx else)))
                    fn (let [[_ args & body] form
                             arg-ts (mapv (partial gen-type ctx) args)
                             ctx (merge ctx (zipmap args arg-ts))]
                         (concat
                          (list 'fn
                                (mapv (partial assign-types ctx) args))
                          (map (partial assign-types ctx) body)))
                    let (let [[_ bindings & body] form
                              vars (map first (partition 2 bindings))
                              var-ts (mapv (partial gen-type ctx) vars)
                              ctx (merge ctx (zipmap vars var-ts))]
                          (concat
                           (list 'let
                                 (mapv (partial assign-types ctx) bindings))
                           (map (partial assign-types ctx) body)))
                    (map (partial assign-types ctx) form))
                  form)]
       (vary-meta form assoc :tag t))
     form)))

(defn- tag [form]
  (:tag (meta form) (get replacements (class form) form)))

(defn generate-equations [form]
  (if (seq? form)
    (case (first form)
      if (let [[_ cond then else] form]
           (concat
            (generate-equations cond)
            (generate-equations then)
            (generate-equations else)
            [['bool (tag cond) cond]
             [(tag form) (tag then) then]
             [(tag form) (tag else) else]]))
      fn (let [[_ args & body] form]
           (concat
            (mapcat generate-equations body)
            [[(tag form)
              (list (map tag args) '-> (tag (last body)))
              form]]))
      let (let [[_ bindings & body] form
                bindings (partition 2 bindings)]
            (concat
             (mapcat generate-equations (concat (map second bindings) body))
             (for [[var binding :as form] bindings]
               [(tag var) (tag binding) (cons 'set! form)])
             [[(tag form)
               (tag (last body))
               form]]))
      (let [[f & args] form]
        (concat
         (mapcat generate-equations form)
         [[(tag f)
           (list (map tag args) '-> (tag form))
           form]])))
    []))

(defn unify [acc x y msg]
  (cond
    (= x y)
    acc

    (and (symbol? x)
         (not (contains? known-types x)))
    (if (contains? acc x)
      (unify acc (get acc x) y msg)
      (assoc acc x y))

    (and (symbol? y)
         (not (contains? known-types y)))
    (if (contains? acc y)
      (unify acc (get acc y) x msg)
      (assoc acc y x))

    (and (seq? x) (seq? y))
    (let [acc (unify acc (last x) (last y) msg)]
      (assert (= (count (first x))
                 (count (first y)))
              (str (count (first x))
                   " != "
                   (count (first y))
                   " "
                   @msg))
      (reduce
       (fn [acc [x y]]
         (unify acc x y msg))
       acc
       (map vector (first x) (first y))))

    :else
    (assert false (str x " != " y " " @msg))))

(defn apply-unifier* [subst t]
  (let [t-new (w/postwalk-replace subst t)]
    (if (= t-new t)
      t
      (recur subst t-new))))

(defn unify-all [equations]
  (reduce
   (fn [acc [x y form]]
     (unify acc x y (delay (pr-str (apply-unifier* acc x)
                                   (apply-unifier* acc y)
                                   form))))
   {} (reverse equations)))

(defn apply-unifier [subst form]
  (apply-unifier* subst (tag form)))

(comment
  (let [form '(fn [f g x]
                (if (f (= x 1))
                  (g x)
                  20))
        form (foil.infer/assign-types form)
        eqs (foil.infer/generate-equations form)
        subst (foil.infer/unify-all eqs)]
    (foil.infer/apply-unifier subst form)))
