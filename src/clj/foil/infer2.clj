(ns foil.infer2
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
(def ^:dynamic *built-ins* '{= ((t t) -> bool)})

(defn assign-types
  ([form]
   (assign-types *built-ins* form))
  ([ctx form]
   (let [t (or (get ctx form)
               (:tag (meta form))
               (get replacements (class form))
               (gensym "t"))
         ctx (assoc ctx (if (instance? IObj form)
                          (vary-meta form assoc :tag t)
                          form) t)]
     (if (seq? form)
       (reduce assign-types ctx
               (case (first form)
                 if (let [[_ cond then else] form]
                      [cond then else])
                 fn (let [[_ args & body] form]
                      (concat args body))
                 let (let [[_ bindings & body] form]
                       (concat bindings body))
                 form))
       ctx))))

(defn generate-equations [ctx form]
  (if (seq? form)
    (case (first form)
      if (let [[_ cond then else] form]
           (concat
            (generate-equations ctx cond)
            (generate-equations ctx then)
            (generate-equations ctx else)
            [['bool (get ctx cond) cond]
             [(get ctx form) (get ctx then) then]
             [(get ctx form) (get ctx else) else]]))
      fn (let [[_ args & body] form]
           (concat
            (->> (for [x body]
                   (generate-equations ctx x))
                 (apply concat))
            [[(get ctx form)
              (list (map ctx args) '-> (get ctx (last body)))
              form]]))
      let (let [[_ bindings & body] form
                bindings (partition 2 bindings)]
            (concat
             (->> (for [x (concat (map second bindings) body)]
                    (generate-equations ctx x))
                  (apply concat))
             (for [[var binding :as form] bindings]
               [(get ctx var) (get ctx binding) (cons 'set! form)])
             [[(get ctx form)
               (get ctx (last body))
               form]]))
      (let [[f & args] form]
        (concat
         (->> (for [x form]
                (generate-equations ctx x))
              (apply concat))
         [[(get ctx f)
           (list (map ctx args) '-> (get ctx form))
           f]])))
    []))

(defn unify [acc x y]
  (cond
    (= x y)
    acc

    (and (symbol? x)
         (not (contains? known-types x)))
    (assoc acc (get acc x x) (get acc y y))

    (and (symbol? y)
         (not (contains? known-types y)))
    (assoc acc (get acc y y) (get acc x x))

    (and (seq? x) (seq? y))
    (let [acc (unify acc (last x) (last y))]
      (assert (= (count (first x))
                 (count (first y)))
              (str (count (first x))
                   " != "
                   (count (first y))))
      (reduce
       (fn [acc [x y]]
         (unify acc x y))
       acc
       (map vector (first x) (first y))))

    :else
    (assert false (str (get acc x x) " != " (get acc y y)))))

(defn unify-all [equations]
  (reduce
   (fn [acc [x y]]
     (when acc
       (unify acc x y)))
   {} equations))

(defn apply-unifier [subst form]
  (let [form-new (w/postwalk-replace subst form)]
    (if (= form-new form)
      form
      (recur subst form-new))))

(comment
  (let [form '(fn [f g x]
                (if (f (= x 1))
                  (g x)
                  20))
        env (foil.infer2/assign-types form)
        eqs (foil.infer2/generate-equations env form)
        subst (foil.infer2/unify-all eqs)]
    (foil.infer2/apply-unifier subst (get env form))))
