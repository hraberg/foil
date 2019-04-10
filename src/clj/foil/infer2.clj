(ns foil.infer2
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

(defn assign-types
  ([form]
   (assign-types {'= '((t t) -> bool)} form))
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
      (let [[f & args] form]
        (concat
         (->> (for [x form]
                (generate-equations ctx x))
              (apply concat))
         [[(get ctx f)
           (list (map ctx args) '-> (get ctx form))
           form]])))
    []))

(comment
  (let [form '(fn [f g x]
                (if (f (= x 1))
                  (g x)
                  20))
        env (foil.infer2/assign-types form)]
    [env
     (foil.infer2/generate-equations
      env
      form)]))
