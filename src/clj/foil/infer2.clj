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
                 if (rest form)
                 fn (concat (second form)
                            (next (next form)))
                 form))
       ctx))))

(comment
  (assign-types
   '(fn [f g x]
      (if (f (= x 1))
        (g x)
        20))))
