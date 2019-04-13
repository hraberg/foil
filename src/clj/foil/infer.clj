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

(defn generic-type-names []
  (->> (iterate (fn [c]
                  (char (inc (int c))))
                \a)
       (map (comp symbol str))))

(defn- generic-name? [x]
  (and (symbol? x)
       (re-find #"^[a-z]$" (str x))))

(defn- find-generic-names [ctx t]
  (let [tn (atom (sorted-set))]
    (w/postwalk
     #(when (and (generic-name? %)
                 (not (contains? ctx %)))
        (swap! tn conj %))
     t)
    @tn))

(defn- update-generics [ctx t]
  (let [tn (find-generic-names ctx t)]
    (w/postwalk-replace (zipmap tn (map gensym tn)) t)))

(defn- gen-type [ctx form]
  (or (get ctx form)
      (:tag (meta form))
      (gensym "t")))

(defn- tag [form]
  (:tag (meta form) (get replacements (class form) form)))

(declare infer)

(defn assign-types [ctx form]
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
                            ctx (apply dissoc ctx args)
                            tags (set (concat (keys ctx) (cons (tag args) (map tag args))))
                            tns (take (inc (count args))
                                      (remove tags (generic-type-names)))
                            arg-ts (for [[arg tv] (map vector args tns)]
                                     (or (:tag (meta arg)) tv))
                            ctx (merge ctx (zipmap args arg-ts))]
                        (concat
                         (list 'fn
                               (with-meta (mapv (partial assign-types ctx) args)
                                 (update (meta args) :tag (fn [t]
                                                            (or t (last tns))))))
                         (map (partial assign-types ctx) body)))
                   let (let [[_ bindings & body] form
                             [ctx bindings] (reduce
                                             (fn [[ctx acc] [var binding]]
                                               (let [binding (infer ctx binding)
                                                     ctx (dissoc ctx var)
                                                     ctx (assoc ctx var (tag binding))]
                                                 [ctx (concat acc [(assign-types ctx var)
                                                                   binding])]))
                                             [ctx []]
                                             (partition 2 bindings))]
                         (concat
                          (list 'let (vec bindings))
                          (map (partial assign-types ctx) body)))
                   (let [[f & args] form
                         f (assign-types ctx f)]
                     (cons (vary-meta f update :tag (partial update-generics ctx))
                           (map (partial assign-types ctx) args))))
                 form)]
      (when (and (symbol? form)
                 (not (contains? ctx form)))
        (assert false (str "unknown var: " form)))
      (vary-meta form assoc :tag t))
    form))

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
              form]
             [(tag (last body))
              (tag args)
              args]]))
      let (let [[_ bindings & body] form
                bindings (partition 2 bindings)]
            (concat
             (mapcat generate-equations (concat (map second bindings) body))
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

(defn- unify [acc x y msg]
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

(defn- apply-unifier [subst t]
  (let [t-new (w/postwalk-replace subst t)]
    (if (= t-new t)
      t
      (recur subst t-new))))

(defn unify-all [equations]
  (reduce
   (fn [acc [x y form]]
     (unify acc x y (delay (pr-str (apply-unifier acc x)
                                   (apply-unifier acc y)
                                   form))))
   {} (reverse equations)))

(defn type-all [subst form]
  (let [form (if (instance? IObj form)
               (vary-meta form update :tag (partial apply-unifier subst))
               form)]
    (cond
      (seq? form)
      (with-meta (map (partial type-all subst) form) (meta form))
      (coll? form)
      (with-meta (into (empty form) (map (partial type-all subst) form)) (meta form))
      :else
      form)))

(defn infer
  ([form]
   (infer *built-ins* form))
  ([ctx form]
   (let [form (assign-types ctx form)
         eqs (generate-equations form)
         subst (unify-all eqs)]
     (type-all subst form))))

(comment
  (->> '(fn [f g x]
          (if (f (= x 1))
            (g x)
            20))
       (foil.infer/infer)
       (meta)
       :tag))
