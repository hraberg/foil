(ns foil.core
  (:require [forward_list]
            [vector]))

(def ^std::plus<> +)
(def ^std::minus<> -)
(def ^std::multiplies<> *)
(def ^std::divides<> /)
(def ^std::modulus<> %)

(def ^std::equal_to<> =)
(def ^std::not_equal_to<> !=)
(def ^std::greater<> >)
(def ^std::less<> <)
(def ^std::greater_equal<> >=)
(def ^std::less_equal<> <=)

(def ^std::logical_and<> &&)
(def ^std::logical_or<> ||)
(def ^std::logical_not<> !)

(def ^std::bit_and<> &)
(def ^std::bit_or<> |)
;; ($code "const std::bit_xor<> _CARET_;")
;; ($code "const std::bit_not<> _TILDE_;")

(defn first [^:ref coll]
  (.front coll))

(defn conj!
  (^{:tmpl [T]} [^:mut ^:ref ^std::vector<T> coll
                 ^:ref ^T x]
   (.push_back coll x)
   coll)
  (^{:tmpl [T]} [^:mut ^:ref ^std::forward_list<T> coll
                 ^:ref ^T x]
   (.push_front coll x)
   coll))

(defn map ^{:tmpl [TF TC]} [^:ref ^TF f
                            ^:ref ^TC coll]
  (let [^:mut acc ^"decltype(f(std::declval<typename TC::value_type>()))" []]
    (doseq [x coll]
      (conj! acc (f x)))
    acc))

(defn filter ^{:tmpl [TP TC]} [^:ref ^TP pred
                               ^:ref ^TC coll]
  (let [^:mut acc ^"typename TC::value_type" []]
    (doseq [x coll]
      (when (pred x)
        (conj! acc x)))
    acc))

(defn reduce
  ([^:ref f ^:ref coll]
   (let [^:mut acc (first coll)
         ^:mut first? true]
     (doseq [x coll]
       (if first?
         (set! first? false)
         (set! acc (f acc x))))
     acc))
  ([^:ref f ^:ref val ^:ref coll]
   (let [^:mut acc val]
     (doseq [x coll]
       (set! acc (f acc x)))
     acc)))
