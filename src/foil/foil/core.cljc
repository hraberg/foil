(ns foil.core
  (:require [forward_list]
            [functional]
            [iostream]
            [map]
            [set]
            [vector]))

(def ^:dynamic ^:ref *out* std::cout)
(def ^:dynamic ^:ref *err* std::cerr)
(def ^:dynamic ^:ref *in* std::cin)

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

(defn assoc!
  (^{:tmpl [K V]} [^:mut ^"std::map<K,V>" map ^K key ^V val]
   (.insert_or_assign map key val)
   map))

(defn conj!
  (^{:tmpl [T]} [^:mut ^std::vector<T> coll ^T x]
   (.push_back coll x)
   coll)
  (^{:tmpl [T]} [^:mut ^std::forward_list<T> coll ^T x]
   (.push_front coll x)
   coll)
  (^{:tmpl [T]} [^:mut ^std::set<T> coll ^T x]
   (.insert coll x)
   coll))

(defn disj!
  (^{:tmpl [T]} [^:mut ^std::set<T> set ^T x]
   (.erase set x)
   set))

(defn dissoc!
  (^{:tmpl [K V]} [^:mut ^"std::map<K,V>" map ^K key]
   (.erase map key)
   map))

(defn contains? [coll x]
  (.contains coll x))

(defn count [coll]
  (.count coll))

(defn empty? [coll]
  (.empty coll))

(defn first [coll]
  (.front coll))

(defn get [map key]
  (.at map key))

(defn last [coll]
  (.back coll))

(defn inc [n]
  (+ n 1))

(defn dec [n]
  (- n 1))

(defn partial ^{:tmpl "<typename F, typename... Args>"} [^F f ^"Args&&..." args]
  (std::bind f ($ "std::forward<Args>(args)...")))

(defn print [x]
  (<< *out* x))

(defn println [x]
  (<< *out* x std::endl))

(defn map ^{:tmpl [TF TC]} [^TF f ^TC coll]
  (let [^:mut acc ^"decltype(f(std::declval<typename TC::value_type>()))" []]
    (doseq [x coll]
      (conj! acc (f x)))
    acc))

(defn filter ^{:tmpl [TP TC]} [^TP pred ^TC coll]
  (let [^:mut acc ^"typename TC::value_type" []]
    (doseq [x coll]
      (when (pred x)
        (conj! acc x)))
    acc))

(defn reduce
  ([f coll]
   (let [^:mut acc (first coll)
         ^:mut first? true]
     (doseq [x coll]
       (if first?
         (set! first? false)
         (set! acc (f acc x))))
     acc))
  ([f val coll]
   (let [^:mut acc val]
     (doseq [x coll]
       (set! acc (f acc x)))
     acc)))
