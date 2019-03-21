(ns foil.core
  (:require [algorithm]
            [forward_list]
            [functional]
            [iostream]
            [map]
            [set]
            [vector]))

(def ^:dynamic ^:ref *out* std::cout)
(def ^:dynamic ^:ref *err* std::cerr)
(def ^:dynamic ^:ref *in* std::cin)

(def ^:dynamic ^"std::vector<std::string>" *command-line-args*)
(def ^:dynamic *foil-version* "0.1.0-SNAPSHOT")

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

(defn contains?
  (^{:tmpl [K]} [^std::set<K> coll ^K key]
   (> (.count coll key) 0))
  ([coll key]
   (.contains coll key)))

(defn count
  (^{:tmpl [T]} [^std::forward_list<T> coll]
   (let [^:mut n 0]
     (doseq [_ coll]
       (set! n (inc n)))
     n))
  ([coll]
   (.size coll)))

(defn empty? [coll]
  (.empty coll))

(defn first [coll]
  (.front coll))

(defn next [coll]
  (let [^:mut tail coll]
    (when-not (empty? tail)
      (.pop_front tail))
    tail))

(defn get [map key]
  (.at map key))

(defn last [coll]
  (.back coll))

(defn second [x]
  (get x 1))

(defn inc [n]
  (+ n 1))

(defn dec [n]
  (- n 1))

(defn constantly [x]
  (fn [] x))

(defn hash-set ^{:tmpl [Arg ...Args]} [^Arg arg ^Args&... args]
  ^Arg (std::set. arg args...))

(defn list ^{:tmpl [Arg ...Args]} [^Arg arg ^Args&... args]
  ^Arg (std::forward_list. arg args...))

(defn vector ^{:tmpl [Arg ...Args]} [^Arg arg ^Args&... args]
  ^Arg (std::vector. arg args...))

(defn identity [x]
  x)

(defn partial
  ([f]
   f)
  (^{:tmpl [F Arg]} [^F f ^Arg arg]
   (fn [arg2]
     (f arg arg2)))
  (^{:tmpl [F Arg ...Args]} [^F f ^Arg arg ^Args&... args]
   (partial (partial f arg) args...)))

(defn nil? [x]
  (= nullptr x))

(defn even? [n]
  (= (mod n 2) 0))

(defn odd? [n]
  (not (even? n)))

(defn neg? [n]
  (< n 0))

(defn pos? [n]
  (> n 0))

(defn zero? [n]
  (= n 0))

(defn min [x y]
  (if (> x y)
    y
    x))

(defn max [x y]
  (if (< x y)
    x
    y))

(defn sort
  ([^:mut coll]
   (sort < coll))
  ([comp ^:mut coll]
   (std::sort (.begin coll) (.end coll) comp)
   coll))

(defn print
  ([arg]
   (<< *out* arg))
  (^{:tmpl [Arg ...Args]} [^Arg arg ^Args&... args]
   (print arg)
   (print " ")
   (print args...)))

(defn flush []
  (.flush *out*))

(defn newline []
  (print "\n"))

(defn println
  ([]
   (newline))
  (^{:tmpl [Arg ...Args]} [^Arg arg ^Args&... args]
   (print arg args...)
   (println)))

(defn into! [^:mut to from]
  (doseq [x from]
    (conj! to x))
  to)

(defn set ^{:tmpl [T]} [^T coll]
  (let [^:mut acc ^"typename T::value_type" #{}]
    (into! acc coll)))

(defn vec ^{:tmpl [T]} [^T coll]
  (let [^:mut acc ^"typename T::value_type" []]
    (into! acc coll)))

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

(defn nth
  (^{:tmpl [T]} [^std::vector<T> coll ^std::size_t index ^T not-found]
   (if (>= (count coll) index)
     not-found
     (get coll index)))
  ([coll index not-found]
   (let [^:mut n 0]
     (doseq [x coll]
       (when (= n index)
         (return x))
       (set! n (inc n)))
     not-found)))
