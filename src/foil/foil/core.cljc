(ns foil.core
  (:require [algorithm]
            [atomic]
            [forward_list]
            [functional]
            [iostream]
            [map]
            [set]
            [unordered_map]
            [unordered_set]
            [vector]
            [experimental/optional]))

(def ^:dynamic ^:ref *out* (<< std::cout std::boolalpha))
(def ^:dynamic ^:ref *err* (<< std::cerr std::boolalpha))
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

(defn empty? [coll]
  (.empty coll))

(defn get [map key]
  (.at map key))

(defn first
  (^{:tpl [T1 T2]} [^"std::pair<T1,T2>" coll]
   (let [^:mut k (.-first coll)]
     k))
  ([coll]
   (.front coll)))

(defn first-opt ^{:tpl [T]} [^T coll]
  (if (empty? coll)
    ^"typename T::value_type" (std::experimental::fundamentals_v1::optional.)
    (std::experimental::fundamentals_v1::make_optional (.front coll))))

(def key first)

(defn next [coll]
  (let [^:mut tail coll]
    (when-not (empty? tail)
      (.pop_front tail))
    tail))

(defn second
  (^{:tpl [T]} [^std::forward_list<T> coll]
   (first (next coll)))
  (^{:tpl [T1 T2]} [^"std::pair<T1,T2>" coll]
   (.-second coll))
  ([x]
   (get x 1)))

(def val second)

(defn assoc!
  (^{:tpl [K V]} [^:mut ^"std::unordered_map<K,V>" map ^K key ^V val]
   (aset map key val)
   map)
  (^{:tpl [K V C]} [^:mut ^"std::map<K,V,C>" map ^K key ^V val]
   (aset map key val)
   map))

(defn conj!
  (^{:tpl [T]} [^:mut ^std::vector<T> coll ^T x]
   (.push_back coll x)
   coll)
  (^{:tpl [T]} [^:mut ^std::forward_list<T> coll ^T x]
   (.push_front coll x)
   coll)
  (^{:tpl [T]} [^:mut ^std::unordered_set<T> coll ^T x]
   (.insert coll x)
   coll)
  (^{:tpl [T C]} [^:mut ^"std::set<T,C>" coll ^T x]
   (.insert coll x)
   coll)
  (^{:tpl [K V T]} [^:mut ^"std::unordered_map<K,V>" map ^T x]
   (assoc! map (first x) (second x)))
  (^{:tpl [K V C T]} [^:mut ^"std::map<K,V,C>" map ^T x]
   (assoc! map (first x) (second x))))

(defn disj!
  (^{:tpl [T]} [^:mut ^std::unordered_set<T> set ^T x]
   (.erase set x)
   set)
  (^{:tpl [T C]} [^:mut ^"std::set<T,C>" set ^T x]
   (.erase set x)
   set))

(defn dissoc!
  (^{:tpl [K V]} [^:mut ^"std::unordered_map<K,V>" map ^K key]
   (.erase map key)
   map)
  (^{:tpl [K V C]} [^:mut ^"std::map<K,V,C>" map ^K key]
   (.erase map key)
   map))

(defn contains?
  (^{:tpl [K]} [^std::unordered_set<K> coll ^K key]
   (= (.count coll key) 1))
  (^{:tpl [K C]} [^"std::set<K,C>" coll ^K key]
   (= (.count coll key) 1))
  ([coll key]
   (.contains coll key)))

(defn count
  (^{:tpl [T]} [^std::forward_list<T> coll]
   (let [^:mut n 0]
     (doseq [_ coll]
       (set! n (inc n)))
     n))
  ([coll]
   (.size coll)))

(defn cons [x coll]
  (let [^:mut tail coll]
    (.push_front tail x)
    tail))

(defn nth
  (^{:tpl [T]} [^std::vector<T> coll ^std::size_t index ^T not-found]
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

(defn last
  (^{:tpl [T]} [^std::forward_list<T> coll]
   (let [tail (next coll)]
     (if (empty? tail)
       (first coll)
       (recur tail))))
  ([coll]
   (.back coll)))

(defn inc [n]
  (+ n 1))

(defn dec [n]
  (- n 1))

(defn constantly [x]
  (fn [] x))

(defn identity ^{:tpl [T]} ^T&& [^:mut ^T&& x]
  ^T (std::forward x))

(defn partial
  ([f]
   f)
  (^{:tpl [F Arg]} [^F f ^Arg arg]
   (fn [arg2]
     (f arg arg2)))
  (^{:tpl [F Arg ...Args]} [^F f ^Arg arg ^Args&... args]
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

(defn true? [x]
  (= true x))

(defn false? [x]
  (= false x))

(defn boolean [x]
  (if x
    true
    false))

(defn sort
  ([^:mut coll]
   (sort < coll))
  ([comp ^:mut coll]
   (std::sort (.begin coll) (.end coll) comp)
   coll))

(defn print
  ([arg]
   (<< *out* arg))
  (^{:tpl [Arg ...Args]} [^Arg arg ^Args&... args]
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
  (^{:tpl [Arg ...Args]} [^Arg arg ^Args&... args]
   (print arg args...)
   (println)))

(defn into! ^{:tpl [T F]} ^T [^:mut ^T to ^F from]
  (doseq [x from]
    (conj! to x))
  to)

(defn hash-set ^{:tpl [T ...Args]} [^Args&... args]
  ^T (std::unordered_set. args...))

(defn sorted-set ^{:tpl [T ...Args]} [^Args&... args]
  ^T (std::set. args...))

(defn sorted-set-by ^{:tpl [T C ...Args]} [^C comp ^Args&... args]
  ($ "auto xs = {args...}")
  ^"T,C" (std::set. xs comp))

(defn hash-map ^{:tpl [K V ...Args]} [^Args&... args]
  ($ "auto xs = {args...}")
  (let [^:mut m ^"K,V" (std::unordered_map.)]
    (into! m xs)))

(defn sorted-map ^{:tpl [K V ...Args]} [^Args&... args]
  ($ "auto xs = {args...}")
  (let [^:mut m ^"K,V" (std::map.)]
    (into! m xs)))

(defn sorted-map-by ^{:tpl [K V C ...Args]} [^C comp ^Args&... args]
  ($ "auto xs = {args...}")
  (let [^:mut m ^"K,V,C" (std::map. comp)]
    (into! m xs)))

(defn list ^{:tpl [T ...Args]} [^Args&... args]
  ^T (std::forward_list. args...))

(defn vector ^{:tpl [T ...Args]} [^Args&... args]
  ^T (std::vector. args...))

(defn set ^{:tpl [T]} [^T coll]
  (let [^:mut acc ^"typename T::value_type" #{}]
    (into! acc coll)))

(defn vec ^{:tpl [T]} [^T coll]
  (let [^:mut acc ^"typename T::value_type" []]
    (into! acc coll)))

(defn map ^{:tpl [F C]} [^F f ^C coll]
  (let [^:mut acc ^"decltype(f(std::declval<typename C::value_type>()))" []]
    (doseq [x coll]
      (conj! acc (f x)))
    acc))

(defn filter ^{:tpl [P C]} [^P pred ^C coll]
  (let [^:mut acc ^"typename C::value_type" []]
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

(defn keys [m]
  (map key m))

(defn vals [m]
  (map val m))

(defn deref
  (^{:tpl [T]} [^std::atomic<T>& x]
   (.load x))
  ([x]
   (* x)))

(defn reset! [^:mut atom x]
  (.store atom x)
  x)

(defn swap! ^{:tpl [T F ...Args]} [^std::atomic<T> ^:mut atom ^F f ^Args&... args]
  (let [^:mut oldval @atom
        newval (f oldval args...)]
    (if (.compare_exchange_strong atom oldval newval)
      newval
      (recur))))

#_(defn atom ^{:tpl [T]} ^std::atomic<T> [^T x]
    (def ^std::atomic<T> ^:mut a)
    (reset! a x)
    a)
