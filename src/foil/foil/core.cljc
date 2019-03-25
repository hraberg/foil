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

(defn empty?
  (^{:tpl [T1 T2]} [^"std::pair<T1,T2>" _]
   false)
  ([coll]
   (.empty coll)))

(defn empty! ^{:tpl [T]} ^T [^:mut ^T coll]
  (doto coll
    (.clear)))

(defn empty [coll]
  (let [^:mut xs coll]
    (doto xs
      (.clear))))

(defn get [map key]
  (.at map key))

(defn first
  (^{:tpl [T1 T2]} [^"std::pair<T1,T2>" coll]
   (let [^:mut k (.-first coll)]
     k))
  ([coll]
   (.front coll)))

(defn ffirst [coll]
  (first (first coll)))

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

(defn butlast [coll]
  (let [^:mut xs coll]
    (when-not (empty? xs)
      (.pop_back xs))
    xs))

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
   (doto map
     (aset key val)))
  (^{:tpl [K V C]} [^:mut ^"std::map<K,V,C>" map ^K key ^V val]
   (doto map
     (aset key val))))

(defn conj!
  (^{:tpl [T]} [^:mut ^std::vector<T> coll ^T x]
   (doto coll
     (.push_back x)))
  (^{:tpl [T]} [^:mut ^std::forward_list<T> coll ^T x]
   (doto coll
     (.push_front x)))
  (^{:tpl [T]} [^:mut ^std::unordered_set<T> coll ^T x]
   (doto coll
     (.insert x)))
  (^{:tpl [T C]} [^:mut ^"std::set<T,C>" coll ^T x]
   (doto coll
     (.insert x)))
  (^{:tpl [K V T]} [^:mut ^"std::unordered_map<K,V>" map ^T x]
   (assoc! map (first x) (second x)))
  (^{:tpl [K V C T]} [^:mut ^"std::map<K,V,C>" map ^T x]
   (assoc! map (first x) (second x))))

(defn disj!
  (^{:tpl [T]} [^:mut ^std::unordered_set<T> set ^T x]
   (doto set
     (.erase x)))
  (^{:tpl [T C]} [^:mut ^"std::set<T,C>" set ^T x]
   (doto set
     (.erase x))))

(defn dissoc!
  (^{:tpl [K V]} [^:mut ^"std::unordered_map<K,V>" map ^K key]
   (doto map
     (.erase key)))
  (^{:tpl [K V C]} [^:mut ^"std::map<K,V,C>" map ^K key]
   (doto map
     (.erase key))))

(defn update!
  (^{:tpl [K V F ...Args]} [^:mut ^"std::unordered_map<K,V>" m ^K k ^F f ^Args&... args]
   (assoc! m k (f (get m k) args...)))
  (^{:tpl [K V C F ...Args]} [^:mut ^"std::map<K,V,C>" m ^K k ^F f ^Args&... args]
   (assoc! m k (f (get m k) args...))))

(defn contains?
  (^{:tpl [K]} [^std::unordered_set<K> coll ^K key]
   (= (.count coll key) 1))
  (^{:tpl [K C]} [^"std::set<K,C>" coll ^K key]
   (= (.count coll key) 1))
  (^{:tpl [K V]} [^"std::unordered_map<K,V>" coll ^K key]
   (= (.count coll key) 1))
  (^{:tpl [K V C]} [^"std::map<K,V,C>" coll ^K key]
   (= (.count coll key) 1))
  ([coll key]
   (.contains coll key)))

(defn count
  (^{:tpl [T]} [^std::forward_list<T> coll]
   (let [^:mut n 0]
     (doseq [_ coll]
       (set! n (inc n)))
     n))
  (^{:tpl [T1 T2]} [^"std::pair<T1,T2>" _]
   2)
  ([coll]
   (.size coll)))

(defn cons [x coll]
  (let [^:mut tail coll]
    (doto tail
      (.push_front x))))

(defn nth
  (^{:tpl [T]} [^std::vector<T> coll ^std::size_t index ^T not-found]
   (if (>= (count coll) index)
     not-found
     (get coll index)))
  ([coll index not-found]
   (let [^:mut n 0]
     (doseq [x coll]
       (if (= n index)
         (return x)
         (set! n (inc n))))
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

(defn sort!
  ([^:mut coll]
   (sort! < coll))
  ([comp ^:mut coll]
   (std::sort (.begin coll) (.end coll) comp)
   coll))

(defn sort
  ([coll]
   (sort < coll))
  ([comp coll]
   (let [^:mut xs coll]
     (sort! comp xs)
     xs)))

(defn sort-by
  ([keyfn coll]
   (sort-by keyfn < coll))
  ([keyfn comp coll]
   (sort (fn [a b]
           (comp (keyfn a) (keyfn b))) coll)))

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

(defn concat [x y]
  (let [^:mut acc x]
    (into! acc y)))

(defn hash-set ^{:tpl [T ...Args]} [^Args&... args]
  ^T (std::unordered_set. args...))

(defn sorted-set ^{:tpl [T ...Args]} [^Args&... args]
  ^T (std::set. args...))

(defn sorted-set-by ^{:tpl [T C ...Args]} [^C comp ^Args&... args]
  (let [^:mut xs args...]
    ^"T,C" (std::set. xs comp)))

(defn hash-map ^{:tpl [K V ...Args]} [^Args&... args]
  (let [xs args...
        ^:mut m ^"K,V" (std::unordered_map.)]
    (into! m xs)))

(defn sorted-map ^{:tpl [K V ...Args]} [^Args&... args]
  (let [xs args...
        ^:mut m ^"K,V" (std::map.)]
    (into! m xs)))

(defn sorted-map-by ^{:tpl [K V C ...Args]} [^C comp ^Args&... args]
  (let [xs args...
        ^:mut m ^"K,V,C" (std::map. comp)]
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
    (.reserve acc (count coll))
    (doseq [x coll]
      (conj! acc (f x)))
    acc))

(defn map-indexed ^{:tpl [F C]} [^F f ^C coll]
  (let [^:mut ^std::size_t n 0
        ^:mut acc ^"decltype(f(std::declval<std::size_t>(), std::declval<typename C::value_type>()))" []]
    (.reserve acc (count coll))
    (doseq [x coll]
      (conj! acc (f n x))
      (set! n (inc n)))
    acc))

(defn mapcat ^{:tpl [F C]} [^F f ^C coll]
  (let [^:mut acc ^"typename decltype(f(std::declval<typename C::value_type>()))::value_type" []]
    (doseq [x coll
            y (f x)]
      (conj! acc y))
    acc))

(defn filter ^{:tpl [P C]} [^P pred ^C coll]
  (let [^:mut acc ^"typename C::value_type" []]
    (doseq [x coll
            :when (pred x)]
      (conj! acc x))
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

(defn reductions
  (^{:tpl [F C]} [^F f ^C coll]
   (let [^:mut accs ^"typename C::value_type" []
         ^:mut acc (first coll)
         ^:mut first? true]
     (.reserve accs (count coll))
     (doseq [x coll]
       (if first?
         (set! first? false)
         (do (set! acc (f acc x))
             (conj! accs acc))))
     accs))
  (^{:tpl [F T C]} [^F f ^T val ^C coll]
   (let [^:mut accs ^T []
         ^:mut acc val]
     (.reserve accs (count coll))
     (doseq [x coll]
       (set! acc (f acc x))
       (conj! accs acc))
     accs)))

(defn reverse ^{:tpl [C]} [^C coll]
  (let [^:mut acc ^"typename C::value_type" ()]
    (into! acc coll)))

(defn drop ^{:tpl [C]} [^std::size_t n ^C coll]
  (let [^:mut ^std::size_t i 0
        ^:mut acc ^"typename C::value_type" []]
    (doseq [x coll]
      (when (>= i n)
        (conj! acc x))
      (set! i (inc i)))
    acc))

(defn take ^{:tpl [C]} [^std::size_t n ^C coll]
  (let [^:mut ^std::size_t i 0
        ^:mut acc ^"typename C::value_type" []]
    (doseq [x coll
            :while (< i n)]
      (conj! acc x)
      (set! i (inc i)))
    acc))

(defn group-by ^{:tpl [F C]} [^F f ^C coll]
  (let [^:mut acc ^"decltype(f(std::declval<typename C::value_type>())),std::vector<typename C::value_type>" {}]
    (doseq [x coll
            :let [k (f x)
                  ^:ref ^:mut v (aget acc k)]]
      (conj! v x))
    acc))

(defn every? ^{:tpl [P C]} [^P pred ^C coll]
  (doseq [x coll]
    (when-not (pred x)
      (return false)))
  true)

(defn not-any? [pred coll]
  (not (every? pred coll)))

(defn repeat ^{:tpl [T]} [^std::size_t n ^T x]
  (let [^:mut acc ^T []]
    (.reserve acc n)
    (dotimes [_ n]
      (conj! acc x))
    acc))

(defn repeatedly ^{:tpl [F]} [^std::size_t n ^F f]
  (let [^:mut acc ^"decltype(f())" []]
    (.reserve acc n)
    (dotimes [_ n]
      (conj! acc (f)))
    acc))

(defn keys [m]
  (map key m))

(defn vals [m]
  (map val m))

(defn deref
  (^{:tpl [T]} [^std::atomic<T> x]
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
