(ns foil.core
  (:require [algorithm]
            [atomic]
            [chrono]
            [forward_list]
            [functional]
            [iostream]
            [map]
            [memory]
            [regex]
            [set]
            [unordered_map]
            [unordered_set]
            [vector]
            [experimental/optional]))

(def ^:dynamic ^:ptr ^std::ostream *out* ^:unsafe (& (<< std::cout std::boolalpha)))
(def ^:dynamic ^:ptr ^std::ostream *err* ^:unsafe (& (<< std::cerr std::boolalpha)))
(def ^:dynamic ^:ptr ^std::istream *in* ^:unsafe (& std::cin))

(def ^:dynamic ^"std::vector<std::string>" *command-line-args*)
(def ^:dynamic *foil-version* "0.1.0-SNAPSHOT")

(def ^std::plus<> +)
(def ^std::minus<> -)
(def ^std::multiplies<> *)
(def ^std::divides<> /)
(def ^std::modulus<> mod)

(def ^std::equal_to<> =)
(def ^std::not_equal_to<> !=)
(def ^std::greater<> >)
(def ^std::less<> <)
(def ^std::greater_equal<> >=)
(def ^std::less_equal<> <=)

(def ^std::logical_and<> &&)
(def ^std::logical_or<> ||)
(def ^std::logical_not<> !)

(def ^std::bit_and<> bit-and)
(def ^std::bit_or<> bit-or)
(def ^std::bit_xor<> bit-xor)
(def ^std::bit_not<> bit-not)

(defstruct Cons ^{:tpl [T]} [^:mut ^T car ^:mut ^std::shared_ptr<Cons<T>> cdr])
(defstruct ConsIterator ^{:tpl [T]} [^:ptr ^:mut ^Cons<T> head])

($ "
template <typename T>
ConsIterator<T> begin(Cons<T>& cons) {
    return ConsIterator<T>{&cons};
}

template<typename T>
ConsIterator<T> end(__attribute__((unused)) Cons<T>& cons) {
    return ConsIterator<T>{nullptr};
}

template<typename T>
bool operator!=(__attribute__((unused)) ConsIterator<T>& x, __attribute__((unused)) ConsIterator<T>& y) {
    return x.head != y.head;
}

template<typename T>
T operator*(ConsIterator<T>& it) {
    return it.head->car;
}

template<typename T>
ConsIterator<T>& operator++(ConsIterator<T>& it) {
    it.head = it.head->cdr.get();
    return it;
}
")

(defn cons-2
  (^{:tpl [T]} ^std::shared_ptr<Cons<T>> [^T car ^:val ^std::nullptr_t _]
   ^:unsafe
   (let [n ^Cons<T> (std::make_shared)]
     (set! (.-car (* n)) car)
     (set! (.-cdr (* n)) nullptr)
     n))
  (^{:tpl [T]} ^std::shared_ptr<Cons<T>> [^T car ^:val ^std::shared_ptr<Cons<T>> cdr]
   ^:unsafe
   (let [n ^Cons<T> (std::make_shared)]
     (set! (.-car (* n)) car)
     (set! (.-cdr (* n)) cdr)
     n)))

(defn optional
  (^{:tpl [T]} []
   ^T (std::experimental::fundamentals_v1::optional.))
  (^{:tpl [T]} [^:mut ^T&& x]
   (std::experimental::fundamentals_v1::make_optional ^T (std::forward x))))

(defn cast
  (^{:tpl [C T]} [^:mut ^T&& x]
   ^C (static_cast ^T (std::forward x))))

(defn deref
  (^{:tpl [T]} [^std::atomic<T> x]
   (.load x))
  (^{:tpl [T]} [^std::unique_ptr<std::atomic<T>> x]
   ^:unsafe (deref (* x)))
  ([x]
   ^:unsafe (* x)))

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
   (let [^:mut x (.-first coll)]
     x))
  ([coll]
   (let [x (.front coll)]
     x)))

(defn ffirst [coll]
  (first (first coll)))

(defn first-opt ^{:tpl [T]} [^T coll]
  (if (empty? coll)
    ^"typename T::value_type" (optional)
    (optional (.front coll))))

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
   (let [^:mut x (.-second coll)]
     x))
  ([x]
   (get x 1)))

(def val second)

(defn assoc!
  (^{:tpl [K V]} [^:mut ^"std::unordered_map<K,V>" map ^K key ^V val]
   ^:unsafe
   (doto map
     (aset key val)))
  (^{:tpl [K V C]} [^:mut ^"std::map<K,V,C>" map ^K key ^V val]
   ^:unsafe
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
  (^{:tpl [K V F ...Args]} [^:mut ^"std::unordered_map<K,V>" m ^K k ^F f ^:mut ^Args&&... args]
   (assoc! m k (f (get m k) ^Args ^:... (std::forward args))))
  (^{:tpl [K V C F ...Args]} [^:mut ^"std::map<K,V,C>" m ^K k ^F f ^:mut ^Args&&... args]
   (assoc! m k (f (get m k) ^Args ^:... (std::forward args)))))

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

(defn cons
  ([car cdr]
   (std::make_pair car cdr))
  (^{:tpl [T]} [^T x ^std::forward_list<T> coll]
   (let [^:mut tail coll
         tail-ret (doto tail
                    (.push_front x))]
     tail-ret)))

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
  (fn [^auto... _] x))

(defn identity ^{:tpl [T]} ^T&& [^:mut ^T&& x]
  ^T (std::forward x))

(defn partial
  ([f]
   f)
  (^{:tpl [F ...Args]} [^F f ^Args&... args]
   (fn [^auto... p-args]
     (f args... p-args...))))

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
  (^{:tpl [Arg]} [^:mut ^Arg&& arg]
   ^:unsafe (<< @*out* ^Arg (std::forward arg)))
  (^{:tpl [Arg ...Args]} [^:mut ^Arg&& arg ^:mut ^Args&&... args]
   (print ^Arg (std::forward arg))
   (print " ")
   (print ^Args ^:... (std::forward args))))

(defn flush []
  ^:unsafe (.flush @*out*))

(defn newline []
  (print "\n"))

(defn println
  ([]
   (newline))
  (^{:tpl [Arg ...Args]} [^:mut ^Arg&& arg ^:mut ^Args&&... args]
   (print ^Arg (std::forward arg) ^Args ^:... (std::forward args))
   (println)))

(defn into! ^{:tpl [T F]} ^T [^:mut ^T to ^F from]
  (doseq [x from]
    (conj! to x))
  to)

(defn concat [x y]
  (let [^:mut acc x]
    (into! acc y)))

(defn hash-set ^{:tpl [T ...Args]} [^:mut ^Args&&... args]
  ^T (std::unordered_set. ^Args ^:... (std::forward args)))

(defn sorted-set ^{:tpl [T ...Args]} [^:mut ^Args&&... args]
  ^T (std::set. ^Args ^:... (std::forward args)))

(defn sorted-set-by
  (^{:tpl [T C]} [^C comp]
   ^"T,C" (std::set. comp))
  (^{:tpl [T C ...Args]} [^C comp ^Args&... args]
   (let [^:mut xs args...]
     ^"T,C" (std::set. xs comp))))

(defn hash-map
  (^{:tpl [K V]} []
   ^"K,V" (std::unordered_map.))
  (^{:tpl [K V ...Args]} [^Args&... args]
   (let [xs args...
         ^:mut m ^"K,V" (std::unordered_map.)]
     (into! m xs))))

(defn sorted-map
  (^{:tpl [K V]} []
   ^"K,V" (std::map.))
  (^{:tpl [K V ...Args]} [^Args&... args]
   (let [xs args...
         ^:mut m ^"K,V" (std::map.)]
     (into! m xs))))

(defn sorted-map-by
  (^{:tpl [K V C]} [^C comp]
   ^"K,V" (std::map. comp))
  (^{:tpl [K V C ...Args]} [^C comp ^Args&... args]
   (let [xs args...
         ^:mut m ^"K,V,C" (std::map. comp)]
     (into! m xs))))

(defn list ^{:tpl [T ...Args]} [^:mut ^Args&&... args]
  ^T (std::forward_list. ^Args ^:... (std::forward args)))

(defn vector ^{:tpl [T ...Args]} [^:mut ^Args&&... args]
  ^T (std::vector. ^Args ^:... (std::forward args)))

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
  ^"typename decltype(f(std::declval<typename C::value_type>()))::value_type"
  (for [x coll
        y (f x)]
    y))

(defn filter ^{:tpl [P C]} [^P pred ^C coll]
  ^"typename C::value_type"
  (for [x coll
        :when (pred x)]
    x))

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

(defn drop-while ^{:tpl [P C]} [^P pred ^C coll]
  (let [^:mut drop? true]
    ^"typename C::value_type"
    (for [x coll
          :when (not (and drop? (p x)))]
      (do (set! drop? false)
          x))))

(defn take ^{:tpl [C]} [^std::size_t n ^C coll]
  (let [^:mut ^std::size_t i 0]
    ^"typename C::value_type"
    (for [x coll
          :while (< i n)]
      (do (set! i (inc i))
          x))))

(defn take-while ^{:tpl [P C]} [^P pred ^C coll]
  ^"typename C::value_type"
  (for [x coll
        :while (p x)]
    x))

(defn group-by ^{:tpl [F C]} [^F f ^C coll]
  (let [^:mut acc ^"decltype(f(std::declval<typename C::value_type>())),std::vector<typename C::value_type>" {}]
    (doseq [x coll
            :let [k (f x)
                  ^:ref ^:mut v ^:unsafe (aget acc k)]]
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

(defn box ^{:tpl [T ...Args]} [^:mut ^Args&&... args]
  ^T (std::make_unique ^Args ^:... (std::forward args)))

(defn rc ^{:tpl [T ...Args]} [^:mut ^Args&&... args]
  ^T (std::make_shared ^Args ^:... (std::forward args)))

(defn weak
  (^{:tpl [T]} [^T x]
   ^"typename T::element_type" (std::weak_ptr. x))
  (^{:tpl [T]} [^:mut ^T&& x]
   ^"typename T::element_type" (std::weak_ptr. ^T (std::forward x))))

(defn reset! ^{:tpl [T]} [^std::unique_ptr<std::atomic<T>> atom ^T x]
  ^:unsafe (.store (* atom) x)
  x)

(defn swap! ^{:tpl [T F ...Args]} [^:mut ^std::unique_ptr<std::atomic<T>> atom ^F f ^:mut ^Args&&... args]
  ^:unsafe
  (let [^:mut oldval @atom
        newval (f oldval ^Args ^:... (std::forward args))]
    (if (.compare_exchange_strong (* atom) oldval newval)
      newval
      (recur))))

(defn atom ^{:tpl [T]} [^:mut ^T&& x]
  ^std::atomic<T> (box ^T (std::forward x)))

(defn re-pattern [s]
  (std::regex. s))

(defn subs
  ([s start]
   (.substr s start))
  ([s start end]
   (.substr s start (- end start))))

(defn str
  ([]
   "")
  ([x]
   (let [^:mut out (std::ostringstream.)]
     (<< out std::boolalpha x)
     (.str out)))
  (^{:tpl [Arg ...Args]} [^Arg arg ^Args&... args]
   (+ (str arg) (str args...))))
