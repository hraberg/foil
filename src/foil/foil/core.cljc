(ns foil.core
  (:require [algorithm]
            [atomic]
            [chrono]
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

(def ^:dynamic ^:ptr ^std::ostream *out* ^:unsafe (address-of (<< std::cout std::boolalpha)))
(def ^:dynamic ^:ptr ^std::ostream *err* ^:unsafe (address-of (<< std::cerr std::boolalpha)))
(def ^:dynamic ^:ptr ^std::istream *in* ^:unsafe (address-of std::cin))

(def ^:mut ^std::vector<std::string> *command-line-args*)
(def *foil-version* "0.1.0-SNAPSHOT")

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

(defn nil? [x]
  (= nullptr x))

(defn deref
  (^{:tpl [T]} [^std::atomic<T> x]
   (.load x))
  (^{:tpl [T]} [^std::unique_ptr<std::atomic<T>> x]
   ^:unsafe (deref (* x)))
  ([x]
   ^:unsafe (* x)))

(defn inc [n]
  (+ n 1))

(defn dec [n]
  (- n 1))

(defn box ^{:tpl [T ...Args]} [^:mut ^Args&&... args]
  ^T (std::make_unique ^Args ^:... (std::forward args)))

(defn rc ^{:tpl [T ...Args]} [^:mut ^Args&&... args]
  ^T (std::make_shared ^Args ^:... (std::forward args)))

(defn weak
  (^{:tpl [T]} [^T x]
   ^"typename T::element_type" (std::weak_ptr. x))
  (^{:tpl [T]} [^:mut ^T&& x]
   ^"typename T::element_type" (std::weak_ptr. ^T (std::forward x))))

(defstruct Cons ^{:tpl [T]}
  [^T car
   ^std::shared_ptr<Cons<T>> cdr])

(defstruct ConsIterator ^{:tpl [T]
                          :using [[value_type T]
                                  [pointer T*]
                                  [reference T&]
                                  [difference_type std::ptrdiff_t]
                                  [iterator_category std::input_iterator_tag]]}
  [^:mut ^std::shared_ptr<Cons<T>> next]

  (operator* []
             ^:unsafe (.-car @next))

  (operator== [^ConsIterator<T> x]
              (= (.get next) (.get (.-next x))))

  (operator!= [^ConsIterator<T> x]
              (not= (.get next) (.get (.-next x))))

  (operator++ ^:mut []
              ^:unsafe
              (do (set! next (.-cdr @next))
                  @this)))

(defstruct ConsList ^{:tpl [T]
                      :using [[value_type T]
                              [pointer T*]
                              [reference T&]
                              [difference_type std::ptrdiff_t]
                              [size_type std::size_t]
                              [iterator ConsIterator<value_type>]]}
  [^:mut ^std::shared_ptr<Cons<T>> head
   ^:mut ^size_type count]

  (size [] count)
  (empty [] (nil? head))
  (front [] ^:unsafe (.-car @head))

  (begin [] (iterator. head))
  (end [] (iterator. nullptr)))

(defn empty?
  (^{:tpl [T1 T2]} [^std::pair<T1|T2> _]
   false)
  ([coll]
   (.empty coll)))

(defn first
  (^{:tpl [T1 T2]} [^std::pair<T1|T2> coll]
   (let [^:mut x (.-first coll)]
     x))
  ([coll]
   (.front coll)))

(defn next
  (^{:tpl [T]} ^ConsList<T> [^ConsList<T> coll]
   (if (empty? coll)
     coll
     ^:unsafe
     ^T (ConsList. (.-cdr @(.-head coll)) (dec (.-count coll))))))

(defmethod operator== ^{:tpl [T]} ^bool [^ConsList<T> x ^ConsList<T> y]
  (std::equal (.begin x) (.end x) (.begin y) (.end y)))

(defmethod operator!= ^{:tpl [T]} ^bool [^ConsList<T> x ^ConsList<T> y]
  (not (= x y)))

(defn cons
  ([car cdr]
   (std::make_pair car cdr))
  (^{:tpl [T]} [^T car ^std::nullptr_t cdr]
   ^T (ConsList. ^Cons<T> (rc ^T (Cons. car cdr)) 1))
  (^{:tpl [T]} [^T car ^ConsList<T> cdr]
   ^T (ConsList. ^Cons<T> (rc ^T (Cons. car (.-head cdr))) (inc (.-count cdr))))
  (^{:tpl [T]} [^T&& car ^ConsList<T>&& cdr]
   ^T (ConsList. ^Cons<T> (rc ^T (Cons. car (.-head cdr))) (inc (.-count cdr)))))

(defn optional
  (^{:tpl [T]} []
   ^T (std::experimental::fundamentals_v1::optional.))
  (^{:tpl [T]} [^:mut ^T&& x]
   (std::experimental::fundamentals_v1::make_optional ^T (std::forward x))))

(defn cast
  (^{:tpl [C T]} [^:mut ^T&& x]
   ^C (static_cast ^T (std::forward x))))

(defn boolean [x]
  (if x
    true
    false))

(defn empty! [^:mut coll]
  (doto coll
    (.clear)))

(defn empty [coll]
  (let [^:mut xs coll]
    (empty! xs)))

(defn get [map key]
  (.at map key))

(defn ffirst [coll]
  (first (first coll)))

(defn first-opt ^{:tpl [T]} [^T coll]
  (if (empty? coll)
    ^"typename T::value_type" (optional)
    (optional (first coll))))

(def key first)

(defn second
  (^{:tpl [T]} [^ConsList<T> coll]
   (first (next coll)))
  (^{:tpl [T1 T2]} [^std::pair<T1|T2> coll]
   (let [^:mut x (.-second coll)]
     x))
  ([x]
   (get x 1)))

(def val second)

(defn assoc!
  (^{:tpl [K V]} [^:mut ^std::unordered_map<K|V> map ^K key ^V val]
   ^:unsafe
   (doto map
     (aset key val)))
  (^{:tpl [K V C]} [^:mut ^std::map<K|V|C> map ^K key ^V val]
   ^:unsafe
   (doto map
     (aset key val))))

(defn conj!
  ([coll] coll)
  (^{:tpl [T]} [^:mut ^std::vector<T> coll ^T x]
   (doto coll
     (.push_back x)))
  (^{:tpl [T]} [^ConsList<T> coll ^T x]
   (cons x coll))
  (^{:tpl [T]} [^:mut ^std::unordered_set<T> coll ^T x]
   (doto coll
     (.insert x)))
  (^{:tpl [T C]} [^:mut ^std::set<T|C> coll ^T x]
   (doto coll
     (.insert x)))
  (^{:tpl [K V T]} [^:mut ^std::unordered_map<K|V> map ^T x]
   (assoc! map (first x) (second x)))
  (^{:tpl [K V C T]} [^:mut ^std::map<K|V|C> map ^T x]
   (assoc! map (first x) (second x))))

(defn disj!
  ([set] set)
  (^{:tpl [T]} [^:mut ^std::unordered_set<T> set ^T x]
   (doto set
     (.erase x)))
  (^{:tpl [T C]} [^:mut ^std::set<T|C> set ^T x]
   (doto set
     (.erase x))))

(defn dissoc!
  ([map] map)
  (^{:tpl [K V]} [^:mut ^std::unordered_map<K|V> map ^K key]
   (doto map
     (.erase key)))
  (^{:tpl [K V C]} [^:mut ^std::map<K|V|C> map ^K key]
   (doto map
     (.erase key))))

(defn update!
  (^{:tpl [K V F ...Args]} [^:mut ^std::unordered_map<K|V> m ^K k ^F f ^:mut ^Args&&... args]
   (assoc! m k (f (get m k) ^Args ^:... (std::forward args))))
  (^{:tpl [K V C F ...Args]} [^:mut ^std::map<K|V|C> m ^K k ^F f ^:mut ^Args&&... args]
   (assoc! m k (f (get m k) ^Args ^:... (std::forward args)))))

(defn contains?
  (^{:tpl [K]} [^std::unordered_set<K> coll ^K key]
   (= (.count coll key) 1))
  (^{:tpl [K C]} [^std::set<K|C> coll ^K key]
   (= (.count coll key) 1))
  (^{:tpl [K V]} [^std::unordered_map<K|V> coll ^K key]
   (= (.count coll key) 1))
  (^{:tpl [K V C]} [^std::map<K|V|C> coll ^K key]
   (= (.count coll key) 1))
  ([coll key]
   (.contains coll key)))

(defn count
  (^{:tpl [T1 T2]} [^std::pair<T1|T2> _]
   2)
  ([coll]
   (.size coll)))

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
  (^{:tpl [T]} [^ConsList<T> coll]
   (let [tail (next coll)]
     (if (empty? tail)
       (first coll)
       (recur tail))))
  ([coll]
   (.back coll)))

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

(defn comp
  (^{:tpl [F1 F2]} [^F1 f1 ^F2 f2]
   (fn [^auto... args]
     (f1 (f2 args...)))))

(defn complement [f]
  (fn [^auto... args]
    (not (f args...))))

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
    y
    x))

(defn true? [x]
  (= true x))

(defn false? [x]
  (= false x))

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

(defn reduce
  ([f coll]
   (let [^:mut acc (first coll)
         ^:mut first? true]
     (doseq [x coll]
       (if first?
         (set! first? false)
         (set! acc (f acc x))))
     acc))
  ([f ^:mut val coll]
   (doseq [x coll]
     (set! val (f val x)))
   val))

(defn into!
  (^{:tpl [T F]} ^T [^:mut ^T to ^F from]
   (reduce conj! to from))
  (^{:tpl [T F]} ^T [^:mut ^T&& to ^F from]
   (reduce conj! to from)))

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
   (let [xs args...]
     (into! ^"K,V" (std::unordered_map.) xs))))

(defn sorted-map
  (^{:tpl [K V]} []
   ^"K,V" (std::map.))
  (^{:tpl [K V ...Args]} [^Args&... args]
   (let [xs args...]
     (into! ^"K,V" (std::map.) xs))))

(defn sorted-map-by
  (^{:tpl [K V C]} [^C comp]
   ^"K,V" (std::map. comp))
  (^{:tpl [K V C ...Args]} [^C comp ^Args&... args]
   (let [xs args...]
     (into! ^"K,V,C" (std::map. comp) xs))))

(defn vector ^{:tpl [T ...Args]} [^:mut ^Args&&... args]
  ^T (std::vector. ^Args ^:... (std::forward args)))

(defn list
  (^{:tpl [T]} []
   ^T (ConsList. nullptr 0))
  (^{:tpl [T Arg ...Args]} [^:mut ^Arg&& arg ^:mut ^Args&&... args]
   ^T (cons ^Arg (std::forward arg) ^T (list ^Args ^:... (std::forward args)))))

(defn set ^{:tpl [T]} [^T coll]
  (into! ^"typename T::value_type" #{} coll))

(defn vec ^{:tpl [T]} [^T coll]
  (into! ^"typename T::value_type" [] coll))

(defn concat
  ([x] x)
  ([x y]
   (into! (vec x) y)))

(defn map
  (^{:tpl [F C]} [^F f ^C coll]
   (let [^:mut acc ^"decltype(f(std::declval<typename C::value_type>()))" []]
     (.reserve acc (count coll))
     (doseq [x coll]
       (conj! acc (f x)))
     acc))
  (^{:tpl [F C1 C2]} [^F f ^C1 c1 ^C2 c2]
   (let [^:mut acc ^"decltype(f(std::declval<typename C1::value_type>(), std::declval<typename C2::value_type>()))" []
         ^:mut c1-it (.begin c1)
         ^:mut c2-it (.begin c2)]
     (.reserve acc (min (count c1) (count c2)))
     (while (not (or (= c1-it (.end c1))
                     (= c2-it (.end c2))))
       ^:unsafe
       (conj! acc (f @c1-it @c2-it))
       (inc! c1-it)
       (inc! c2-it))
     acc)))

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

(defn remove ^{:tpl [P C]} [^P pred ^C coll]
  ^"typename C::value_type"
  (for [x coll
        :when (not (pred x))]
    x))

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
  (into! ^"typename C::value_type" () coll))

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

(defn interpose ^{:tpl [T C]} [^T sep ^C coll]
  (let [^:mut first? true
        ^:mut acc ^T []]
    (doseq [x coll]
      (if first?
        (set! first? false)
        (conj! acc sep))
      (conj! acc x))
    acc))

(defn interleave ^{:tpl [C1 C2]} [^C1 c1 ^C2 c2]
  (let [^:mut acc ^"typename C1::value_type" []
        ^:mut c1-it (.begin c1)
        ^:mut c2-it (.begin c2)]
    (.reserve acc (+ (count c1) (count c2)))
    ^:unsafe
    (while (not (or (= c1-it (.end c1))
                    (= c2-it (.end c2))))
      (conj! acc @c1-it)
      (conj! acc @c2-it)
      (inc! c1-it)
      (inc! c2-it))
    acc))

(defn partition ^{:tpl [C]} [^std::size_t n ^C coll]
  (let [^:mut idx 0
        ^:mut acc ^"std::vector<typename C::value_type>" []
        ^:mut current ^"typename C::value_type" []]
    (doseq [x coll]
      (conj! current x)
      (when (mod idx n)
        (conj! acc current)
        (set! current ^"typename C::value_type" []))
      (set! idx (inc idx)))
    acc))

(defn zipmap ^{:tpl [K V]} [^K keys ^V vals]
  (let [^:mut acc ^"typename K::value_type,typename V::value_type" {}
        ^:mut k-it (.begin keys)
        ^:mut v-it (.begin vals)]
    ^:unsafe
    (while (not (or (= k-it (.end keys))
                    (= v-it (.end vals))))
      (assoc! acc @k-it @v-it)
      (inc! k-it)
      (inc! v-it))
    acc))

(defn select-keys ^{:tpl [M K]} [^M m ^K keys]
  (let [^:mut acc ^"typename M::key_type,typename M::mapped_type" {}]
    (doseq [k keys
            :when (contains? m k)]
      (assoc! acc k (get m k)))
    acc))

(defn repeatedly ^{:tpl [F]} [^std::size_t n ^F f]
  (let [^:mut acc ^"decltype(f())" []]
    (.reserve acc n)
    (dotimes [_ n]
      (conj! acc (f)))
    acc))

(defn repeat [n x]
  (repeatedly n (constantly x)))

(defn keys [m]
  (map key m))

(defn vals [m]
  (map val m))

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

(defn inst-ms [d]
  (.count (.time_since_epoch ^std::chrono::milliseconds (std::chrono::time_point_cast d))))

(defn subs
  ([s start]
   (.substr s start))
  ([s start end]
   (.substr s start (- end start))))

(defn print-container [^:mut ^std::ostream out coll]
  (let [^:mut first? true]
    (doseq [x coll]
      (if first?
        (set! first? false)
        (<< out " "))
      (<< out x))))

(defn print-map [^:mut ^std::ostream out map]
  (let [^:mut first? true]
    (doseq [kv map]
      (if first?
        (set! first? false)
        (<< out " "))
      (<< out (key kv) " " (val kv)))))

(defmethod operator<< ^{:tpl [T]} [^:mut ^std::ostream out ^ConsList<T> list]
  (<< out "(")
  (print-container out list)
  (<< out ")"))

(defmethod operator<< ^{:tpl [T1 T2]} [^:mut ^std::ostream out ^std::pair<T1|T2> pair]
  (<< out "(" (first pair)  " . " (second pair) ")"))

(defmethod operator<< ^{:tpl [T]} [^:mut ^std::ostream out ^std::vector<T> vector]
  (<< out "[")
  (print-container out vector)
  (<< out "]"))

(defmethod operator<< ^{:tpl [T]} [^:mut ^std::ostream out ^std::unordered_set<T> set]
  (<< out "#{")
  (print-container out set)
  (<< out "}"))

(defmethod operator<< ^{:tpl [T]} [^:mut ^std::ostream out ^std::set<T> set]
  (<< out "#{")
  (print-container out set)
  (<< out "}"))

(defmethod operator<< ^{:tpl [K V]} [^:mut ^std::ostream out ^std::unordered_map<K|V> map]
  (<< out "{")
  (print-map out map)
  (<< out "}"))

(defmethod operator<< ^{:tpl [K V]} [^:mut ^std::ostream out ^std::map<K|V> map]
  (<< out "{")
  (print-map out map)
  (<< out "}"))

(defn str
  ([]
   "")
  ([^std::string arg]
   arg)
  (^{:tpl [Arg]} [^:mut ^Arg&& arg]
   (let [^:mut out (std::ostringstream.)]
     (<< out std::boolalpha ^Arg (std::forward arg))
     (.str out)))
  (^{:tpl [Arg ...Args]} [^:mut ^Arg&& arg ^:mut ^Args&&... args]
   (+ (str ^Arg (std::forward arg))
      (str ^Args ^:... (std::forward args)))))

(def ^:mut *test-vars* ^"std::function<void()>" [])
(def ^:dynamic *testing-contexts* "")
(def ^:mut *report-counters* ^std::string|int {:pass 0 :fail 0})

(defn run-all-tests []
  (doseq [f *test-vars*]
    (f))
  (binding [*out* *err*]
    (println)
    (println "Ran" (count *test-vars*) "tests containing"
             (+ (get *report-counters* :pass)
                (get *report-counters* :fail)) "assertions.")
    (println (get *report-counters* :fail) "failures.")
    (if (pos? (get *report-counters* :fail))
      1
      0)))

(defn register-test! [^"std::function<void()>" test]
  (conj! *test-vars* test)
  test)

(defn assert-predicate ^void [msg expected actual actual-str]
  ^:unsafe (if actual
             (update! *report-counters* :pass inc)
             (binding [*out* *err*]
               (println)
               (print msg)
               (println *testing-contexts*)
               (println "expected:" expected)
               (println "  actual: " actual-str)
               (println actual)
               (update! *report-counters* :fail inc))))
