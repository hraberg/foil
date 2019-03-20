(ns foil.core)

(def ^"std::plus<>" +)
(def ^"std::minus<>" -)
(def ^"std::multiplies<>" *)
(def ^"std::divides<>" /)
(def ^"std::modulus<>" %)

(def ^"std::equal_to<>" =)
(def ^"std::not_equal_to<>" !=)
(def ^"std::greater<>" >)
(def ^"std::less<>" <)
(def ^"std::greater_equal<>" >=)
(def ^"std::less_equal<>" <=)

(def ^"std::logical_and<>" &&)
(def ^"std::logical_or<>" ||)
(def ^"std::logical_not<>" !)

(def ^"std::bit_and<>" &)
(def ^"std::bit_or<>" |)
($code "const std::bit_or<> _CARET_;")
($code "const std::bit_not<> _TILDE_;")

(defn map [^:ref f ^:ref coll]
  (let [^:mut acc ^"decltype(f(std::declval<typename __T_coll::value_type>()))" []]
    (doseq [x coll]
      (.push_back acc (f x)))
    acc))

(defn filter [^:ref pred ^:ref coll]
  (let [^:mut acc ^"typename __T_coll::value_type" []]
    (doseq [x coll]
      (when (pred x)
        (.push_back acc x)))
    acc))

(defn reduce
  ([^:ref f ^:ref coll]
   (let [^:mut acc (.front coll)
         ^:mut first true]
     (doseq [x coll]
       (if first
         (set! first false)
         (set! acc (f acc x))))
     acc))
  ([^:ref f ^:ref val ^:ref coll]
   (let [^:mut acc val]
     (doseq [x coll]
       (set! acc (f acc x)))
     acc)))
