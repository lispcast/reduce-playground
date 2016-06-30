(ns reduce-playground.core)

(defn reduce* [f i coll]
  (if (empty? coll)
    i
    (let [[fst & rst] coll]
      (recur f (f i fst) rst))))

(defn map* [f ls]
  (reduce (fn [res v]
            (conj res (f v)))
    [] ls))

(defn filter* [f ls]
  (reduce (fn [res v]
            (if (f v)
              (conj res v)
              res))
    [] ls))

(reduce-kv (fn [[kcount vcount] k v]
             [(+ kcount (count k))
              (+ vcount (count v))])
  [0 0] {"Eric" "Normand"
         "John" "Doe"
         "Jane" "Marshall"})

(reduce-kv (fn [m k v]
             (assoc m k v))
  {} (vec "abcdefghij"))

(reductions (fn [[kcount vcount] [k v]]
             [(+ kcount (count k))
              (+ vcount (count v))])
  [0 0] {"Eric" "Normand"
         "John" "Doe"
         "Jane" "Marshall"})

(defn plus
  ([] [0 0])
  ([avg] avg)
  ([[n1 d1] [n2 d2]]
   [(+ n1 n2)
    (+ d1 d2)]))

(defn avg [number]
  [number 1])

(defn average [numbers]
  (reduce plus [0 0] (map avg numbers)))

(defn longer-than? [n coll]
  (> (reduce (fn [res _]
             (let [res (inc res)]
               (if (> res n)
                 (reduced res)
                 res)))
       0 coll)
    n))

(longer-than? 3 (range))







