(ns spbau.clojure.ordian.One)

;; 1
(defn call-twice [f a]
  (dotimes [_ 2] (f a)))

(call-twice println "printed twice")

;; 2
(defn read-and-print [file]
  (doto (slurp file)
    (println)))

(read-and-print "One.clj")

;; 3
(defn cube-anonymous [n]
  (let [f #(-> (* (* n n) n))]
    (f)))

(assert (= 8 (cube-anonymous (+ 1 1))))

;; 4
(defn reverse-and-concat [coll1 coll2]
  (concat (reverse coll1) (reverse coll2)))

(println (reverse-and-concat [1 2 3] [4 5 6]))

;; 5
(defn contains-value? [coll value]
  (some #(= value %) coll))

(assert (contains-value? [1 2 3] 3))

;; 6
(defn print-distinct-pairs [coll1 coll2]
  (doseq [p (distinct (for [x coll1
                            y coll2]
                        [x y]))]
    (println p)))

(print-distinct-pairs [1, 1, 2] [3, 3, 5])

;; 7
;(println (repeat 4 3))
(defn my-repeat [n x]
  (loop [i n acc []]
    (if (not (pos? i))
      acc
      (recur (dec i) (conj acc x)))))

(println (my-repeat 4 3))