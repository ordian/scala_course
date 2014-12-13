(ns spbau.clojure.ordian.Two)

;; 1
(defn factorial [n]
  (defmulti fact (fn [a _] a))
  (defmethod fact 0 [_, acc]
    acc)
  (defmethod fact :default [n, acc]
    (fact (dec n) (* n acc)))
  (fact n 1))

(assert (= 120 (factorial 5)))

;; 2
(defn balance [account]
  @account)

(defn increment [account amount]
  (dosync
    (commute account #(+ % amount))))

(defn decrement [account amount]
  (dosync
    (when (> amount (balance account))
      (throw (Exception. "Not enough money")))
    (alter account #(- % amount))))

(defn transfer [amount from to]
  (dosync
    (when (>= (balance from) amount)
      ;(Thread/sleep 42) ;; Do stuff
      (decrement from amount)
      (increment to amount))))

(defn test-concurrent-transfers [accounts-number, transfers-number]
  (def initial 100)
  (def accounts (take accounts-number (repeatedly #(ref initial))))
  (defn next-account [] (nth accounts (rand-int accounts-number)))
  (doall (pmap #(do (transfer % (next-account) (next-account)))
               (take transfers-number (repeatedly #(rand-int 15)))))
  (= (* accounts-number initial) (reduce + (map balance accounts))))

(assert (test-concurrent-transfers 4 100))
(println "Transfers passed")

;; 3
(defmacro my-or
  ([] nil)
  ([x] x)
  ([x & next]
    `(let [z# ~x]
       (if z#
         z#
         (my-or ~@next)))))

(defn test-fn [b]
  do
  (println b)
  b)

(or false (test-fn false) (test-fn true))

;; 3.5
(defmacro my-let [bindings & body]
  "no check"
  (`(let* ~(destructure bindings) ~@body)))
