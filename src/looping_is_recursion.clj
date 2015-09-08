(ns looping-is-recursion)

; for non-zero integer exp only
(defn power [base exp]
  (let [helper (fn [acc n]
                 (cond
		   (zero? n) 1
		   (= n 1) acc
		   :else (recur (* acc base) (dec n))))]
    (helper base exp)))

(defn last-element [a-seq]
  (cond
    (zero? (count a-seq)) nil
    (= 1 (count a-seq)) (first a-seq)
    :else (recur (rest a-seq))))

(defn seq= [seq1 seq2]
  (cond
    (and (empty? seq1) (empty? seq2)) true
    (or (empty? seq1) (empty? seq2)) false
    (not (= (first seq1) (first seq2))) false
    :else (recur (rest seq1) (rest seq2))))

(defn find-first-index [pred a-seq]
  (loop [index 0 seq a-seq]
    (cond
      (empty? seq) nil
      (pred (first seq)) index
      :else (recur (inc index) (rest seq)))))

(defn avg [a-seq]
  (loop [index 0 sum 0 seq a-seq]
    (if (empty? seq)
      (/ sum index)
      (recur (inc index) (+ sum (first seq)) (rest seq)))))

(defn parity [a-seq]
  (loop [seq a-seq result #{}]
    (if (empty? seq)
      result
      (recur (rest seq)
             (if (contains? result (first seq))
	       (disj result (first seq))
	       (conj result (first seq)))))))

(defn fast-fibo [n]
  (cond
    (= n 0) 0
    (= n 1) 1
    :else (loop [index n fn-1 0 fn 1]
      (if (zero? index)
        fn-1
	(recur (dec index) fn (+ fn-1 fn))))))

(defn cut-at-repetition [a-seq]
  (loop [seq a-seq result '()]
    (if (or (empty? seq) (contains? (set result) (first seq)))
      (reverse result)
      (recur (rest seq) (conj result (first seq))))))


