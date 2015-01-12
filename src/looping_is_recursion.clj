(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [a b]
                 (if (zero? b)
                   a
                   (recur (* a base) (dec b))))]
    (helper 1 exp)))

(defn last-element [a-seq]
  (let [helper (fn [a-seq n]
                 (if (<= (count a-seq) 1)
                   (first a-seq)
                   (recur (rest a-seq) (dec n))))]
        (helper a-seq (count a-seq))))

(defn seq= [seq1 seq2]
  (let [helper (fn [a-seq b-seq]
                 (cond
                  (and (empty? a-seq) (empty? b-seq)) true
                  (or (empty? a-seq) (empty? b-seq)) false
                  (= (first a-seq) (first b-seq)) (recur (rest a-seq) (rest b-seq))
                  :else false))]
    (helper seq1 seq2)))

(defn find-first-index [pred a-seq]
  (loop [acc 0 seq a-seq]
    (cond
     (empty? seq)
      nil
     (pred (first seq))
      acc
     :else
     (recur (inc acc) (rest seq)))))

(defn avg [a-seq]
  (loop [acc 0 sum 0 seq a-seq]
    (cond
     (empty? seq)
      (/ sum acc)
     :else
     (recur (inc acc) (+ sum (first seq)) (rest seq)))))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (loop [l-seq a-seq
         b-set #{}]
  (if (empty? l-seq)
    b-set
    (recur (rest l-seq) (toggle b-set (first l-seq))))))

(defn fast-fibo [n]
  (loop [acc 0
         fN 0
         fN1 1]
    (if (== acc n)
      fN
      (recur (inc acc) (+ fN fN1) fN))))


(defn cut-at-repetition [a-seq]
   (loop [loop-seq a-seq
          a-set #{}
          return-seq ()]
     (if (empty? loop-seq)
       (reverse return-seq)
     (if (contains? a-set (first loop-seq))
       (reverse return-seq)
       (recur (rest loop-seq) (conj a-set (first loop-seq)) (conj return-seq (first loop-seq)))))))

