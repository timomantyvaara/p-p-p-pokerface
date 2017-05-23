(ns p-p-p-pokerface)

(def symbolcards {\T 10, \J 11, \Q 12, \K 13, \A 14})

(defn high-card? [hand]
  true) ; All hands have a high card.

(defn rank [card]
  (let [[rank _] card]
    (if (Character/isDigit rank)
      (Integer/valueOf (str rank))
      (symbolcards rank))))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn rank-freq [hand]
  (sort (vals (frequencies (map rank hand)))))

(defn pair? [hand]
  (= 2 (apply max (rank-freq hand))))

(defn three-of-a-kind? [hand]
  (= 3 (apply max (rank-freq hand))))

(defn four-of-a-kind? [hand]
  (= 4 (apply max (rank-freq hand))))

(defn flush? [hand]
  (= 1 (count (keys (frequencies (map suit hand))))))

(defn full-house? [hand]
  (=
    (seq [2 3])
    (rank-freq hand)))

(defn two-pairs? [hand]
  (or
    (four-of-a-kind? hand)
    (= (seq [1 2 2]) (rank-freq hand))))

(defn straight? [hand]
  (let [ranks (sort (seq (map rank hand)))
        ranks-low-ace (sort (replace {14 1} ranks))
        hand-range (fn [ranks] (range (apply min ranks) (+ 1 (apply max ranks))))]
    (or
      (= (hand-range ranks) ranks)
      (= (hand-range ranks-low-ace) ranks-low-ace))))

(defn straight-flush? [hand]
  (and
    (straight? hand)
    (flush? hand)))

(defn value [hand]
  (let [checkers #{[high-card? 0]
                   [pair? 1]
                   [two-pairs? 2]
                   [three-of-a-kind? 3]
                   [straight? 4]
                   [flush? 5]
                   [full-house? 6]
                   [four-of-a-kind? 7]
                   [straight-flush? 8]}
        match? (fn [checker] ((first checker) hand))
        all-hand-values (map second (filter match? checkers))]
    (apply max all-hand-values)))
