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
    (str suit)
    ))

(defn pair? [hand]
  (< 1 (apply max (vals (frequencies (map rank hand))))))

(defn three-of-a-kind? [hand]
  (< 2 (apply max (vals (frequencies (map rank hand))))))

(defn four-of-a-kind? [hand]
  (< 3 (apply max (vals (frequencies (map rank hand))))))

(defn flush? [hand]
  (= 1 (count (keys (frequencies (map suit hand))))))

(defn full-house? [hand]
  (= (seq [2 3]) (sort (vals (frequencies (map rank hand))))))

(defn two-pairs? [hand]
  (or
    (four-of-a-kind? hand)
    (= (seq [1 2 2]) (sort (vals (frequencies (map rank hand)))))))

(defn straight? [hand]
  (let [ranks (map rank hand)
        hand-range (range (apply min ranks) (+ 1 (apply max ranks)))
        ranks-ace-one (replace {14 1} (map rank hand))
        hand-range-ace-one (range (apply min ranks-ace-one) (+ 1 (apply max ranks-ace-one)))]
    (or
      (= hand-range (sort (seq ranks)))
      (= hand-range-ace-one (sort (seq ranks-ace-one))))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}
        match? (fn [checker] ((first checker) hand))
        all-hand-values (map second (filter match? checkers))]
    (apply max all-hand-values)))
