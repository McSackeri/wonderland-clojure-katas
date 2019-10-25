(ns card-game-war.game)

;; feel free to use these cards or use your own data structure
(def suits [:spade :club :diamond :heart])
(def ranks [2 3 4 5 6 7 8 9 10 :jack :queen :king :ace])
(def cards
  (apply vector
         (for [suit suits
               rank ranks]
           [suit rank])))

(def card-values (array-map 1 1 2 2 3 3 4 4 5 5 6 6 7 7 8 8 9 9 10 10
                            :jack 11 :queen 12 :king 13 :ace 14))

(defn shuffle-cards
  "Quick and dirty shuffling algorithm.  Splits the vector at random
  indexes, and reverses the order.  Recursively calls itself based on
  the amount of 'times' given.  Seems to take approx 2-3X deck size to
  get a good shuffled vector."
  [deck times]
  (let [rand-index (rand-int (- (count deck)))]
    (let [rand-cards (reduce conj
                             (into [] (reverse (subvec deck rand-index)))
                             (subvec deck 0 rand-index))]
      (if (> times 0)
        (shuffle-cards rand-cards (dec times))
        rand-cards))))

(defn post-round-shift
  "puts the first card of the winner deck at the end and adds the loser card"
  [winner-cards loser-cards]
  (reduce conj
          (subvec winner-cards (count loser-cards))
          (into (subvec winner-cards 0 (count loser-cards))
                loser-cards)))

(defn play-round [player1-card player2-card]
  (let [p1-card-value (get card-values (nth player1-card 1))
       p2-card-value (get card-values (nth player2-card 1))]
  (cond
    (= p1-card-value p2-card-value) -1
    (> p1-card-value p2-card-value) 0
    :else 1)))

(defn play-game [player1-cards player2-cards]
  (cond
    (and (empty? player1-cards) (empty? player2-cards)) (println "draw!")
    (empty? player1-cards) (println "player 2 wins!")
    (empty? player2-cards) (println "player 1 wins!")
    :else ; play the rounds
    (let [round-result (play-round (nth player1-cards 0)
                                   (nth player2-cards 0))]
      (cond 
        (= round-result 0) (play-game (post-round-shift
                                       player1-cards [(nth player2-cards 0)])
                                      (subvec player2-cards 1))
        (= round-result 1) (play-game (subvec player1-cards 1)
                                      (post-round-shift
                                       player2-cards
                                       [(nth player1-cards 0)]))
        (= round-result -1)
        (loop [war-count 0]
          (cond
            (and (< (+ war-count (count player1-cards)) 3)
                 (< (+ war-count (count player2-cards)) 3))
            (play-game [] []) ; draw
            (< (+ war-count (count player1-cards)) 3) ; player2 win
            (play-game [] (post-round-shift player2-cards player1-cards))
            (< (+ war-count (count player2-cards)) 3) ; player1 win
            (play-game (post-round-shift player1-cards player2-cards []))
            :else ;war
            (if (or (> war-count (count player1-cards) (count player2-cards)))
              (let [war-round (play-round (nth player1-cards (+ war-count 2))
                                          (nth player2-cards (+ war-count 2)))]
                (cond
                  (= war-round -1) (recur (+ war-count 2))
                  (= war-round 0)
                  (play-game (post-round-shift
                              player1-cards (nth player2-cards 0 war-count))
                             (subvec player2-cards war-count))
                  (= war-round 1)
                  (play-game (subvec player2-cards war-count)
                             (post-round-shift
                              player1-cards (nth player2-cards 0 war-count))))
                ))))))))

; Testing junk for REPL

(def test-deck-a [[:spade :queen] [:diamond 4] [:heart 10] [:spade 5]])
(def test-deck-b [[:diamond 8] [:diamond :queen] [:spade :king] [:heart 9]])

(defn shuffle-test
  [vec times]
  (let [r (rand-int (- (count vec) 1))]
    (let [rand-vec (reduce conj
                           (into [] (reverse (subvec vec r)))
                           (subvec vec 0 r))]
      (if (> times 0)
        (do
          (println times r rand-vec)
          (shuffle-test rand-vec (dec times)))
        rand-vec))))
