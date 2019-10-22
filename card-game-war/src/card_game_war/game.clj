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

(def face-values (array-map :spade 0 :club 1 :diamond 2 :heart 3))

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

(defn play-round [player1-card player2-card]
  (cond
    (= (get card-values (nth player1-card 1))
       (get card-values (nth player2-card 1)))
    (if (> (get face-values (nth player1-card 0))
           (get face-values (nth player2-card 0)))
      0
      1)
    (> (get card-values (nth player1-card 1))
       (get card-values (nth player2-card 1)))
    0
    :else 1)
)

(defn play-game [player1-cards player2-cards])



