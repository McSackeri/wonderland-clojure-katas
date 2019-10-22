(ns card-game-war.game-test
  (:require [clojure.test :refer :all]
            [card-game-war.game :refer :all]))


;; fill in  tests for your game
(deftest test-play-round
  (testing "the highest rank wins the cards in the round"
    (is (= 0
           (play-round [:spade 8] [:diamond 2])))
    (is (= 1
           (play-round [:club 5] [:heart 6]))))
  (testing "queens are higher rank than jacks"
    (is (= 1
           (play-round [:heart :jack] [:spade :queen]))))
  (testing "kings are higher rank than queens"
    (is (= 1
           (play-round [:heart :queen] [:spade :king]))))
  (testing "aces are higher rank than kings"
    (is (= 1
           (play-round [:heart :king] [:diamond :ace]))))
  (testing "if the ranks are equal, clubs beat spades"
    (is (= 0
           (play-round [:club :king] [:spade :king]))))
  (testing "if the ranks are equal, diamonds beat clubs"
    (is (= 0
           (play-round [:diamond 3] [:club 3]))))
  (testing "if the ranks are equal, hearts beat diamonds"
    (is (= 0
           (play-round [:heart 3] [:diamond 3]))))
)

(deftest test-play-game
  (testing "the player loses when they run out of cards"))

