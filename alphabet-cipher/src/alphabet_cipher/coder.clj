(ns alphabet-cipher.coder)

(defn index-num
  "returns the alpha index value based on integer unicode value"
  [letter]
  (- (int letter) (int \a)))

(def row-vec (apply vector (seq "abcdefghijklmnopqrstuvwxyz")))

(defn column-vec 
  "returns column vector.  Shifts row-vec order based on letter.  "
  [letter]
  (reduce conj
          (subvec row-vec (index-num letter))
          (subvec row-vec 0 (index-num letter))))

(defn letter-fill
  "Repeats string until reaches length provided, and truncates if needed."
  [keyword msg-length]
  (let [kw (apply str (repeat (int (Math/ceil (/ msg-length (count keyword)))) keyword))]
     (subs kw 0 (min (count kw) msg-length))))

(defn encode-letter
  "returns encoded letter from based on each keyword and message"
  [kw-letter msg-letter]
  (nth (column-vec msg-letter) (index-num kw-letter)))

(defn decode-letter
  "returns the decoded letter based on each keyword and message"
  [kw-letter msg-letter]
  (nth row-vec (.indexOf (column-vec kw-letter) msg-letter)))


(defn encode [keyword message]
  "encodeme"
  (apply str (map encode-letter (seq message) (seq (letter-fill keyword (count message))))))

(defn decode [keyword message]
  "decodeme"
  (apply str (map decode-letter (seq (letter-fill keyword (count message))) (seq message))))

(defn decipher [cipher message]
  "decypherme"
  ; admittedly there is a bug in this.  Words that are made up of repeatable
  ; characters don't work with this.  Ex. 'booboo' 'haha'
  (let [full-kw (apply str (map decode-letter (seq message) (seq cipher)))]
    (loop [x 1]
      (let [part-kw (.substring full-kw 0 x)]
      (if (= cipher (encode part-kw message))
        (str part-kw)
        (recur (inc x)))))))

