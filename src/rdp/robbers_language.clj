(ns rdp.robbers-language (:gen-class))

;; https://www.reddit.com/r/dailyprogrammer/comments/341c03/20150427_challenge_212_easy_r%C3%B6varspr%C3%A5ket/

(def consonants (set "bcdfghjklmnpqrstvwxz"))
(defn consonant? [c] (not (nil? (consonants (java.lang.Character/toLowerCase c)))))

(defn encode-robbers-language
  [text]
  (clojure.string/join "" 
                       (mapcat (fn [c] 
                                 (if (consonant? c)
                                   [c \o (java.lang.Character/toLowerCase c)]
                                   [c])) text)))

(defn decode-robbers-language 
  [encoded]
  (loop [decoded []
         remaining encoded]
    (if (empty? remaining)
      (clojure.string/join "" decoded)
      (let [c (first remaining)
            decoded (conj decoded c)
            remaining (if (consonant? c) (drop 3 remaining) (rest remaining))]
        (recur decoded remaining)))))
 
