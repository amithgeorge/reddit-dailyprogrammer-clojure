(ns rdp.pronounce-hex (:gen-class))

;; https://www.reddit.com/r/dailyprogrammer/comments/34rxkc/20150504_challenge_213_easy_pronouncing_hex/

(def ^:private char->pronunciation
  {:units {\0 ""
           \1 "one"
           \2 "two"
           \3 "three"
           \4 "four"
           \5 "five"
           \6 "six"
           \7 "seven"
           \8 "eight"
           \9 "nine"
           \a "ehh"
           \b "bee"
           \c "cee"
           \d "dee"
           \e "eee"
           \f "eff"} 
   :tens {\0 "zero"
          \1 "eleventy" 
          \2 "twenty"
          \3 "thirty"
          \4 "forty"
          \5 "fifty"
          \6 "sixty"
          \7 "seventy"
          \8 "eighty"
          \9 "ninety"
          \a "atta"
          \b "bibbity"
          \c "city"
          \d "dickety"
          \e "ebbity"
          \f "fleventy"}})

(def ^:private word-formats ["%1$s %2$s" "%1$s-%2$sbitey"])

(defn- thousands [{:keys [units tens]}]
  (if (empty? tens)
    (format "%s-bitey" units)
    (if (empty? units) 
      (format "%s-bitey" tens)
      (format "%s-%s bitey" tens units))))

(defn- tens [{:keys [units tens]}]
  (if (empty? tens) 
    (format "%s" units) 
    (if (empty? units)
      (format "%s" tens) 
      (format "%s-%s" tens units))))

(def ^:private word-formatters
  [tens thousands])

(defn- units-tens-pairs
  [str]
  (->> str
       (.toLowerCase)
       (drop 2)
       (reverse)
       (partition 2 2 nil)
       (map #(map vector [:units :tens] %1))
       (map #(into {} %))))

(defn- pronounce-pair
  ([{:keys [units tens] :as pair}]
   {:units (get-in char->pronunciation [:units units] "")
    :tens (get-in char->pronunciation [:tens tens] "")})
  ([word-formatter pair]
   (->> pair
        (pronounce-pair)
        (word-formatter)
        (clojure.string/trim))))

(defn pronounce-hex-str [str]
  (->> str
       (units-tens-pairs)
       (map pronounce-pair word-formatters)
       (reverse)
       (clojure.string/join " ")))
 
