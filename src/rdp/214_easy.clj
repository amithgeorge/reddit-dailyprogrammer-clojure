(ns rdp.214-easy)

;; https://www.reddit.com/r/dailyprogrammer/comments/35l5eo/20150511_challenge_214_easy_calculating_the/

(defn- mean
  [& nums]
  (/ (apply + nums) (count nums)))

(defn- parse-input
  [input-str]
  (->> (clojure.string/split input-str #" ")
       (map #(Integer/parseInt %))))

(defn standard-deviation
  [& nums]
  (let [mean (apply mean nums)]
    (->> nums
         (map #(- mean %))
         (map #(Math/pow % 2))
         (apply +)
         (#(/ %1 (count nums)))
         (Math/sqrt))))

(defn -main
  []
  (let [inputs ["5 6 11 13 19 20 25 26 28 37"
                "37 81 86 91 97 108 109 112 112 114 115 117 121 123 141"
                "266 344 375 399 409 433 436 440 449 476 502 504 530 584 587"
                "809 816 833 849 851 961 976 1009 1069 1125 1161 1172 1178 1187 1208 1215 1229 1241 1260 1373"]
        results (->> inputs
                     (map parse-input)
                     (map #(apply standard-deviation %1)))
        result-lines (map vector inputs results)] 
    (doseq [line result-lines]
      (println (line 0))
      (println (format "%.4f" (line 1)))
      (println "")
      (println ""))))
