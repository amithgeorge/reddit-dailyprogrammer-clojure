(ns rdp.214-intermediate-arr
  (:require [clojure.java.io :as io]))

(defn- read-line-nums
  ([] (read-line-nums (read-line) #(Long/parseLong %1)))
  ([input-line parse-fn]
   (if-let [line input-line]
     (->> line
          (#(clojure.string/split %1 #" "))
          (map parse-fn)))))

(defrecord Paper [^long color 
                  ^long x1  ^long y1
                  ^long x2 ^long y2])

(defrecord Canvas [^long width ^long height])

(defn- make-paper
  ([^long w ^long h] (make-paper [0 0 0 w h]))
  ([^longs [color x y w h]]
   (Paper. color x y (+ x w -1) (+ y h -1))))

(defn- parse-inputs
  []
  (let [canvas (apply ->Canvas (read-line-nums))]
    (loop [papers (list (make-paper (:width canvas) (:height canvas)))]
      (if-let [values (read-line-nums)]
        (recur (conj papers (make-paper values)))
        {:papers (into [] papers)
         :canvas canvas
         :canvas-width (:width canvas)
         :canvas-height (:height canvas)}))))

(defn- read-input-file 
  [file-name]
  (do
    (println "parsing file into records")
    (time (with-open 
           [rdr (io/reader (io/resource file-name))]
            (binding [*in* rdr]
              (parse-inputs))))))

(def ^:private input-files
  ["100rects100x100.in"
   "100rects10Kx10K.in"
   "100rects3Kx3K.in"])

(defn- covered?
  [[^long canvas-x ^long canvas-y] paper]
  (and (<= (:x1 paper) canvas-x (:x2 paper)) 
       (<= (:y1 paper) canvas-y (:y2 paper))))

(defn- visible-color
  [^longs coord papers]
  (some #(when (covered? coord %1) (:color %1))
        papers))

(defn- visible-color-frequencies
  [{:keys [canvas papers]}]
  (persistent!
   (reduce 
    (fn [acc ^longs coord]
      (if-let [color (visible-color coord papers)]
        (assoc! acc color (inc (get acc color 0)))
        acc))
    (transient {})
    (for [^long y (range (:height canvas))
          ^long x (range (:width canvas))]
      [x y]))))

(defn- solve
  [input-file]
  (let [input (read-input-file input-file)
        color-map (visible-color-frequencies input)
        sorted (sort-by key color-map)]
    (doseq [line sorted]
      (println (key line) (val line)))))

(defn -main 
  ([] (-main "0"))
  ([index] 
   (time 
    (binding [*unchecked-math* :warn-on-boxed
              *warn-on-reflection* true] 
      (solve (input-files (Integer/parseInt index)))))))
