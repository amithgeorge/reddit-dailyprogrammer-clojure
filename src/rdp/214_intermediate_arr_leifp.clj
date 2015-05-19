(ns rdp.214-intermediate-arr-leifp
  (:require [clojure.java.io :as io]))

;; https://groups.google.com/d/msg/clojure/JgxFQLP2E34/xRPAxJeGZ94J

(defn- read-line-nums
  ([] (read-line-nums (read-line) #(Long/parseLong %1)))
  ([input-line parse-fn]
   (if-let [line input-line]
     (->> line
          (#(clojure.string/split %1 #" "))
          (map parse-fn)))))

(def BLOCKS 10)

(defrecord Paper [^long color 
                  ^long x1  ^long y1
                  ^long x2 ^long y2])

(defrecord Canvas [^long width ^long height])

(defn- make-paper
  ([^long w ^long h] (make-paper [0 0 0 w h]))
  ([^longs [color x y w h]]
   (Paper. color x y (+ x w -1) (+ y h -1))))

;; (defn- make-paper
;;   ([w h] (make-paper 0 0 0 w h))
;;   ([color x y w h]
;;    {:color color
;;     :x1 x
;;     :y1 y
;;     :w w
;;     :h h
;;     :x2 (dec (+ x w))
;;     :y2 (dec (+ y h))}))

(defn- intersects?
  [{left-1 :x1 top-1 :y1 right-1 :x2 bottom-1 :y2 :as rect1}
   {left-2 :x1 top-2 :y1 right-2 :x2 bottom-2 :y2 :as rect2}]
  (not
   (or (> left-1 right-2) ; 1 to right of 2
       (< right-1 left-2) ; 1 to left of 2
       (> top-1 bottom-2) ; 1 below 2
       (< bottom-1 top-2) ; 1 above 2
       )))

(defn- coarse-index [canvas x y]
  (let [x-step (int (Math/ceil (/ (:width canvas) (float BLOCKS))))
        y-step (int (Math/ceil (/ (:height canvas) (float BLOCKS))))
        x-idx (quot x x-step)
        y-idx (quot y y-step)]
    (+ (* BLOCKS y-idx) x-idx)))

(defn- subdiv-rect [canvas x-idx y-idx]
  (let [x-step (int (Math/ceil (/ (:width canvas) BLOCKS)))
        y-step (int (Math/ceil (/ (:height canvas) BLOCKS)))]
    (make-paper [0 (* x-idx x-step) (* y-idx y-step) x-step y-step])))

(defn- make-paper-index [canvas papers]
  ;; (println canvas papers)
  (reduce
   (fn [index [xi yi]]
     (let [rect (subdiv-rect canvas xi yi)]
       (assoc index
              (+ (* BLOCKS yi) xi)
              (filterv #(intersects? rect %) papers))))
   {}
   (for [xi (range BLOCKS) yi (range BLOCKS)] [xi yi])))

(defn- parse-inputs
  []
  (let [canvas (apply ->Canvas (read-line-nums))]
    (loop [papers (list (make-paper (:width canvas) (:height canvas)))
           colors #{0}]
      (if-let [values (read-line-nums)]
        (recur (conj papers (make-paper values))
               (conj colors (first values)))
        {:papers (into [] papers)
         :colors colors
         :canvas canvas}))))

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
  (let [^long x1 (:x1 paper)
        ^long x2 (:x2 paper)
        ^long y1 (:y1 paper)
        ^long y2 (:y2 paper)]
    (and (<= x1 canvas-x)
         (<= canvas-x x2)
         (<= y1 canvas-y)
         (<= canvas-y y2))))

;; (defn- visible-color
;;   [^longs coord papers]
;;   (some #(when (covered? coord %1) (:color %1))
;;         papers))

(defn- visible-color
  [coord canvas paper-index]
  ;; (println "visible color -- " coord canvas paper-index)
  (let [i (apply coarse-index canvas coord)
        papers (get paper-index i [])]
    (some #(when (covered? coord %1) (:color %1))
          papers)))

(defn- visible-color-frequencies
  [{:keys [canvas paper-index papers]}]
  (persistent!
    (reduce 
      (fn [acc ^longs coord]
        (if-let [color (visible-color coord canvas paper-index)]
          (assoc! acc color (inc (get acc color 0)))
          acc))
      (transient {})
      (for [^long y (range (:height canvas))
            ^long x (range (:width canvas))]
        [x y]))))

(defn- visible-color-frequencies-arr
  [{:keys [colors canvas papers paper-index]}]
  (let [colorCounts (long-array (count colors))
        ] 
    (reduce 
     (fn [_ ^longs coord]
       (if-let [color (visible-color coord canvas paper-index)]
         (aset colorCounts color (+ 1 (aget colorCounts color)))
         _))
     -1
     (for [^long y (range (:height canvas))
           ^long x (range (:width canvas))]
       [x y]))
    (zipmap (range) colorCounts)))

(defn- solve
  [input-file]
  (let [input (read-input-file input-file)
        input (assoc input
                     :paper-index
                     (make-paper-index (:canvas input) (:papers input)))
        color-map (visible-color-frequencies input)
        sorted (sort-by key color-map)]
    (doseq [line sorted]
      (println (key line) (val line)))))

(defn- solve-arr
  [input-file]
  (let [input (read-input-file input-file)
        input (assoc input
                     :paper-index
                     (make-paper-index (:canvas input) (:papers input)))
        color-map (visible-color-frequencies-arr input)
        sorted (sort-by key (remove #(zero? (val %1)) color-map))]
    (doseq [line sorted]
      (println (key line) (val line)))))

(defn -main 
  ([] (-main "0" "false"))
  ([index use-arrays] 
   (time 
    (binding [*unchecked-math* :warn-on-boxed
              *warn-on-reflection* true] 
      (if-not (Boolean/parseBoolean use-arrays) 
        (solve (input-files (Integer/parseInt index)))
        (solve-arr (input-files (Integer/parseInt index))))))))
