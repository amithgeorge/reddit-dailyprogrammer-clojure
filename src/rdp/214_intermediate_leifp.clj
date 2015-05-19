(ns rdp.214-intermediate-leifp
  (:require [clojure.java.io :as io]) (:gen-class))

;; https://groups.google.com/d/msg/clojure/JgxFQLP2E34/xRPAxJeGZ94J

(defn- read-line-ints
  ([] (read-line-ints (read-line)))
  ([input-line]
   (if-let [line input-line]
     (->> line
          (#(clojure.string/split %1 #" "))
          (map #(Integer/parseInt %1))))))

(def BLOCKS 10)

(defn- make-paper
  ([w h] (make-paper 0 0 0 w h))
  ([color x y w h]
   {:color color
    :x1 x
    :y1 y
    :w w
    :h h
    :x2 (dec (+ x w))
    :y2 (dec (+ y h))}))

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
  (let [x-step (int (Math/ceil (/ (:w canvas) (float BLOCKS))))
        y-step (int (Math/ceil (/ (:h canvas) (float BLOCKS))))
        x-idx (quot x x-step)
        y-idx (quot y y-step)]
    (+ (* BLOCKS y-idx) x-idx)))

(defn- subdiv-rect [canvas x-idx y-idx]
  (let [x-step (int (Math/ceil (/ (:w canvas) BLOCKS)))
        y-step (int (Math/ceil (/ (:h canvas) BLOCKS)))]
    (make-paper 0 (* x-idx x-step) (* y-idx y-step) x-step y-step)))

(defn- make-paper-index [canvas papers]
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
  (let [[canvas-width canvas-height] (read-line-ints)]
    (loop [papers (list (make-paper canvas-width canvas-height))]
      (if-let [[color x y w h] (read-line-ints)]
        (recur (conj papers (make-paper color x y w h)))
        {:papers (into [] papers)
         :canvas-width canvas-width
         :canvas-height canvas-height}))))

(defn- read-input-file 
  [file-name]
  (with-open 
   [rdr (io/reader (io/resource file-name))]
    (binding [*in* rdr]
      (parse-inputs))))

(def ^:private input-files
  ["100rects100x100.in"
   "100rects10Kx10K.in"
   "100rects3Kx3K.in"])

(defn- covered?
  [[canvas-x canvas-y] {:keys [x1 y1 x2 y2]}]
  (and (<= x1 canvas-x x2) (<= y1 canvas-y y2)))

;; (defn- visible-color
;;   [coord papers]
;;   (some #(when (covered? coord %1) (:color %1))
;;         papers))

(defn- visible-color
  [coord canvas paper-index]
  (let [i (apply coarse-index canvas coord)
        papers (get paper-index i [])]
    (some #(when (covered? coord %1) (:color %1))
          papers)))

(defn- visible-color-frequencies
  [{:keys [canvas-width canvas-height papers paper-index]}]
  (let [canvas (last papers)] 
    (persistent!
     (reduce 
      (fn [acc coord]
        (if-let [color (visible-color coord canvas paper-index)]
          (assoc! acc color (inc (get acc color 0)))
          acc))
      (transient {})
      (for [y (range canvas-height)
            x (range canvas-width)]
        [x y])))))

(defn- solve
  [input-file]
  (let [input (read-input-file input-file)
        input (assoc input
                     :paper-index
                     (make-paper-index (last (:papers input)) (:papers input)))
        color-map (visible-color-frequencies input)
        sorted (sort-by key color-map)]
    (doseq [line sorted]
      (println (key line) (val line)))))

;; (defn -main 
;;   ([] (-main 0))
;;   ([index] 
;;    (time (solve (input-files (Integer/parseInt index))))))

(defn -main 
  ([] (-main "0"))
  ([index] 
   (time 
    (binding [*unchecked-math* :warn-on-boxed
              *warn-on-reflection* true] 
      (solve (input-files (Integer/parseInt index)))))))
