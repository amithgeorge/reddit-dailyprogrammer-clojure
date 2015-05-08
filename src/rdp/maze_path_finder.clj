(ns rdp.maze-path-finder
  (:gen-class))



(defn- neighbors
  [[x y]]
  ;; (println "[x y] - " [x y])
  (->> [[0 1] [0 -1] [1 0] [-1 0]]
       (map (fn [[x-inc y-inc]] [(+ x x-inc) (+ y y-inc)]))
       ;; (tprintln "neighbors calc - ")
       ))


(defn- tprintln
  [message param]
  (println message param)
  param)

(defn- not-visited-neighbors
  [open-cells visited current]
  (->> current
       (neighbors)
       (filter (partial contains? open-cells))
       (remove (partial contains? visited))))

(defn find-path-dfs
  [open-cells start end]
  ;; (println "cells - " open-cells)
  ;; (println "start - " start " end - " end)
  (loop [stack (list start)
         visited #{}]
    (if (empty? stack)
      stack
      (if (= (peek stack) end)
        (reverse stack)
        (let [current (peek stack)
              visited (conj visited current)
              neighbor (->> (not-visited-neighbors 
                             open-cells visited current)
                            (first))
              stack (if (nil? neighbor) (pop stack) (conj stack neighbor))]
          (recur stack visited))))))

(defn- add-back-path
  [path-map source destinations]
  (reduce (fn [path-map key]
            (assoc path-map key source))
          path-map
          destinations))

(defn- traverse-back-paths
  [path-map start]
  (loop [path []
         source start] 
    (if-not (contains? path-map source)
      (throw (Exception. (format "% has no back path" source)))
      (if (nil? (path-map source))
        (reverse (conj path source))
        (recur (conj path source) 
               (path-map source))))))

(defn find-path-bfs
  [open-cells start end]
  (loop [queue [start]
         visited #{start}
         path-map {start nil}]
    (if (empty? queue)
      queue
      (if (= (first queue) end)
        (traverse-back-paths path-map (first queue))
        (let [current (first queue)
              neighbors (not-visited-neighbors open-cells visited current)
              path-map (add-back-path path-map current neighbors)
              visited (set (concat visited neighbors))
              queue (vec (concat queue neighbors))] 
          ;; (println "Q -- " queue)
          (recur (subvec queue 1) visited path-map)))))) 


(def ^:private maze-vec
  [[1 1 0 1 0]
   [1 0 1 1 1]
   [1 1 1 1 0]
   [1 1 0 1 1]
   [1 0 1 0 1]])

(def ^:private open-cells
  (reduce (fn [acc [y x]]
            (if (= 1 (get-in maze-vec [y x]))
              (conj acc [x y])
              acc))
          #{}
          (for [y (range (count maze-vec))
                x (range  (count (first maze-vec)))] [y x]))) 

(def ^:private start [0 0])
(def ^:private end [4 4])
 
