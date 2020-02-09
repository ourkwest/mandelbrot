(ns new-namespace
  "Here's what I live-coded at re:Clojure."
  (:require [draw :as draw]))


(defn add [[ra ia] [rb ib]]
  [(+ ra rb) (+ ia ib)])

(defn multiply [[ra ia] [rb ib]]
  [(- (* ra rb)
      (* ia ib))
   (+ (* ra ib)
      (* ia rb))])

(defn smallish? [[r i]]
  (> 2 (Math/sqrt (+ (* r r) (* i i)))))

(defn iterations [c]
  (->> [0 0]
       (iterate (fn [z]
                  (add (multiply z z) c)))
       (take-while smallish?)
       (take 100)))

(defn render-mandelbrot-set [size]
  (let [canvas (draw/new-canvas "" size size)]
    (doseq [x (range size)
            y (range size)]
      (let [real (-> x (/ size) (- 0.5) (* 4))
            imaginary (-> y (/ size) (- 0.5) (* 4))
            c [real imaginary]
            colour (nth draw/high-contrast-palette
                        (count (iterations c)))]
        (draw/pixel canvas x y colour)))))

(render-mandelbrot-set 500)