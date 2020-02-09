(ns mandelbrot
  "Here's where I was playing with various methods for rendering the mandlebrot set."
  (:require
    [draw :as draw])
  (:import (java.util.concurrent Executors ExecutorService)))


(defn add [[r1 i1] [r2 i2]]
  [(+ r1 r2) (+ i1 i2)])

(defn multiply [[r1 i1] [r2 i2]]
  [(- (* r1 r2) (* i1 i2)) (+ (* r1 i2) (* r2 i1))])

(defn iteration [z c]
  (add (multiply z z) c))

(defn magnitude [[r i]]
  (Math/sqrt (+ (* r r) (* i i))))

(def palette (take-nth 2 (cycle (draw/palette-of 7))))

(defn mandelbrot [size iteration-limit]
  (let [canvas (draw/new-canvas "Mandelbrot" size size)]
    (doseq [x (range size)
            y (range size)
            :let [r (-> x (/ size) (- 0.5) (* 4))
                  i (-> y (/ size) (- 0.5) (* 4))
                  c [r i]
                  iterations (iterate #(iteration % c) c)
                  iteration-count (count (take iteration-limit (take-while #(> 2 (magnitude %)) iterations)))
                  colour (if (= iteration-count iteration-limit)
                           (draw/rgb 0 0 0)
                           (nth palette iteration-count))]]
      (draw/pixel canvas x y colour))))

;==========================================




(defn pixel->colour [x y size iteration-limit]
  (let [r (-> x (/ size) (- 0.5) (* 4))
        i (-> y (/ size) (- 0.5) (* 4))
        c [r i]
        iterations (iterate #(iteration % c) c)
        iteration-count (count (take iteration-limit (take-while #(> 2 (magnitude %)) iterations)))
        colour (if (= iteration-count iteration-limit)
                 (draw/rgb 0 0 0)
                 (nth palette iteration-count))]
    colour))

(defn mandelbrot [size iteration-limit]
  (let [canvas (draw/new-canvas "Mandelbrot" size size)]
    (doseq [x (range size)
            y (range size)]
      (draw/pixel canvas x y (pixel->colour x y size iteration-limit)))))

(defn mandelbrot [size iteration-limit]
  (let [canvas (draw/new-canvas "Mandelbrot" size size)]
    (doseq [

            x (range size)
            y (range size)]
      (draw/pixel canvas x y (pixel->colour x y size iteration-limit)))))

;==========================================

(defn iter [[z c :as point]]
  (assoc point 0 (iteration z c)))

(defn mandelbrot-a-b [size iteration-limit]
  (let [canvas (draw/new-canvas "Mandelbrot" size size)
        ex ^ExecutorService (Executors/newSingleThreadExecutor)
        all-pixels (for [x (range size)
                         y (range size)
                         :let [r (-> x (/ size) (- 0.5) (* 4))
                               i (-> y (/ size) (- 0.5) (* 4))
                               z [r i]
                               c [r i]]]
                     [z c x y])]
    (draw/clear canvas (draw/rgb 0 0 0))
    (loop [counter 0
           pixels all-pixels]
      (let [split (group-by #(-> % first magnitude (> 2)) pixels)]
        (.submit ex ^Runnable #(doseq [[_ _ x y] (get split true)]
                                 (draw/pixel canvas x y (nth palette counter))))
        (when-not (= counter iteration-limit)
          (recur (inc counter) (map iter (get split false))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype Complex [^double real ^double imaginary]
  Object
  (toString [this]
    (str "[" real \, imaginary "]")))

(defn next-iteration-of [^Complex z ^Complex c]
  (Complex. (+ (- (* (.-real z) (.-real z)) (* (.-imaginary z) (.-imaginary z)))
               (.-real c))
            (+ (* 2 (.-real z) (.-imaginary z))
               (.-imaginary c))))

(defn too-big? [^Complex c]
  (< 4 (+ (* (.-real c) (.-real c))
          (* (.-imaginary c) (.-imaginary c)))))

(defn iteration-count [^Complex c ^long limit] ; winner on speed so far - but can deftypes have mutable fields?
  (loop [counter 0
         ^Complex z c]
    (if (or (too-big? z)
            (= limit counter))
      counter
      (recur (inc counter)
             (next-iteration-of z c)))))

(defn mandelbrot-2 [size iteration-limit]
  (let [canvas (draw/new-canvas "Mandelbrot" size size)]
    (doseq [x (range size)
            y (range size)
            :let [r (-> x (/ size) (- 0.5) (* 4))
                  i (-> y (/ size) (- 0.5) (* 4))
                  c (->Complex r i)
                  iteration-count (iteration-count c iteration-limit)
                  colour (if (= iteration-count iteration-limit)
                           (draw/rgb 0 0 0)
                           (nth palette iteration-count))]]
      (draw/pixel canvas x y colour))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol Mutable
  (next-iteration [z])
  (stop? [z]))

(deftype MutableComplex [^double initial-real
                         ^double initial-imaginary
                         ^double ^:unsynchronized-mutable real
                         ^double ^:unsynchronized-mutable imaginary]
  Mutable
  (next-iteration [this]
    (let [real' (+ (- (* real real) (* imaginary imaginary))
                   initial-real)
          imaginary' (+ (* 2 real imaginary)
                        initial-imaginary)]
      (set! real real')
      (set! imaginary imaginary')))
  (stop? [this]
    (< 4 (+ (* real real)
            (* imaginary imaginary))))
  Object
  (toString [this]
    (str "[" real \, imaginary "]"))
  )

(defn iteration-count-3 [^MutableComplex c ^long limit]
  (loop [counter 0]
    (if (or (stop? c)
            (= limit counter))
      counter
      (do (next-iteration c)
          (recur (inc counter))))))

(defn mandelbrot-3 [size iteration-limit]
  (let [canvas (draw/new-canvas "Mandelbrot" size size)]
    (doseq [x (range size)
            y (range size)
            :let [r (-> x (/ size) (- 0.5) (* 4))
                  i (-> y (/ size) (- 0.5) (* 4))
                  c (->MutableComplex r i r i)
                  iteration-count (iteration-count-3 c iteration-limit)
                  colour (if (= iteration-count iteration-limit)
                           (draw/rgb 0 0 0)
                           (nth palette iteration-count))]]
      (draw/pixel canvas x y colour))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;(defn complex [real imaginary]
;  (double-array [real imaginary]))
;
;
;(defn next-iteration-4 [z c]
;  (let [real (aget z 0)
;        imaginary (aget z 1)
;        real' (+ (- (* real real) (* imaginary imaginary))
;                 initial-real)
;        imaginary' (+ (* 2 real imaginary)
;                      initial-imaginary)]
;    (set! real real')
;    (set! imaginary imaginary')))
;
;(defn stop?-4 [this]
;  (< 4 (+ (* real real)
;          (* imaginary imaginary))))
;
