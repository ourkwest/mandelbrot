(ns slides
  "Here are the slides. Loading the namespace should create a Java Swing Window and
  then pressing <left> / <right> should navigate the slides."
  (:require
    [draw :as draw]
    [see.core :as see]
    [clojure.java.io :as io])
  (:import
    [java.awt.image BufferedImage]
    [java.awt Graphics2D BasicStroke Font RenderingHints Polygon]
    [java.awt.font FontRenderContext]
    [java.awt.geom AffineTransform]
    [javax.imageio ImageIO]))



(def test-scale-factor 2.0)

(def width (* 600 test-scale-factor))
(def height (* 480 test-scale-factor))

(def g-scale (/ width 100))

(def w2 (/ width 2))
(def h2 (/ height 2))

(def unit (/ width 11))

(def x-origin (volatile! 0))
(def y-origin (volatile! 0))
(def s-scale (volatile! 1))

(defn set-origin! [x y]
  (vreset! x-origin x)
  (vreset! y-origin y))

(defn reset-origin! []
  (set-origin! 0 0))

(defn set-scale! [s]
  (vreset! s-scale s))

(defn reset-scale! []
  (set-scale! 1))

(defn project [x y]
  [(+ w2 (double (* (+ x @x-origin) (* @s-scale unit))))
   (+ h2 (double (* (+ y @y-origin) (* @s-scale unit))))])

(def black (draw/rgb 0 0 0))
(def white (draw/rgb 255 255 255))
(def cyan (draw/rgb 0 255 255))
(def magenta (draw/rgb 255 0 255))
(def yellow (draw/rgb 255 255 0))
(def red (draw/rgb 255 0 0))
(def green (draw/rgb 0 255 0))
(def blue (draw/rgb 0 0 255))

(def TAU (* 2 Math/PI))

(declare prev-slide next-slide)

(defn prev-slide []
  (println 'prev-slide))

(defn handle-key [kc]
  (case kc
    39 (next-slide)
    37 (prev-slide)
    (println kc)))

(defonce image (BufferedImage. width height BufferedImage/TYPE_INT_ARGB))
(defonce update-fn (see/see image
                            :only-draw-when-updated? true
                            :key-handler-fn (fn [kc] (handle-key kc))))

(defn polygon
  ([xys]
   (let [xs (map first xys)
         ys (map second xys)]
     (Polygon. (int-array xs) (int-array ys) (count xs))))
  ([x y r n angle & [ar]]
   (let [angles (->> (range 0 TAU (/ TAU n))
                     (map (partial + angle)))
         aspect-ratio (or ar 1)
         xs (map #(-> % Math/sin (* r) (+ x)) angles)
         ys (map #(-> % Math/cos (* r aspect-ratio) (+ y)) angles)]
     (Polygon. (int-array xs) (int-array ys) (count xs)))))

(defn ^Graphics2D graphics []
  (let [^Graphics2D g (.getGraphics image)]
    (.setRenderingHint g RenderingHints/KEY_ANTIALIASING RenderingHints/VALUE_ANTIALIAS_ON)
    (.setRenderingHint g RenderingHints/KEY_RENDERING RenderingHints/VALUE_RENDER_QUALITY)
    g))

(defn clear-slide []
  (let [g (graphics)]
    (.setColor g black)
    (.fillRect g 0 0 width height)))

(defn draw-title [colour & lines]
  (let [g (graphics)
        font-size (/ height (inc (count lines)))
        spare-space (- height (* font-size (count lines)))
        spacer (/ spare-space (inc (count lines)))
        font (Font. nil Font/BOLD font-size)]
    (.setColor g colour)
    (.setFont g font)
    (doseq [n (range (count lines))
            :let [line (nth lines n)
                  bounds (.getStringBounds font line (FontRenderContext. (AffineTransform.) (boolean true) (boolean true)))
                  x (- w2 (/ (.getWidth bounds) 2))
                  y (- (* (inc n) (+ font-size spacer)) spacer)]]
      (.drawString g (str line) (int x) (int y)))))

(defn draw-text [text colour font-size x y left-right-middle]
  (let [g (graphics)
        font (Font. nil Font/PLAIN font-size)
        bounds (.getStringBounds font text (FontRenderContext. (AffineTransform.) (boolean true) (boolean true)))
        [gx gy] (project x y)
        gx (case left-right-middle
             :left gx
             :right (- gx (.getWidth bounds))
             :middle (- gx (/ (.getWidth bounds) 2)))]
    (.setColor g colour)
    (.setFont g font)
    (.drawString g (str text) (int gx) (int gy))))

(defn smear-text [text colour font-size x y left-right-middle radius]
  (doseq [angle (range 0 TAU (/ TAU 20))
          :let [smear-x (+ x (* radius (Math/sin angle)))
                smear-y (+ y (* radius (Math/cos angle)))]]
    (draw-text text colour font-size smear-x smear-y left-right-middle)))

(defn text [^Graphics2D g text x y colour font]
  (.setColor g colour)
  (.setFont g font)
  (.drawString g (str text) (int x) (int y)))

(defn proj-text [text colour font-size x y left-right-middle]
  (let [g (graphics)
        font (Font. nil Font/PLAIN font-size)
        bounds (.getStringBounds font text (FontRenderContext. (AffineTransform.) (boolean true) (boolean true)))
        [gx gy] (project x (+ y 0.5))
        gx (case left-right-middle
             :left gx
             :right (- gx (.getWidth bounds))
             :middle (- gx (/ (.getWidth bounds) 2)))]
    (.setColor g colour)
    (.setFont g font)
    (.drawString g (str text) (int gx) (int gy))))

(defn proj-texts [font-size x y left-right-middle & text-colours]
  (let [g (graphics)
        font (Font. nil Font/PLAIN font-size)
        bits (map (fn [[text colour]]
                    {:text text
                     :colour colour
                     :width (.getWidth (.getStringBounds font text (FontRenderContext. (AffineTransform.) (boolean true) (boolean true))))})
                  (partition 2 text-colours))
        total-width (transduce (map :width) + bits)
        [gx gy] (project x (+ y 0.5))
        gx (case left-right-middle
             :left gx
             :right (- gx total-width)
             :middle (- gx (/ total-width 2)))]
    (loop [gx gx
           [bit & more] bits]
      (text g (:text bit) gx gy (:colour bit) font)
      (when more (recur (+ gx (:width bit)) more)))))

(defn unproj-text [text colour font-size x y left-right-middle]
  (let [g (graphics)
        font (Font. nil Font/PLAIN font-size)
        bounds (.getStringBounds font text (FontRenderContext. (AffineTransform.) (boolean true) (boolean true)))
        [gx gy] [x y]
        gx (case left-right-middle
             :left gx
             :right (- gx (.getWidth bounds))
             :middle (- gx (/ (.getWidth bounds) 2)))]
    (.setColor g colour)
    (.setFont g font)
    (.drawString g (str text) (int gx) (int gy))))

(defn arrow-head [g gx gy size angle]
  (.fill g (polygon gx gy size 3 angle)))

(defn draw-positive-horizontal-axis []
  (let [g (graphics)]
    (.setColor g magenta)
    (.setStroke g (BasicStroke. (/ g-scale 3)))
    (.drawLine g w2 h2 (- width g-scale) h2)
    (.setColor g magenta)
    (arrow-head g (- width (* g-scale 2)) h2 (* g-scale 2) (- (/ TAU 12)))))

(defn draw-horizontal-axis []
  (let [g (graphics)
        [_ gy] (project 0 0)]
    (.setColor g magenta)
    (.setStroke g (BasicStroke. (/ g-scale 3)))
    (.drawLine g g-scale gy (- width g-scale) gy)
    (arrow-head g (- width (* g-scale 2)) gy (* g-scale 2) (- (/ TAU 12)))
    (arrow-head g (* g-scale 2) gy (* g-scale 2) (/ TAU 12))))

(defn draw-vertical-axis []
  (let [g (graphics)
        [gx _] (project 0 0)]
    (.setColor g magenta)
    (.setStroke g (BasicStroke. (/ g-scale 3)))
    (.drawLine g gx g-scale gx (- height g-scale))
    (arrow-head g gx (- height (* g-scale 2)) (* g-scale 2) 0)
    (arrow-head g gx (* g-scale 2) (* g-scale 2) (/ TAU 6))))

(defn draw-axes []
  (draw-horizontal-axis)
  (draw-vertical-axis))

(defn draw-grid []
  (let [g (graphics)]
    ;(draw-axes)
    (.setColor g cyan)
    (.setStroke g (BasicStroke. (/ g-scale 2)))
    (doseq [x (range -10 10)
            y (range -14 6)
            :let [[gx gy] (project x y)]]
      (.drawLine g gx gy gx gy))))

(defn draw-tally [n x y]
  (let [g (graphics)
        [x1 y1] (project x y)]
    (.setColor g yellow)
    (.setStroke g (BasicStroke. (/ width 120) BasicStroke/CAP_ROUND BasicStroke/JOIN_ROUND))
    (doseq [i (range (min 4 n))]
      (.drawLine g
                 (+ x1 (* i (/ width 40))) y1
                 (+ x1 (* i (/ width 40))) (+ y1 (/ height 10))))
    (when (>= n 5)
      (.drawLine g
                 (- x1 (* 1 (/ width 40))) (+ y1 (/ height 12.5))
                 (+ x1 (* 4 (/ width 40))) (+ y1 (/ height 50))))))


(defn arrow [colour xa ya xb yb]
  (let [g (graphics)
        [gxa gya] (project xa ya)
        [gxb gyb] (project xb yb)
        angle (Math/atan2 (- gxa gxb) (- gya gyb))
        ang-a (+ angle 0.5)
        ang-b (+ angle -0.5)]
    (.setColor g colour)
    (.setStroke g (BasicStroke. (/ g-scale 5)))
    (.drawLine g gxa gya gxb gyb)

    (.fill g (polygon [[gxb gyb]
                       [(+ gxb (* 2 g-scale (Math/sin ang-a)))
                        (+ gyb (* 2 g-scale (Math/cos ang-a)))]
                       [(+ gxb (* 2 g-scale (Math/sin ang-b)))
                        (+ gyb (* 2 g-scale (Math/cos ang-b)))]]))))

(defn arc [colour xa ya xb yb stroke? fill?]
  (let [g (graphics)
        fudge 0.0098
        theta-a (+ fudge (Math/atan2 (- ya) xa))
        theta-b (+ fudge (Math/atan2 (- yb) xb))
        radius (Math/sqrt (+ (* xa xa) (* ya ya)))
        [gx1 gy1] (project (- radius) (- radius))
        [gx2 gy2] (project radius radius)
        [gw gh] [(- gx2 gx1) (- gy2 gy1)]
        gta (* 360.0 (/ theta-a TAU))
        gtb (* 360.0 (/ theta-b TAU))]
    (.setColor g colour)
    (when stroke?
      (.setStroke g (BasicStroke. (/ g-scale 5)))
      (.drawArc g gx1 gy1 gw gh gta (- gtb gta)))
    (when fill?
      (.fillArc g gx1 gy1 gw gh gta (- gtb gta)))))

(defn arc-line [colour xa ya xb yb]
  (arc colour xa ya xb yb :stroke nil))

(defn arc-fill [colour xa ya xb yb]
  (arc colour xa ya xb yb nil :fill))


(def pendulum-state (volatile! nil))
;(def running-pendulum (volatile! 0))

(def pendulum-fn (volatile! +))
(defonce rendering-thread
  (future
    (while true
      (try
        (@pendulum-fn)
        (Thread/sleep 60)
        (catch Exception e
          (.printStackTrace e)
          (vreset! pendulum-fn +))))))

(defmacro render-loop [& forms]
  `(vreset! pendulum-fn (fn [] ~@forms)))

(defn init-pendulum [energy friction]
  (vswap! pendulum-state
          (fn [state]
            (let [new-angle (or (:angle state) energy)]
              {:angle    (- energy)
               :speed    0
               :friction friction
               :dt       0.1}))))

(defn step-pendulum []
  (vswap! pendulum-state
          (fn [{:keys [angle speed friction dt] :as state}]
            (let [new-angle (+ angle (* dt speed))
                  new-speed (-> speed
                                (- (* dt (Math/sin new-angle)))
                                (* (- 1 friction)))]
              (assoc state :angle new-angle
                           :speed new-speed)))))

(declare current-slide)

(defn render-pendulum [colour]
  (let [g (graphics)
        xa (/ width 4)
        ya (/ height 4)
        xb (+ xa (* (/ width 3) (Math/sin (:angle @pendulum-state))))
        yb (+ ya (* (/ width 3) (Math/cos (:angle @pendulum-state))))]
    (.setColor g colour)
    (.setStroke g (BasicStroke. (* 0.2 g-scale)))
    (.drawLine g xa ya xb yb)
    (.fill g (polygon xb yb g-scale 4 (:angle @pendulum-state)))))

(def phase-space-trail (volatile! []))

(defn render-pendulum-phase-space [colour]
  (let [g (graphics)
        xa (* width 3/4)
        ya (* height 1/2)
        scale (* g-scale 50)
        gx (+ xa (* scale (:angle @pendulum-state)))
        gy (+ ya (* scale (:speed @pendulum-state)))]
    (vswap! phase-space-trail (fn [trail] (conj (take 100 trail) [gx gy])))
    (.setStroke g (BasicStroke. (* 0.1 g-scale)))
    (doseq [[i [[x1 y1] [x2 y2]]] (reverse (map-indexed vector (partition 2 1 @phase-space-trail)))
            :let [light (- 255 (* 255 (/ i 100)))]]
      (.setColor g (draw/rgb light 0 light))
      (.drawLine g x1 y1 x2 y2))
    (.setColor g colour)
    (.setStroke g (BasicStroke. (* 0.2 g-scale)))
    (.drawLine g (- xa (* g-scale 20)) ya (+ xa (* g-scale 20)) ya)
    (.drawLine g xa (- ya (* g-scale 20)) xa (+ ya (* g-scale 20)))
    (arrow-head g (+ xa (* g-scale 20)) ya (* g-scale 1) (- (/ TAU 12)))
    (arrow-head g xa (- ya (* g-scale 20)) (* g-scale 1) (/ TAU 2))
    (unproj-text "speed" yellow (* g-scale 2) xa (- ya (* g-scale 22)) :middle)
    (unproj-text "angle" yellow (* g-scale 2) (+ xa (* g-scale 21)) (+ ya (* g-scale 3)) :middle)
    (.fill g (polygon gx gy g-scale 4 0))))

(defn draw-image [file-name]
  (let [image (ImageIO/read (io/file (str "./resources/" file-name)))
        g (graphics)
        border (* g-scale 2)
        avail-width (- width border border)
        avail-height (- height border border)
        img-scale (min (/ avail-width (.getWidth image))
                       (/ avail-height (.getHeight image)))
        dest-width (* img-scale (.getWidth image))
        dest-height (* img-scale (.getHeight image))
        sx1 0
        sy1 0
        sx2 (.getWidth image)
        sy2 (.getHeight image)
        dx1 (- w2 (/ dest-width 2))
        dy1 (- h2 (/ dest-height 2))
        dx2 (+ w2 (/ dest-width 2))
        dy2 (+ h2 (/ dest-height 2))]
    (.drawImage g image dx1 dy1 dx2 dy2 sx1 sy1 sx2 sy2 nil)))

(defn mandelbrot-iterate [r i n]
  (->> [0 0 n]
       (iterate (fn [[this-r this-i this-n]]
                  [(+ (- (* this-r this-r) (* this-i this-i)) r)
                   (+ (+ (* this-r this-i) (* this-i this-r)) i)
                   (dec this-n)]))
       (take-while #(-> % last pos?))))

(defn mandelbrot-step [n r i]
  (draw-axes)
  (draw-grid)
  (proj-texts (* 4 g-scale) -5 -4 :left (str "c = " r " + " i "i") green)
  (arrow green r (- i) r (- i))
  (if (> n 1)
    (let [points (mandelbrot-iterate r i n)
          steps (partition 2 1 points)
          [[prev-r prev-i] [current-r current-i]] (last steps)]
      (doseq [[[ra ia] [rb ib]] steps]
        (arrow cyan ra (- ia) rb (- ib)))
      (arrow yellow prev-r (- prev-i) current-r (- current-i))
      (proj-text (str current-r " + " current-i "i") yellow (* 4 g-scale) current-r (- current-i) :left))
    (arrow yellow 0 0 0 0)))

(declare render-slide)

(def colour-cycle (volatile! 0.0))

(def slides
  (remove
    nil?
    [

     (fn [_] (clear-slide))

     (fn [_]
       (clear-slide)
       (unproj-text "Peter Westmacott" green (* g-scale 4) (* 5 g-scale) (* 20 g-scale) :left)
       ;(unproj-text "Twitter: @PMWestmacott, but don't expect any tweets" white (* g-scale 3) (* 5 g-scale) (* 15 g-scale) :left)
       ;(unproj-text "Facebook: no, thank you." white (* g-scale 3) (* 5 g-scale) (* 20 g-scale) :left)
       ;(unproj-text "LinkedIn: um, just no." white (* g-scale 3) (* 5 g-scale) (* 25 g-scale) :left)

       (unproj-text "Strategic Blue" (draw/rgb 150 150 255) (* g-scale 4) (* 5 g-scale) (* 35 g-scale) :left)
       (unproj-text "We can save you money on your Amazon EC2 instances." white (* g-scale 3) (* 5 g-scale) (* 40 g-scale) :left)
       )

     (fn [_]
       (clear-slide)
       (draw-title magenta "" "The" "Mandelbrot" "Set" ""))

     (fn [_]
       (clear-slide)
       (draw-title yellow "" "Part 1:" "Complex" "Numbers" ""))

     (fn [_]
       (clear-slide)
       (draw-tally 1 -4 -3)
       (draw-tally 2 2 -2)
       (draw-tally 3 -2 -1)
       (draw-tally 4 -3 1)
       (draw-tally 5 3 2))

     (fn [_]
       (clear-slide)
       (draw-text "4" yellow (/ height 10) 1 -1 :right)
       (draw-text "42" yellow (/ height 10) 1 0 :right)
       (draw-text "418" yellow (/ height 10) 1 1 :right))

     (fn [_]
       (clear-slide)
       (draw-positive-horizontal-axis)
       (draw-text "1" yellow (/ height 20) 1 0.5 :left)
       (draw-text "2" yellow (/ height 20) 2 0.5 :left)
       (draw-text "3" yellow (/ height 20) 3 0.5 :left)
       (draw-text "..." yellow (/ height 20) 4 0.5 :left))

     (fn [_]
       (clear-slide)
       (draw-horizontal-axis)
       (draw-text "1" cyan (/ height 20) 1 0.5 :left)
       (draw-text "2" cyan (/ height 20) 2 0.5 :left)
       (draw-text "3" cyan (/ height 20) 3 0.5 :left)
       (draw-text "..." cyan (/ height 20) 4 0.5 :left)
       (draw-text "-1" yellow (/ height 20) -1.0 0.5 :left)
       (draw-text "-2" yellow (/ height 20) -2.0 0.5 :left)
       (draw-text "-3" yellow (/ height 20) -3.0 0.5 :left)
       (draw-text "..." yellow (/ height 20) -4 0.5 :left))

     (fn [_]
       (clear-slide)
       (draw-horizontal-axis)
       (draw-text "0" yellow (/ height 20) 0 0.5 :left)
       (draw-text "1" cyan (/ height 20) 1 0.5 :left)
       (draw-text "2" cyan (/ height 20) 2 0.5 :left)
       (draw-text "3" cyan (/ height 20) 3 0.5 :left)
       (draw-text "..." cyan (/ height 20) 4 0.5 :left)
       (draw-text "-1" cyan (/ height 20) -1.0 0.5 :left)
       (draw-text "-2" cyan (/ height 20) -2.0 0.5 :left)
       (draw-text "-3" cyan (/ height 20) -3.0 0.5 :left)
       (draw-text "..." cyan (/ height 20) -4 0.5 :left))

     (fn [_]
       (clear-slide)
       (draw-horizontal-axis)
       (draw-text "i" yellow (/ height 20) 0.1 -0.5 :left)
       (draw-text "0" cyan (/ height 20) 0 0.5 :left)
       (draw-text "1" cyan (/ height 20) 1 0.5 :left)
       (draw-text "2" cyan (/ height 20) 2 0.5 :left)
       (draw-text "3" cyan (/ height 20) 3 0.5 :left)
       (draw-text "..." cyan (/ height 20) 4 0.5 :left)
       (draw-text "-1" cyan (/ height 20) -1.0 0.5 :left)
       (draw-text "-2" cyan (/ height 20) -2.0 0.5 :left)
       (draw-text "-3" cyan (/ height 20) -3.0 0.5 :left)
       (draw-text "..." cyan (/ height 20) -4 0.5 :left))

     (fn [_]
       (clear-slide)
       (draw-horizontal-axis)
       (draw-axes)
       (draw-text "i" yellow (/ height 20) 0.1 -0.5 :left)
       (draw-text "2i" yellow (/ height 20) 0.1 -1.5 :left)
       (draw-text "3i" yellow (/ height 20) 0.1 -2.5 :left)
       (draw-text "0" cyan (/ height 20) 0 0.5 :left)
       (draw-text "1" cyan (/ height 20) 1 0.5 :left)
       (draw-text "2" cyan (/ height 20) 2 0.5 :left)
       (draw-text "3" cyan (/ height 20) 3 0.5 :left)
       (draw-text "-1" cyan (/ height 20) -1.0 0.5 :left)
       (draw-text "-2" cyan (/ height 20) -2.0 0.5 :left)
       (draw-text "-3" cyan (/ height 20) -3.0 0.5 :left)
       )

     (fn [_]
       (clear-slide)
       (draw-axes)
       (draw-grid)
       (draw-text "i" yellow (/ height 20) 0.1 -0.5 :left)
       (draw-text "2i" yellow (/ height 20) 0.1 -1.5 :left)
       (draw-text "3i" yellow (/ height 20) 0.1 -2.5 :left)
       (draw-text "0" cyan (/ height 20) 0 0.5 :left)
       (draw-text "1" cyan (/ height 20) 1 0.5 :left)
       (draw-text "2" cyan (/ height 20) 2 0.5 :left)
       (draw-text "3" cyan (/ height 20) 3 0.5 :left)
       (draw-text "-1" cyan (/ height 20) -1.0 0.5 :left)
       (draw-text "-2" cyan (/ height 20) -2.0 0.5 :left)
       (draw-text "-3" cyan (/ height 20) -3.0 0.5 :left)
       )

     (fn [_]
       (clear-slide)
       (draw-axes)
       (draw-grid)
       (draw-text "i" yellow (/ height 20) 0.1 -0.5 :left)
       (draw-text "2i" yellow (/ height 20) 0.1 -1.5 :left)
       (draw-text "3i" yellow (/ height 20) 0.1 -2.5 :left)
       (draw-text "3 + 2i" yellow (/ height 20) 3 -1.5 :left)
       (draw-text "0" cyan (/ height 20) 0 0.5 :left)
       (draw-text "1" cyan (/ height 20) 1 0.5 :left)
       (draw-text "2" cyan (/ height 20) 2 0.5 :left)
       (draw-text "3" cyan (/ height 20) 3 0.5 :left)
       (draw-text "-1" cyan (/ height 20) -1.0 0.5 :left)
       (draw-text "-2" cyan (/ height 20) -2.0 0.5 :left)
       (draw-text "-3" cyan (/ height 20) -3.0 0.5 :left)
       (arrow yellow 0 0 3 -2)
       )

     (fn [_]
       (clear-slide)
       (set-origin! -4 3)
       (draw-vertical-axis)
       (draw-horizontal-axis)
       (draw-grid)
       (proj-text "1 + 2i" yellow (/ height 20) 1 -2 :left)
       (arrow yellow 0 0 1 -2)
       (proj-text "3 + i" cyan (/ height 20) 3 -1 :left)
       (arrow cyan 0 0 3 -1)
       (reset-origin!))

     (fn [n]
       (render-slide (dec n))
       (set-origin! -4 3)
       (arrow yellow 3 -1 4 -3)
       (arrow cyan 1 -2 4 -3)
       (proj-text "4 + 3i" green (/ height 20) 4 -3 :left)
       (reset-origin!))

     (fn [_]
       (clear-slide)
       (set-origin! 0 -0.25)
       (proj-texts (/ height 20) 0 -3 :middle
                   "(" white "1" yellow " + " white "2i" cyan ") x (" white "3" magenta " + " white "i" green ")" white)

       (reset-origin!))

     (fn [n]
       (render-slide (dec n))
       (set-origin! 0 -0.25)

       (arrow yellow  -1.5 -2.4 -3.4  -0.9)
       (arrow yellow  -1.5 -2.4  0.5  -0.9)
       (arrow cyan    -0.5 -2.4 -1.3  -0.9)
       (arrow cyan    -0.5 -2.4  3.3  -0.9)
       (arrow magenta  0.7 -2.4 -2.5  -0.9)
       (arrow magenta  0.7 -2.4  2.45 -0.9)
       (arrow green    1.55 -2.4 -0.4  -0.9)
       (arrow green    1.55 -2.4  1.4  -0.9)

       (proj-texts (/ height 20) 0 -1 :middle
                   "(" white "1" yellow " x " white "3" magenta ") + (" white
                   "2i" cyan " x " white "i" green ") + (" white
                   "1" yellow " x " white "i" green ") + (" white
                   "3" magenta " x " white "2i" cyan ")" white)

       (reset-origin!))

     (fn [n]
       (render-slide (dec n))
       (set-origin! 0 -0.25)

       (arrow white -3.2 -0.3 -1.9 1.0)
       (arrow white -1.1 -0.3 -0.7 1.0)
       (arrow white  1.0 -0.3  0.7 1.0)
       (arrow white  3.0 -0.3  1.9 1.0)
       (proj-texts (/ height 20) 0 1 :middle
                   "(" white "3" yellow " + " white "-2" yellow ") + (" white "i" yellow " + " white "6i" yellow ")" white )

       (reset-origin!))

     (fn [n]
       (render-slide (dec n))
       (set-origin! 0 -0.25)

       (arrow white -1.2 1.7 -0.6 3.0)
       (arrow white  1.1 1.7  0.6 3.0)
       (proj-texts (/ height 20) 0 3 :middle "1" yellow " + " white "7i" yellow)

       (reset-origin!))

     (fn [_]
       (set-scale! 0.9)
       (set-origin! 0 4)
       (draw-axes)
       (draw-grid)

       (arrow yellow 0 0 1 -2) (proj-text "1 + 2i" yellow (/ height 20) 1 -2 :left)
       (arrow cyan 0 0 3 -1) (proj-text "3 + i" cyan (/ height 20) 3 -1 :left)
       (arrow green 0 0 1 -7) (proj-text "1 + 7i" green (/ height 20) 1 -7 :left)

       (reset-origin!)
       (reset-scale!)
       )

     (fn [n]
       (render-slide (dec n))

       (set-scale! 0.9)
       (set-origin! 0 4)

       (arc-line white 1 -2 0 -5)
       (arc-line white 3 -1 0 -5)
       (arc-line white 1 -7 0 -5)

       (arrow green -2 -7.078 0 -7.078) (proj-text "7.078..." white (/ height 20) -2 -7.65 :right)
       (arrow cyan -2 -3.162 0 -3.162) (proj-text "3.162..." white (/ height 20) -2 -3.68 :right)
       (arrow yellow -2 -2.236 0 -2.236) (proj-text "2.236..." white (/ height 20) -2 -2.78 :right)

       (reset-origin!)
       (reset-scale!))

     (fn [_]
       (set-scale! 0.9)
       (set-origin! 0 4)
       (draw-axes)
       (draw-grid)

       (arrow yellow 0 0 1 -2) (proj-text "1 + 2i" yellow (/ height 20) 1 -2 :left)
       (arrow cyan 0 0 3 -1) (proj-text "3 + i" cyan (/ height 20) 3 -1 :left)
       (arrow green 0 0 1 -7) (proj-text "1 + 7i" green (/ height 20) 1 -7 :left)

       (arc-fill (draw/rgb 0 255 255 200) 1.75 0 3 -1)
       (arc-line (draw/rgb 0 255 255 200) 1.75 0 3 -1)
       (arc-fill (draw/rgb 255 255 0 200) 1 0 1 -2)
       (arc-line (draw/rgb 255 255 0 200) 1 0 1 -2)

       (reset-origin!)
       (reset-scale!))

     (fn [n]
       (render-slide (dec n))
       (set-scale! 0.9)
       (set-origin! 0 4)

       (arc-fill (draw/rgb 0 255 255 200) 0.7826 -1.5652 1 -7)
       (arc-line (draw/rgb 0 255 255 200) 0.7826 -1.5652 1 -7)

       (reset-origin!)
       (reset-scale!))

     #_(fn [n]
       (proj-text "Part 1: Recap" yellow (* g-scale 8)  0 -3.5 :middle)

       (proj-text "- Complex numbers are 2-dimensional" yellow (* g-scale 4)  -5 -2 :left)
       (proj-text "- They have a real part and an imaginary part." yellow (* g-scale 4)  -5 -1 :left)
       (proj-text "- We can add them and multiply them" yellow (* g-scale 4)  -5 0 :left)
       (proj-text "- These correspond to transformations" yellow (* g-scale 4)  -5 1 :left)
       (proj-text "  in 2-dimensional space" yellow (* g-scale 4)  -5 1.5 :left)

       )

     (fn [_]
       (clear-slide)
       (draw-title yellow "" "Part 2:" "Strange" "Attractors" ""))

     (fn [_]
       (init-pendulum 0.35 0.0)
       (render-loop
         (step-pendulum)
         (clear-slide)
         (render-pendulum yellow)
         (update-fn)))

     (fn [_]
       (vreset! phase-space-trail [])
       (init-pendulum 0.35 0.0)
       (render-loop
         (step-pendulum)
         (clear-slide)
         (unproj-text "\"Phase Space\"" yellow (* g-scale 6) (* width 3/4) (* height 1/8) :middle)
         (render-pendulum cyan)
         (render-pendulum-phase-space yellow)
         (update-fn)))

     (fn [_]
       (vreset! phase-space-trail [])
       (init-pendulum 0.35 0.02)
       (render-loop
         (step-pendulum)
         (clear-slide)
         (render-pendulum cyan)
         (render-pendulum-phase-space yellow)
         (update-fn)))

     (fn [_]
       (clear-slide)
       (draw-image "Lorenz_Attractor.png") ;https://commons.wikimedia.org/wiki/File:Lorenz_system_r28_s10_b2-6666.png (Public Domain)
       )

     (fn [_]
       (clear-slide)
       (draw-image "DeJong_Attractor.png") ; Rendered by me.
       )

     (fn [_]
       (clear-slide)
       (draw-image "Hen_On_A_Tractor.png") ; Composited by me based on public domain images, no attribution required.
       )

     (fn [_]
       (clear-slide)
       (draw-image "Henon_Attractor_2.png") ; Rendered by me.
       )


     (fn [_]
       (clear-slide)
       (draw-title yellow "" "Part 3:" "The" "Mandelbrot" "Set" ""))

     (fn [_]
       (clear-slide)
       (draw-image "Mandelbrot_Set.jpg"))

     (fn [_]
       (clear-slide)
       (draw-image "Mandelbrot_Set.jpg")

       (set-origin! 0.7 0)
       (set-scale! 3.2)

       (draw-axes)

       ;(smear-text "0" black (/ height 20) 0.025 0.14 :left 0.015)

       (set-origin! 0.725 0.14)
       (smear-text "0" black (/ height 20) 0 0 :left 0.01)
       (draw-text "0" cyan (/ height 20) 0 0 :left)
       (draw-text "i" cyan (/ height 20) 0 -1 :left)
       (draw-text "-i" cyan (/ height 20) 0 1 :left)
       (smear-text "-1" black (/ height 20) -1 0 :left 0.01)
       (draw-text "-1" cyan (/ height 20) -1 0 :left)
       (draw-text "-2" cyan (/ height 20) -2 0 :left)

       (unproj-text "z   = z ² + c" yellow (* g-scale 6) (* width 1/4) (* height 1/8) :middle)
       (unproj-text "n" yellow (* g-scale 3) (+ (* width 1/4) (* g-scale 2.2)) (+ (* height 1/8) (* g-scale 1)) :middle)
       (unproj-text "n+1" yellow (* g-scale 3) (- (* width 1/4) (* g-scale 11.5)) (+ (* height 1/8) (* g-scale 1)) :middle)

       (reset-origin!)
       (reset-scale!))

     (fn [_]
       (draw-axes)
       (proj-texts (* 4 g-scale) -5 -4 :left "z = 0, " yellow "c = -1" green)
       (arrow green -1 0 -1 0)
       (proj-text "0" yellow (* 2 g-scale) -4.75 -3.9 :left)
       (arrow yellow 0 0 0 0)
       (proj-text "0 + 0i" yellow (* 4 g-scale) 0 0 :left))

     (fn [n]
       (draw-axes)
       (proj-texts (* 4 g-scale) -5 -4 :left "z = 0, " cyan "c = -1" green)
       (arrow green -1 0 -1 0)
       (proj-text "0" cyan (* 2 g-scale) -4.75 -3.9 :left)
       (proj-texts (* 4 g-scale) -5 -3 :left "z = 0² " yellow "-1" green)
       (proj-text "1" yellow (* 2 g-scale) -4.75 -2.9 :left)
       (proj-text "z = -1" yellow (* 4 g-scale) -5 -2.5 :left)
       (proj-text "1" yellow (* 2 g-scale) -4.75 -2.4 :left)
       (arrow yellow 0 0 -1 0)
       (proj-text "-1 + 0i" yellow (* 4 g-scale) -1 0 :left))

     (fn [n]
       (draw-axes)
       (proj-texts (* 4 g-scale) -5 -4 :left "z = 0, " cyan "c = -1" green)
       (arrow green -1 0 -1 0)
       (proj-text "0" cyan (* 2 g-scale) -4.75 -3.9 :left)
       (proj-text "z = 0² -1" cyan (* 4 g-scale) -5 -3 :left)
       (proj-text "1" cyan (* 2 g-scale) -4.75 -2.9 :left)
       (proj-text "z = -1" cyan (* 4 g-scale) -5 -2.5 :left)
       (proj-text "1" cyan (* 2 g-scale) -4.75 -2.4 :left)
       (proj-texts (* 4 g-scale) -5 -2 :left "z = -1²" yellow " -1" green)
       (proj-text "2" yellow (* 2 g-scale) -4.75 -1.9 :left)
       (proj-text "z = 0" yellow (* 4 g-scale) -5 -1.5 :left)
       (proj-text "2" yellow (* 2 g-scale) -4.75 -1.4 :left)
       (arrow yellow -1 0 0 0)
       (proj-text "0 + 0i" yellow (* 4 g-scale) 0 0 :left))

     ;(fn [n]
     ;  (draw-axes)
     ;  (proj-texts (* 4 g-scale) -5 -4 :left "z = 0, " cyan "c = 0.5 + 0.5i" green)
     ;  (arrow green 0.5 -0.5 0.5 -0.5)
     ;
     ;  (let [points (mandelbrot-iterate 0.5 0.5 7)
     ;        steps (partition 2 1 points)
     ;        [[prev-r prev-i] [current-r current-i]] (last steps)]
     ;    (doseq [[[ra ia] [rb ib]] steps]
     ;      (arrow cyan ra (- ia) rb (- ib)))
     ;    (arrow yellow prev-r (- prev-i) current-r (- current-i))
     ;    (proj-text (str current-r " + " current-i "i") yellow (* 4 g-scale) current-r (- current-i) :left)))

     (fn [n] (mandelbrot-step 1 0.5 0.5))
     (fn [n] (mandelbrot-step 2 0.5 0.5))
     (fn [n] (mandelbrot-step 3 0.5 0.5))
     (fn [n] (mandelbrot-step 4 0.5 0.5))
     (fn [n] (mandelbrot-step 5 0.5 0.5))
     (fn [n] (mandelbrot-step 6 0.5 0.5))
     (fn [n] (mandelbrot-step 7 0.5 0.5))

     (fn [n] (mandelbrot-step 1 -0.6 0.6))
     (fn [n] (mandelbrot-step 2 -0.6 0.6))
     (fn [n] (mandelbrot-step 3 -0.6 0.6))
     (fn [n] (mandelbrot-step 4 -0.6 0.6))
     (fn [n] (mandelbrot-step 5 -0.6 0.6))
     (fn [n] (mandelbrot-step 6 -0.6 0.6))
     (fn [n] (mandelbrot-step 7 -0.6 0.6))
     (fn [n] (mandelbrot-step 8 -0.6 0.6))
     (fn [n] (mandelbrot-step 9 -0.6 0.6))
     (fn [n] (mandelbrot-step 10 -0.6 0.6))
     (fn [n] (mandelbrot-step 11 -0.6 0.6))
     (fn [n] (mandelbrot-step 12 -0.6 0.6))
     (fn [n] (mandelbrot-step 13 -0.6 0.6))
     (fn [n] (mandelbrot-step 14 -0.6 0.6))

     (fn [_]

       #_(let [image (ImageIO/read (io/file (str "./resources/SharkBruce.png")))])

       (vreset! colour-cycle 0.0)
       (render-loop

         (clear-slide)

         (vswap! colour-cycle + 0.03)

         #_(let [g (graphics)
                 factor (- (Math/cos @colour-cycle))
                 face-x (/ width 3.2)
                 face-y (/ height 10)]
             (.setTransform g (doto (AffineTransform.)
                                (.translate face-x face-y)
                                (.rotate (* 9 factor))
                                (.scale (* 3 (+ 1.1 factor))
                                        (* 3 (+ 1.1 factor)))
                                (.translate (- face-x) (- face-y))))
             (.drawImage g image
                         0 0 width height
                         0 0 (.getWidth image) (.getHeight image)
                         nil))

         (draw-title (draw/hsb (+ @colour-cycle (* TAU 0.1)) 1.0 1.0) "" "" "Live!           " "" "")
         (draw-title (draw/hsb (+ @colour-cycle (* TAU 0.2)) 1.0 1.0) "" "" "" "Coding!!" "")
         (draw-title (draw/hsb (+ @colour-cycle (* TAU 0.3)) 1.0 1.0) "" "" "" "" "       Time!!!")

         (update-fn))

       )

     ]))

(defonce current-slide (volatile! 0))

;(defn render-previous-slide []
;  (let [slide-fn (nth slides (mod (+ (dec @current-slide) (count slides)) (count slides)))]
;    (slide-fn)))

(defn render-slide [n]
  (let [slide-fn (nth slides n)]
    (render-loop nil)
    (slide-fn n)
    (update-fn)))

(defn render-current-slide []
  (clear-slide)
  (render-slide @current-slide)
  (text (graphics) (str @current-slide " / " (count slides)) 5 (- height 7) white (Font. nil Font/PLAIN (* 2 g-scale))))

(defn next-slide []
  (vswap! current-slide (fn [n] (mod (inc n) (count slides))))
  (render-current-slide))

(defn prev-slide []
  (vswap! current-slide (fn [n] (mod (+ (dec n) (count slides)) (count slides))))
  (render-current-slide))

(render-current-slide)

; - numbers and counting, no documented evidence for the start of this, it predates human history and probably humans
; - place-based numbers 3400BC mesopotamia first documented evidence
;    1, 10, 100
;    number line
; - negative numbers, First documented in ‘The Nine Chapters on the Mathematical Art’ in China, 100-50 BC
; - zero, Brahmagupta, AD 628
;   square-root of minus one? can't do it. Maths kept asking for it.
;   No single person invented or discovered this, but gradually accepted a "square root of minus one"
;   Descartes called it 'imaginary' because it couldn't be real. It stuck.
;   Where does it go on this number line?
; - It doesn't! It goes just off the number line.
; - Implies a new axis that's perpendicular!
; - implies 2d numbers - "Complex" numbers!
;
; - addition
; - multiplication


; strange attractors
;   attractors
;     pendulum - cyclic
;     + friction - fixed point
;     but some systems more complicated
;     Edward Lorenz, simplified model of convection, three parameters, an equation for each
;   strange attractors
;     lorenz attractor
;     seem to cycle forever, but never actually repeat themselves
;     de jong attractor
;     henon attractor
;     starting point doesn't matter (within certain range) - will converge to strange fractal paths
;     but starting point does matter if you want to predict where it will be in the future!

;   one variant on this idea:
;     instead of taking one point and iterating it to see where it goes
;     take every point and iterate them and see where they go!
;   one particularly famous system like this is the mandelbrot set
;     equation, set of points which remain bounded in absolute value = mandelbrot set
;     an infinitely long boundary, in a finite space!
;     also has fractal self-similarity, and very famous images.

;;;;;;;;

; mandelbrot set
;   iterate a complex number
;     small ones tend towards zero (check this)
;     large ones tend towards infinity
;     and at the boundary?
;       an infinitely long boundary, in a finite space! with Fractal properties
;     how do we visualise that?
;     with code!!!
