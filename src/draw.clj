(ns draw
  "Here's the rendering support code."
  (:require
    [see.core :as see])
  (:import
    (java.awt Color RenderingHints Graphics2D)
    (java.awt.image BufferedImage)))


(defn rgb
  ([^Integer r ^Integer g ^Integer b] (rgb r g b 255))
  ([^Integer r ^Integer g ^Integer b ^Integer a] (Color. r g b a)))

(defn hsb
  ([^double h ^double s ^double b] (Color/getHSBColor h s b)))

(defn palette-of [n]
  (map #(hsb % 1.0 1.0) (range 0 1 (/ 1 n))))

(def high-contrast-palette
  (take-nth 2 (cycle (palette-of 7))))

(defprotocol Drawable
  (clear [this rgb])
  (pixel [this x y rgb])
  (line [this x1 y1 x2 y2 rgb]))

(deftype Canvas [graphics width height colour-fn update-fn]
  Drawable
  (clear [_this rgb]
    (colour-fn rgb)
    (.fillRect graphics 0 0 width height)
    (update-fn))
  (pixel [_this x y rgb]
    (colour-fn rgb)
    (.drawLine graphics x y x y)
    (update-fn))
  (line [_this x1 y1 x2 y2 rgb]
    (colour-fn rgb)
    (.drawLine graphics x1 y1 x2 y2)
    (update-fn)))

(defn new-canvas [title width height]
  (let [image (BufferedImage. width height BufferedImage/TYPE_INT_ARGB)
        graphics ^Graphics2D (.getGraphics image)
        colour-fn (fn [rgb] (when (not= rgb (.getColor graphics)) (.setColor graphics rgb)))
        update-fn (see/see image
                           :title title)]
    (doto graphics
      (.setRenderingHint RenderingHints/KEY_RENDERING RenderingHints/VALUE_RENDER_QUALITY)
      (.setRenderingHint RenderingHints/KEY_ANTIALIASING RenderingHints/VALUE_ANTIALIAS_ON))

    (->Canvas graphics width height colour-fn update-fn)))


(comment
  (def thing (draw/new-thing))


  (draw/pixel x y rgb))