(ns free-group.core
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [free-group.complex :refer :all]))

;;; complex and mobius functions

(defn mobius-translation
  "Returns a mobius translation from a complex z."
  [z] {:a (complex 1.0) :b z :c (complex 0.0) :d (complex 1.0)})

(defn mobius-scaling
  "Returns a mobius scaling from a real r."
  [r] {:a (complex r) :b (complex 0.0) :c (complex 0.0) :d (complex 1.0)})

(defn mobius*
  "Multiplies two mobius transforms."
  ([f g] {:a (complex+ (complex* (:a f) (:a g)) (complex* (:b f) (:c g)))
          :b (complex+ (complex* (:a f) (:b g)) (complex* (:b f) (:d g)))
          :c (complex+ (complex* (:c f) (:a g)) (complex* (:d f) (:c g)))
          :d (complex+ (complex* (:c f) (:b g)) (complex* (:d f) (:d g)))})
  ([f g & args] (let [m (mobius* f g)]
                  (if (> (count args) 1)
                    (recur m (first args) (rest args))
                    (mobius* m (first args)))))
  )

(defn mobius-involution
  "Defines an involution on circle of complex center, real radius."
  [circle] (let [c (:center circle)
                 r (:radius circle)]
             (mobius* (mobius-translation (complex-conj c))
             (mobius-scaling r)
             {:a (complex 0.0) :b (complex 1.0) :c (complex 1.0) :d (complex 0.0)}
             (mobius-scaling (/ 1.0 r))
             (mobius-translation (complex* (complex -1.0) c)))))

(defn normalize-homogenous
  "Normalizes homogenous coordinates."
  [z] (if (= (:z1 z) (complex 0.0))
        {:z0 (complex 1.0)
         :z1 (complex 0.0)}
        {:z0 (complex-div (:z0 z) (:z1 z))
         :z1 (complex 1.0)}))

(defn mobius-of-point
  "Takes the mobius transform of a complex point and returns homogenous coord."
  [m z] (let [z0 (:z0 z)
              z1 (:z1 z)
              a (:a m)
              b (:b m)
              c (:c m)
              d (:d m)]
          {:z0 (complex+ (complex* a z0) (complex* b z1))
           :z1 (complex+ (complex* c z0) (complex* d z1))}))

(defn involution-of-circle
  "Takes the circle involution of another circle."
  [circle0 circle1] (let [m (mobius-involution circle0)
                          c1 (:center circle1)
                          r1 (:radius circle1)
                          n (mobius-involution {:center (complex-conj c1) :radius r1})
                          t (mobius* m n m)
                          a (:a t)
                          b (:b t)
                          c (:c t)
                          d (:d t)]
                  (if (= c (complex 0.0))
                    {:center (complex 0.0) :radius 0.0 :color 0.0} ; incomplete, should give homogenous coords of inf
                    (let [b-div-c (complex-div b c)
                          d-div-c (complex-div d c)
                          center (complex* (complex -1.0) d-div-c)
                          center-norm-square (complex* (complex-conj center) center)]
                      {:center center
                       :radius (Math/pow (complex-abs (complex+ b-div-c center-norm-square)) 0.5)
                       :color (:color circle0)}))))

(defn lengthen-word
  "Extend one (nonempty) word by  each of available letters."
  ([num-letters word] (lengthen-word num-letters word #{}))
  ([num-letters word words] (let [letter (- num-letters 1)]
                              (if (= 0 num-letters)
                                words
                                (recur (- num-letters 1)
                                       word
                                       (if (= letter (first word))
                                         words
                                         (conj words (conj word letter))))))))

(defn lengthen-words
  "Map each word in words to (num-letters - 1) new words with a new letter conjoined to it."
  ; '('(0) '(1)) --> ('(1 0) '(0 1))
  ([num-letters words] (lengthen-words num-letters words '()))
  ([num-letters words lengthened] (let [word (first words)
                                        others (rest words)
                                        new-words (lengthen-word num-letters word)]
                                    (if (= 0 (count words))
                                      lengthened
                                      (recur num-letters
                                             others
                                             (concat lengthened new-words))))))

(defn generate-words
  "Creates a collection of words based on generators."
  ([num-letters max-depth] (generate-words num-letters max-depth (map list (range num-letters)) 1 ))
  ([num-letters max-depth words depth] (let [lengthened-words (lengthen-words num-letters words)]
                                         (if (= depth max-depth)
                                           words
                                           (recur num-letters
                                                  max-depth
                                                  (concat lengthened-words (map list (range num-letters)))
                                                  (+ depth 1))))))

(defn word-to-circle
  "Converts a single word (list of generator indices) into a circle."
  ([generators word] (word-to-circle generators (butlast word) (nth generators (last word))))
  ([generators word circle] (let [letter (last word)
                                  involver (if (< 0 (count word))
                                             (nth generators letter))]
                              (if (= 0 (count word))
                                circle
                                (recur generators
                                       (butlast word)
                                       (involution-of-circle involver circle))))))

(defn words-to-circles
  ([generators words] (words-to-circles generators words '()))
  ([generators words circles] (let [word (first words)
                                    others (rest words)]
                                (if (= 0 (count words))
                                  circles
                                  (recur generators others (conj circles
                                                                 (word-to-circle generators word)))))))

(defn generate-circles
  "Generates all circles of word length <= max-depth."
  [max-depth generators] (words-to-circles generators (generate-words (count generators) max-depth)))

;;; quil setup

(defn setup []
  ; Set frame rate to 30 frames per second.
  (q/frame-rate 30)
  ; Set color mode to (default) RGB.
  (q/color-mode :rgb)
  ; Set ellipse mode so the center of an ellipse is its origin, takes semimajor axes
  (q/ellipse-mode :radius)
  ; setup function returns initial state. It contains
  ; circle color and position.
  {:time 0})

(defn update-state [state]
  ; Update sketch state by changing circle color and position.
  {:time (+ (/ 1.0 30.0) (:time state))})

(defn draw-circles [circles]
  "Draws a collection of circles."
  ; each circle should also be tagged with a color
  (let [circle (first circles)
        others (rest circles)
        center (:center circle)
        radius (:radius circle)
        color (:color circle)
        x (:real center)
        y (:imag center)]
        ;dummy (println "\n" circles "\n" (count circles) "\n" (type circles))]
    (q/stroke color)
    (q/ellipse x y radius radius)
    (if (< 0 (count others))
      (recur others))))

(defn draw-state [state]
  ; Clear the sketch by filling it with light-grey color.
  (q/background 240)
  ; Give circles transparent fill
  (q/fill 0 0 0 0)
  ; Calculate x and y coordinates of the circle.
  (let [t (:time state)
        angle (/ t 50.0)
        distance (* (q/width) (q/cos (/ t 6.0)) 0.3)
        pi 3.14159265358979
        r (* (q/width) 0.2)
        offset (* 2.0 (/ pi 3.0))
        c0 (complex-polar distance angle)
        c1 (complex-polar distance (+ angle offset))
        c2 (complex-polar distance (+ angle (* 2.0 offset)))
        circle0 {:center c0 :radius r :color (q/color 255 0 0)}
        circle1 {:center c1 :radius r :color (q/color 0 255 0)}
        circle2 {:center c2 :radius r :color (q/color 0 0 255)}
        max-depth 5 ]
    ; Move origin point to the center of the sketch.
    (q/with-translation [(/ (q/width) 2)
                         (/ (q/height) 2)]
      ; Draw the circles.
      (draw-circles (generate-circles max-depth [circle0 circle1 circle2])))))


(q/defsketch free-group
  :title "Mobius group"
  :size [1000 1000]
  ; setup function called only once, during sketch initialization.
  :setup setup
  ; update-state is called on each iteration before draw-state.
  :update update-state
  :draw draw-state
  :features [:keep-on-top]
  ; This sketch uses functional-mode middleware.
  ; Check quil wiki for more info about middlewares and particularly
  ; fun-mode.
  :middleware [m/fun-mode])

