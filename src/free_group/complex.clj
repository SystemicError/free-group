(ns free-group.complex
  (:require [quil.core :as q]))

(defn complex
  "Returns a complex map."
  ([real] {:real real :imag 0.0})
  ([real imag] {:real real :imag imag}))

(defn complex-polar
  "Returns a complex from polar specification."
  ([r theta] (let [real (* (q/cos theta) r)
                   imag (* (q/sin theta) r)]
               {:real real :imag imag})))

(defn complex*
  "Complex multiplication."
  [w z] (complex (- (* (:real w) (:real z)) (* (:imag w) (:imag z)))
                 (+ (* (:real w) (:imag z)) (* (:imag w) (:real z)))))

(defn complex+
  "Complex addition."
  [w z] (complex (+ (:real w) (:real z))
                 (+ (:imag w) (:imag z))))

(defn complex-conj
  "Complex conjugate."
  [z] (complex (:real z) (- (:imag z))))

(defn complex-div
  "Complex division."
  [w z] (let [wZ (complex* w (complex-conj z))
              zZ (:real (complex* z (complex-conj z)))]
          (complex (/ (:real wZ) zZ)
                   (/ (:imag wZ) zZ))))

(defn complex-abs
  "Complex magnitude."
  [z] (Math/pow (:real (complex* (complex-conj z) z)) 0.5))
