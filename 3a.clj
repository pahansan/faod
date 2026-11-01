(defn power-of-two-at-least [n p]
  (if (< p n)
    (recur n (* 2 p))
    p))

(def positive-prefix
  (memoize
    (fn [f dx p]
      (println "dx = " dx " p = " p)
      (if (= p 1)
        (let [f0 (f 0)
              f1 (f dx)]
          [[0.0 f0]
           [(* dx (/ (+ f0 f1) 2.0)) f1]])
        (let [half (/ p 2)
              v (positive-prefix f dx half)]
          (reduce
            (fn [acc k]
              (let [[prev-acc prev-f] (nth acc (dec k))
                    fk (f (* k dx))
                    acc-k (+ prev-acc
                             (* dx (/ (+ prev-f fk) 2.0)))]
                (conj acc [acc-k fk])))
            v
            (range (inc half) (inc p))))))))

(def negative-prefix
  (memoize
    (fn [f dx p]
      (println "dx = " dx " p = " p)
      (if (= p 1)
        (let [f0 (f 0)
              fm1 (f (- dx))]
          [[0.0 f0]
           [(- (* dx (/ (+ f0 fm1) 2.0))) fm1]])
        (let [half (/ p 2)
              v (negative-prefix f dx half)]
          (reduce
            (fn [acc k]
              (let [[prev-acc prev-f] (nth acc (dec k))
                    fk (f (- (* k dx)))
                    acc-k (- prev-acc
                             (* dx (/ (+ prev-f fk) 2.0)))]
                (conj acc [acc-k fk])))
            v
            (range (inc half) (inc p))))))))

(defn make-memoized-integral [f dx]
  (fn [x]
    (cond
      (zero? x)
      0.0

      (pos? x)
      (let [n (Math/floor (/ x dx))
            r (- x (* n dx))
            p (power-of-two-at-least n 1)
            [acc f-node] (nth (positive-prefix f dx p) n)]
        (if (zero? r)
          acc
          (+ acc (* r (/ (+ f-node (f x)) 2.0)))))

      :else
      (let [a (- x)
            n (Math/floor (/ a dx))
            r (- a (* n dx))
            p (power-of-two-at-least n 1)
            [acc f-node] (nth (negative-prefix f dx p) n)]
        (if (zero? r)
          acc
          (- acc (* r (/ (+ f-node (f x)) 2.0))))))))

(let [F (make-memoized-integral (fn [t] t) 0.01)]
  (println "F(2.0)    =" (F 2.0)   "точное значение =" (/ (* 2.0 2.0) 2.0))
  (println "F(0.537)  =" (F 0.537) "точное значение =" (/ (* 0.537 0.537) 2.0))
  (println "F(0.5)    =" (F 0.5)   "точное значение =" (/ (* 0.5 0.5) 2.0))
  (println "F(0.25)   =" (F 0.25)  "точное значение =" (/ (* 0.25 0.25) 2.0))
  (println "F(-0.537) =" (F -0.537) "точное значение =" (/ (* -0.537 -0.537) 2.0)))

(let [F (make-memoized-integral (fn [t] (* 3.0 t t)) 0.001)]
  (println "F(2)      =" (F 2.0) "точное значение =" (* 2.0 2.0 2.0)))
