(defn make-lazy-integral [f dx]
  (let [f0 (f 0)

        positive-grid
        (map (fn [[x prev-f acc]]
               [acc prev-f])
             (iterate
               (fn [[x prev-f acc]]
                 (let [next-x (+ x dx)
                       next-f (f next-x)
                       area (* dx (/ (+ prev-f next-f) 2.0))]
                   [next-x next-f (+ acc area)]))
               [0.0 f0 0.0]))

        negative-grid
        (map (fn [[x prev-f acc]]
               [acc prev-f])
             (iterate
               (fn [[x prev-f acc]]
                 (let [next-x (- x dx)
                       next-f (f next-x)
                       area (* dx (/ (+ prev-f next-f) 2.0))]
                   [next-x next-f (- acc area)]))
               [0.0 f0 0.0]))]

    (fn [x]
      (cond
        (zero? x)
        0.0

        (pos? x)
        (let [n (Math/floor (/ x dx))
              r (- x (* n dx))
              [acc f-node] (nth positive-grid n)]
          (if (zero? r)
            acc
            (+ acc (* r (/ (+ f-node (f x)) 2.0)))))

        :else
        (let [a (- x)
              n (Math/floor (/ a dx))
              r (- a (* n dx))
              [acc f-node] (nth negative-grid n)]
          (if (zero? r)
            acc
            (- acc (* r (/ (+ f-node (f x)) 2.0)))))))))

(let [F (make-lazy-integral (fn [t] t) 0.01)]
  (println "F(2.0)    =" (F 2.0)   "точное значение =" (/ (* 2.0 2.0) 2.0))
  (println "F(0.537)  =" (F 0.537) "точное значение =" (/ (* 0.537 0.537) 2.0))
  (println "F(0.5)    =" (F 0.5)   "точное значение =" (/ (* 0.5 0.5) 2.0))
  (println "F(0.25)   =" (F 0.25)  "точное значение =" (/ (* 0.25 0.25) 2.0))
  (println "F(-0.537) =" (F -0.537) "точное значение =" (/ (* -0.537 -0.537) 2.0)))

(let [F (make-lazy-integral (fn [t] (* 3.0 t t)) 0.001)]
  (println "F(2)      =" (F 2.0) "точное значение =" (* 2.0 2.0 2.0)))
