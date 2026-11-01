(def precedence {'+ 1 '- 1 '* 2 '/ 2})

(defn apply-operator [[values operators]]
  (let [op (peek operators)
        right (peek values)
        left (peek (pop values))]
    [(conj (pop (pop values)) (list op left right))
     (pop operators)]))

(defn push-operator [state op]
  (loop [s state]
    (let [[values operators] s]
      (if (and (not (empty? operators))
               (>= (precedence (peek operators))
                   (precedence op)))
        (recur (apply-operator s))
        [values (conj operators op)]))))

(defn consume-token [state token]
  (if (precedence token)
    (push-operator state token)
    [(conj (first state) token) (second state)]))

(defn apply-remaining-operators [state]
  (loop [s state]
    (let [[values operators] s]
      (if (not (empty? operators))
        (recur (apply-operator s))
        (peek values)))))

(defn to-prefix-form [expr]
  (apply-remaining-operators (reduce consume-token [[] []] expr)))

(defn eval-expression [expr]
  (eval (to-prefix-form expr)))

(println (to-prefix-form '(1 + 4 * 3 - 5 / 2)))
(println (eval-expression '(2 + 3 * 4)))
(println (eval-expression '(10 - 3 - 2)))
(println (eval-expression '(8 / 4 * 2)))
