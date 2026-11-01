(def precedence {'+ 1 '- 1 '* 2 '/ 2})

(defn operator? [token] (contains? precedence token))

(defn variable? [token]
  (cond
    (operator? token) false
    (= clojure.lang.Symbol (type token)) true
    :else false))

(defn apply-operator [[values operators]]
  (let [op    (first operators)
        right (first values)
        left  (first (rest values))]
    [(cons (list op left right) (rest (rest values)))
     (rest operators)]))

(defn push-operator [state op]
  (let [[values operators] state]
    (cond
      (empty? operators)
      [values (cons op operators)]

      (< (precedence (first operators)) (precedence op))
      [values (cons op operators)]

      :else
      (recur (apply-operator state) op))))

(defn consume-token [state token]
  (if (operator? token)
    (push-operator state token)
    (let [[values operators] state]
      [(cons token values) operators])))

(defn apply-remaining-operators [state]
  (let [[values operators] state]
    (if (empty? operators)
      (first values)
      (recur (apply-operator state)))))

(defn to-prefix-form [expr]
  (apply-remaining-operators (reduce consume-token ['() '()] expr)))

(defn expression-variables [expr]
  (loop [tokens expr
         found  #{}]
    (if (empty? tokens)
      found
      (let [token (first tokens)]
        (recur (rest tokens)
               (if (variable? token)
                 (conj found token)
                 found))))))

(defn let-pairs [variables bindings]
  (reduce (fn [pairs var]
            (if (contains? bindings var)
              (conj pairs var (get bindings var))
              (throw (ex-info (str "Не задано значение переменной: " var) {}))))
          []
          variables))

(defn eval-expression [expr bindings]
  (let [form  (to-prefix-form expr)
        pairs (let-pairs (expression-variables expr) bindings)]
    (eval `(let ~pairs ~form))))

(defmacro infix [expr]
  (to-prefix-form expr))

(println "4a. eval-expression")
(println "(1 + 4 * 3 - 5 / 2), bindings {} =>"
         (eval-expression '(1 + 4 * 3 - 5 / 2) {}))

(println "(x + 4 * y - 5 / 2), bindings {x 10 y 4} =>"
         (eval-expression '(x + 4 * y - 5 / 2) '{x 10 y 4}))

(println "(x - y - 2), bindings {x 10 y 3} =>"
         (eval-expression '(x - y - 2) '{x 10 y 3}))

(println "(x / y * 2), bindings {x 8 y 4} =>"
         (eval-expression '(x / y * 2) '{x 8 y 4}))

(println "ошибка при отсутствии переменной =>"
         (try
           (eval-expression '(x + 1) '{y 2})
           (catch clojure.lang.ExceptionInfo e
             (.getMessage e))))

(println)
(println "4b. infix")
(println "macroexpand-1 '(infix (x + 4 * y - 5 / 2)) =>")
(println (macroexpand-1 '(infix (x + 4 * y - 5 / 2))))

(println "(infix (2 + 3 * 4)) =>" (infix (2 + 3 * 4)))
(println "(infix (10 - 3 - 2)) =>" (infix (10 - 3 - 2)))
(println "(infix (1 + 4 * 3 - 5 / 2)) =>" (infix (1 + 4 * 3 - 5 / 2)))

(println "(let [x 8 y 4] (infix (x / y * 2))) =>"
         (let [x 8 y 4]
           (infix (x / y * 2))))

(println "(let [x 10 y 4] (infix (x + 4 * y - 5 / 2))) =>"
         (let [x 10 y 4]
           (infix (x + 4 * y - 5 / 2))))
