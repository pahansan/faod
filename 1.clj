(require '[clojure.set :as set])

(defn variable [x]
    [:var x])

(defn lambda [x body]
    [:lambda x body])

(defn app [f x]
    [:app f x])

(defn variable? [term]
    (= :var (first term)))

(defn lambda? [term]
    (= :lambda (first term)))

(defn app? [term]
    (= :app (first term)))

(defn free-vars [term]
    (cond
        (variable? term) #{(second term)}
        (lambda? term) (disj (free-vars (last term)) (second term))
        (app? term) (set/union (free-vars (second term)) (free-vars (last term)))
    )
)

(defn naive-substitute [term x replacement]
    (cond
        (variable? term)    (if
                                (contains?
                                    (free-vars term) x
                                )
                                replacement
                                term
                            )
        (lambda? term)      (if
                                (and
                                    (contains?
                                        (free-vars term) x
                                    )
                                    (not
                                        (=
                                            (second term)
                                            x
                                        )
                                    )
                                )
                                (lambda (second term) (naive-substitute (last term) x replacement))
                                term
                            )
        (app? term)         (app
                                (naive-substitute (second term) x replacement)
                                (naive-substitute (last term) x replacement)
                            )
    )
)

(println (app (lambda 'y (variable 'x)) (variable 'z)) 'x (variable 'd))
(println (naive-substitute (app (lambda 'y (variable 'x)) (variable 'z)) 'x (variable 'd)))
