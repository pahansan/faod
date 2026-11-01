(defn extend-prefix [chars prefix acc i]
  (if (= i (count chars))
    acc
    (let [c (nth chars i)]
      (recur chars
             prefix
             (if (not= c (last prefix))
               (conj acc (str prefix c))
               acc)
             (inc i)))))

(defn extend-prefixes [chars prefixes acc i]
  (if (= i (count prefixes))
    acc
    (recur chars
           prefixes
           (extend-prefix chars (nth prefixes i) acc 0)
           (inc i))))

(defn extend-all [chars prefixes]
  (extend-prefixes chars prefixes [] 0))

(defn build-strings [chars prefixes n]
  (if (zero? n)
    (seq prefixes)
    (recur chars (extend-all chars prefixes) (dec n))))

(defn generate-strings [chars n]
  (build-strings chars [""] n))

(prn (generate-strings [\a \b \c] 0))
(prn (generate-strings [\a \b \c] 1))
(prn (generate-strings [\a \b \c] 2))
(prn (generate-strings [\a \b \c] 3))
