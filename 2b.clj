(defn generate-strings-fp [chars n]
  (seq
    (reduce
      (fn [prefixes _]
        (mapcat
          (fn [prefix]
            (map (fn [c] (str prefix c))
                 (filter (fn [c] (not= c (last prefix))) chars)))
          prefixes))
      [""]
      (range n))))

(prn (generate-strings-fp [\a \b \c] 0))
(prn (generate-strings-fp [\a \b \c] 1))
(prn (generate-strings-fp [\a \b \c] 2))
(prn (generate-strings-fp [\a \b \c] 3))
