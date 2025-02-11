(ns danilomo.uniq)

(defn dedupe-count []
  (fn [rf]
    (let [prev (volatile! ::none)
          count_ (volatile! 1)]
      (fn
        ([] (rf))
        ([result] (rf result [@count_ @prev]))
        ([result input]
         (let [last @prev
               last_count @count_]
           (vreset! prev input)
           (cond
             (= last input) (do
                              (vswap! count_ inc)
                              result)
             (= ::none last) result
             :else (do
                     (vreset! count_ 1)
                     (vreset! prev input)
                     (rf result [last_count last])))))))))

(defn format-entry [[count_ entry]]
  (str count_ ": " entry))

(defn uniq
  ([file {count-lines :count-lines}]
   (with-open [reader (clojure.java.io/reader file)]
     (let [lines (line-seq reader)
           xf (if count-lines
                (comp (dedupe-count) (map format-entry))
                (dedupe))]
       (doseq [entry (eduction xf lines)]
         (println entry)))))
  ([file] (uniq file {})))

