(ns danilomo.grep
  (:require [clojure.java.io :as io]
            [clojure.tools.cli :refer [parse-opts]]))

(defn with-open-xform [step]
  (fn
    ([] (step))
    ([dst] (step dst))
    ([dst x] (with-open [y x]
               (step dst y)))))

(defn process-files [filter-func reduce-func coll]
  (transduce
   (comp (remove #(.isDirectory %))
         (map io/reader)
         with-open-xform
         (mapcat line-seq)
         (filter filter-func))
   reduce-func
   coll))

(defn print-seq [l]
  (doseq [item l]
    (println item)))

(def cli-options
  [["-h" "--help"]
   ["-i" "--insensitive"]
   ["-c" "--count"]
   ["-r" "--recursive"]])

(defn files-to-search [recursive inputs]
  (let [list-dir (if recursive file-seq #(.listFiles %))]
    (->> inputs
         (mapcat #(if (.isDirectory %) (list-dir %) [%]))
         (remove #(.isDirectory %)))))

(def print-reducer (fn ([] nil) ([_]) ([_ input] (println input))))
(def sum-reducer (fn ([] 0) ([sum] (println sum)) ([acc _] (inc acc))))

(defn -main
  [& args]
  (let [; parsing of command line args
        parsed-args (parse-opts args cli-options)
        {recursive :recursive insensitive :insensitive count-occurs :count} (:options parsed-args)
        [pattern & inputs] (:arguments parsed-args)
        ; custom functions based on command line flags
        search-function (if insensitive
                          #(.contains (.toLowerCase %) (.toLowerCase pattern))
                          #(.contains % pattern))
        reduce-function (if count-occurs sum-reducer print-reducer)
        ; separate inputs into files that exist and files that don't exist
        inputs (map io/file inputs)
        {file-inputs true non-existing-files false} (group-by #(.exists %) inputs)
        ; lazy sequence of files
        file-seq (files-to-search recursive file-inputs)]
    (print-seq (map #(str % " does not exist.") non-existing-files))
    (process-files search-function reduce-function file-seq)))

