(ns tail
  (:require [clojure.string :as s])
  (:import [clojure.lang PersistentQueue]))

(deftype FQueue [queue max-capacity]
  Object
  (toString [_] (s/join ", " queue))
  
  clojure.lang.IFn
  (invoke [_] queue)
  
  clojure.lang.IPersistentStack
  (peek [_] (peek queue))
  (pop [_] (FQueue. (pop queue) max-capacity))  
  (seq [_] (seq queue))
  (count [_] (count queue)) 
  (cons [_ x]
    (let [new-queue (conj queue x)]
      (if (> (count new-queue) max-capacity)
        (FQueue. (pop new-queue) max-capacity)
        (FQueue. new-queue max-capacity)))))

(defn fixed-queue [capacity]
  (FQueue. PersistentQueue/EMPTY capacity))

(defn tail [file n]
  (with-open [reader (clojure.java.io/reader file)]
    (into (fixed-queue n) (line-seq reader) )))


