(ns clojurer.core)

(defn satom?
  [a]
  (or (integer? a) (not (list? a))))

(defn satoms-list?
  [las]
  (every? true? (map satom? las)))
