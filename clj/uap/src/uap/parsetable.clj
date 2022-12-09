(ns uap.parsetable
(:use [clojure.string :only [split split-lines trim]])
 )


(defn tokens
  [s]
  (-> s trim (split #"\s+")))

(defn pairs
  [coll1 coll2]
  (map vector coll1 coll2))


(defn parse-table
  [raw-table-data]
  (let [table-data (map tokens (split-lines raw-table-data))
        column-names (first table-data)
        row-names (map first (next table-data))
        contents (map next (next table-data))]
    (apply merge-with merge
      (for [[row-name row-contents] (pairs row-names contents)
            [column-name element] (pairs column-names row-contents)]
        {row-name {column-name element}}))))