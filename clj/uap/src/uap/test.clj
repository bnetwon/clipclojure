
#(apply max-key %
  (reverse (for [x (%2 (% %3)) % (%2 x (- (% %3) 1))
                 :let [% (subvec %3 x (+ % 2))]]
            (if (apply < %) % []))))
count range

(fn [c]
  (->> (partition 2 1 c)
    (partition-by #(- (second %) (first %)))
    (filter #(= 1 (- (second (first %)) (ffirst %))))
    (reduce #(if (< (count %1) (count %2)) %2 %1) [])
    (flatten)
    (distinct)))
(fn[s]
  (let[v(apply (partial max-key count)
         (reduce
          #(if(=%2(+(or(last(last%))9)1))
            (concat(butlast%)[(concat(last%)[%2])])
            (concat%[[%2]]))

          []s))]
   (if(>(count v)1)v[])))

(fn [coll]
    (let [sub-seqs
          (reduce
            (fn [a b]
              (if-let [l (last (last a))]
                (if (= b (inc l))
                  (update-in a [(-> a count dec)] conj b)
                  (conj a (vec [b])))
                (conj a (vec [b])))) [[]] coll)]
      (if-let [longest (last (sort-by count (filter #(> (count %) 1) sub-seqs)))]
        longest
        [])))
(fn [s]
  (letfn [(max-by-count [xs ys] (if (> (count ys) (count xs)) ys xs))]
    (loop [tail s, longest-subseq [], this-subseq []]
        (if-let [[x & xs] (seq tail)]
             (if (or (empty? this-subseq) (> x (last this-subseq)))
                 (recur xs longest-subseq                            (conj this-subseq x))
                 (recur xs (max-by-count longest-subseq this-subseq) [x]))
             (let [longest (max-by-count longest-subseq this-subseq)]
                  (if (>= (count longest) 2) longest []))))))
(fn [value]
 (reduce (fn [d k]
          (if (and (< (count d) (count k)) (> (count k) 1))
            k
            d))
   []
   (reduce (fn [c v]
            (if-let [n (last c)]
              (if (= (- v 1) (last n))
                (conj (vec (butlast c)) (conj n v))
                (conj c [v]))
              (conj c [v]))) [] value)))
(fn sub-seq [ns]
        (let [pairs (partition 2 1 ns)]
             (loop [an () ps pairs]
                (if (empty? ps)
                    (let [ns (map first (butlast an))]
                         (concat ns (last an)))
                    (let [[fs rs] (split-with
                                   #(< (first %) (second %)) ps)]
                       (if (< (count an) (count fs))
                           (recur fs (rest rs))
                           (recur an (rest rs))))))))
(fn [xs]
  (reduce #(if (< (count %1) (count %2)) %2 %1)
    []
   (filter #(> (count %) 1)

     (map #(map first
             (take-while (fn [[a b]] (= a b))
                 (map vector % (iterate inc (first %)))))
        (take-while seq (iterate rest xs))))))
(fn longest [lst]
  (loop [ls (rest lst) xs '() ys (list (first lst))]
    (if (= (first ls) nil)
      (if (> (count xs) (count ys))
       (if (= (count xs) 1) [] (reverse xs))
       (if (= (count ys) 1) [] (reverse ys)))
      (if (or (= (first ys) nil)
           (= (+ (first ys) 1) (first ls)))
       (recur (rest ls) xs (conj ys (first ls)))
       (if (> (count xs) (count ys))
          (recur (rest ls) xs (list (first ls)))
          (recur (rest ls) ys (list (first ls))))))))
(fn incr-seq [xs]
  (loop [longest-seq [(first xs)]
         current-seq [(first xs)]
         xs (next xs)]
    (if (empty? xs)
      (if-not (next longest-seq)
        []
        longest-seq)
      (let [newcurrent (if (= (first xs) (inc (last current-seq)))
                         (conj current-seq (first xs))
                         [(first xs)])
            newlongest (if (> (count newcurrent)
                              (count longest-seq))
                         newcurrent
                         longest-seq)]
        (recur newlongest
               newcurrent
               (next xs))))))
(fn [l]
  (map first (loop [m [] l
                    (map (fn [x y] [x (- x y)]) l (cons (first l) l))]
              (if (empty? l) m
                  (let [[j k] (split-with #(not (= 1 (last %))) l)
                        [k r] (split-with #(= 1 (last %)) k)]
                    (recur (if (and (not (empty? k)) (>= (count k) (count m))) (cons (last j) k) m)
                           r))))))
(fn [xxs]
  (loop [xxs xxs rise [] best []]
    (if (empty? xxs)
      (if (> (count best) 1) best [])
      (let [[x & xs] xxs]
        (if (or (empty? rise) (> x (last rise)))
          (recur xs (conj rise x)
                 (if (>= (count rise) (count best))
                    (conj rise x)
                   best))
          (recur xs [x] (if (= (count best) 0) [x] best)))))))
(fn [coll]
  (reduce (fn [r c]
            (if (> (count c) (count r))
              c
              r))
          '()
          (filter #(>= (count %1) 2)
                  (map (fn [i]
                         (loop [r `(~(nth coll i))
                                i (+ i 1)]
                           (cond (>= i (count coll))
                                 (reverse r)
                                 (= (- (nth coll i) 1) (first r))
                                 (recur (conj r (nth coll i)) (+ i 1))
                                 :else
                                 (reverse r))))
                       (range (count coll))))))
(fn [my-list]
    (let [sequences ((fn find-seq
                      ([i-list] (find-seq [] i-list))
                      ([result i-list]
                       (if (zero? (count i-list))
                           result
                           (let [current (first i-list) next (rest i-list) head (drop-last result) tail (last result)]
                               (if (or (zero? (count result)) (not= (inc (last tail)) current))
                                   (find-seq (conj (apply vector result) (vector current)) next)
                                   (find-seq (conj (apply vector head) (conj tail current)) next))))))
                     my-list)]
        (let [sequence-max (apply max (map count sequences))]
            (let [filtered-sequence (filter
                                     (fn [sequence]
                                         (let [sequence-count (count sequence)]
                                             (and (>= sequence-count 2)
                                                 (= sequence-count sequence-max))))
                                     sequences)]
                (if (zero? (count filtered-sequence)) [] (first filtered-sequence))))))
#(let [result (second (reduce (fn [[acc longest] elm]
                               (if (= (inc (last acc)) elm)
                                   (if (>= (inc (count acc)) (count longest))
                                       [(conj acc elm) (conj acc elm)]
                                       [(conj acc elm) longest])
                                   (if (> (count acc) (count longest))
                                       [[elm] acc]
                                       [[elm] longest])))
                       [[(first %)] []]
                       %))]
    (if (= 1 (count result))
      []
      result))
(fn longshot [l]
  (loop [acc (list (first l))
         tam 1
         macc '()
         mtam 0
         llista (rest l)]


    (cond
      (empty? llista)
      (if (zero? mtam)
        (list)
        (if (> tam mtam)
          (reverse acc)
          (reverse macc)))


      (= (first acc) (dec (first llista))) (recur (cons (first llista) acc) (inc tam) macc mtam (rest llista))
      :else
          (if (and (> tam mtam) (>= tam 2))
            (recur (cons (first llista) '()) 1 acc tam (rest llista))
            (recur (cons (first llista) '()) 1 macc mtam (rest llista))))))




(fn [coll]
  (let [c (map #(vector %
                        (let [[x y] %2] (if y (- y x) nil)))
               coll (partition-all 2 1 coll)),
        bigger (fn [l c]
                 (let [ls (count l)
                       cs (count c)
                       bg (if (> cs ls) c l)]
                   (if (> (count bg) 1) bg [])))]
    (loop [longest [], current [], pairs c]
      (let [[v diff] (first pairs)]
        (if (nil? diff)
          (bigger longest (conj current v))
          (if (> diff 0)
            (recur longest (conj current v) (rest pairs))
            (recur (bigger longest (conj current v)) [] (rest pairs))))))))
(fn [input]
  (if (seq input)
    ((fn [input subresult result]
      {:pre [(seq subresult)]}
      (if (seq input)
        ;; nonempty input: compare first in input with the last in subresult and recur
        (if (> (first input) (last subresult))
          (recur (rest input) (conj subresult (first input)) result)
          (recur (rest input) [(first input)] (if (> (count subresult) (count result)) subresult result)))
        ;; empty input: return result or subresult depending on which one is larger
        (let [r (if (> (count subresult) (count result)) subresult result)]
          (if (< 1 (count r)) r [])))) (rest input) [(first input)] [])
    []))
(fn [coll]
  (->> (partition 2 1 coll)
    (partition-by #(- (second %) (first %)))
    (filter #(= 1 (- (second (first %)) (ffirst %))))
    (reduce #(if (< (count %1) (count %2)) %2 %1) [])
    (flatten)
    (distinct)))
(fn [xs]
    (first (reduce (fn [[longest current] x]
                    (let [ new-current (conj current x)]
                        (if (= (last current) (dec x))
                          (if (and (> (count new-current) (count longest))
                                (> (count new-current) 1))
                            [new-current new-current]
                            [longest new-current])
                          (if (and (> (count current) (count longest))
                                (> (count current) 1))
                            [current [x]]
                            [longest [x]]))))
            [[] []] xs)))
(fn [xs]
  (loop [rs   ()
         r    [(first xs)]
         more (next xs)]
    (if (seq more)
      (if (= (inc (peek r)) (first more))
        (recur rs          (conj r (first more)) (next more))
        (recur (conj rs r) [(first more)]        (next more)))
      (reduce
        (fn [a c] (if (and (> (count c) 1) (> (count c) (count a))) c a))
        []
        (conj rs r)))))
(fn [coll]
  (->> (partition 2 1 coll)
    (partition-by #(- (second %) (first %)))
    (filter #(= 1 (- (second (first %)) (ffirst %))))
    (reduce #(if (< (count %1) (count %2)) %2 %1) [])
    flatten
    distinct))
(letfn [
        (step [seqs item]
            (if (> item (peek (first seqs)))
                (conj (rest seqs) (conj (first seqs) item))
                (conj seqs [item])))

        (max-increasing-subseq [coll]
            (if (zero? (count coll))
                []
                (let [subseqs (reduce step (list [(first coll)]) (rest coll))
                      res (apply max-key count subseqs)]
                    (if (< (count res) 2) [] res))))]


    #(max-increasing-subseq %))

(fn [s]
  (loop [max-seq [(first s)] current-seq [(first s)] i 1]
    (if (= i (count s))
      (if (< 1 (count max-seq))
          max-seq
          [])
      (let [num (nth s i)]
        (if (= (inc (last current-seq)) num)
          (if (= (count max-seq) (count current-seq))
            (recur (conj current-seq num) (conj current-seq num) (inc i))
            (recur max-seq (conj current-seq num) (inc i)))
          (recur max-seq [num] (inc i)))))))
(fn longest-inc-seq [coll]
  (reduce
    (fn [a b]
      (let [len-a (count a)
            len-b (count b)]
        (if (and (> len-b 1) (> len-b len-a)) b a)))
    (reductions
      (fn [xs y]
        (let [x (last xs)]
         (if (and x (> y x))
           (conj xs y)
           [y])))
      [] coll)))
(fn longest-inc-subseq [xs]
  (let [pt (partition 2 1 xs)
        _ (prn "1: " pt)
        pt (partition-by (fn [[x y]] (< x y)) pt)
        _ (prn "2: " pt)
        pt (for [[[x1 x2] & rxs] pt
                 :when (< x1 x2)]
             (concat [x1 x2] (map second rxs)))
        _ (prn "3: " pt)
        pt (loop [pt pt maxseq nil]
             (if (empty? pt)
               maxseq
               (let [[p & rpt] pt]
                 (if (> (count p) (count maxseq))
                   (recur rpt p)
                   (recur rpt maxseq)))))
        _ (prn "4: " pt)]
    (vec pt)))
#(let [succ? (fn [coll v] (or (empty? coll) (= (last coll) (dec v))))
       f-max-succ (fn [result v]
                   (let [new_succ (or (last result) [])
                         new_succ (if (succ? new_succ v)
                                      (conj new_succ v) [v])
                         result (if (> (count new_succ)
                                       (count (first result)))
                                    [new_succ new_succ]
                                    [(first result) new_succ])]
                        result))
        max-succ (first (reduce f-max-succ [[] nil] %1))]
   (if (>= (count max-succ) 2) max-succ []))
#(reverse
    (nth
      (reduce
        (fn [[s c] e]
          (if (empty? c)
            [s (cons e c)]
            (let
              [x (nth c 0)
               d (if (= (+ 1 x) e) (cons e c) [e])
               t (if (> (count d) (count s)) d s)
               n (if (> (count t) 1) t [])]
              [n d])))
        [[][]]
        %)
      0))
(fn [xs]
   (let [get-max-seq #(if (>= (count %1)
                             (count %2))
                        %1
                        %2)]

     (loop [max-seq [(first xs)]
            acc-seq [(first xs)]
            xs (rest xs)]

       (let [x (first xs)]
         (if (seq xs)
           (if (> x
                   (peek acc-seq))

             (recur (get-max-seq max-seq acc-seq) (conj acc-seq x) (rest xs))
             (recur (get-max-seq max-seq acc-seq) [x] (rest xs)))
           (if (>= (count (get-max-seq max-seq acc-seq)) 2)
             (get-max-seq max-seq acc-seq)
             []))))))

(fn [col]
  (let [sublists (take-while
                    #(not (empty? %))
                     (iterate #(drop 1 %) col))
        ll (apply max-key count
            (map (fn [l] (some
                          #(if (= % (range (first %) (+ (first %) (count %)))) % false)
                          (take (- (count l) 1) (iterate drop-last l))))
                 sublists))]
       (if ll ll [])))
(fn [s]
  (letfn
    [(reducer [[largest current] n]
      (if (> n (or (first current) 0))
        [largest (cons n current)]
        (if (> (or (count current) 0) (or (count largest) 0))
          [current (list n)]
          [largest (list n)])))]
    (reverse
      (let [[one two :as r] (reduce reducer [[][]] s)]
        (cond
          (and (> 2 (or (count one) 0)) (> 2 (or (count two) 0))) '()
          (>= (count one) (count two)) one
          :else two)))))
(fn liss [initxs]
  (loop [ret [], cur[], xs initxs]
    (if (empty? xs)
      (if (> (count ret) 1)
        ret
        [])
      (if (or (empty? cur) (> (first xs) (last cur)))
        (if (> (count (conj cur (first xs))) (count ret))
          (recur (conj cur (first xs)) (conj cur (first xs)) (rest xs))
          (recur ret (conj cur (first xs)) (rest xs)))
        (recur ret [(first xs)] (rest xs))))))
(fn [x]
 (let [
       s (mapcat #(take-while seq (iterate butlast %)) (take-while seq (iterate rest x)))
       s (filter #(and (apply < %) (> (count %) 1)) s)]
   (reduce #(if (> (count %2)(count %)) %2 %) [] s)))
(fn [x] (->> x
         (iterate next)
         (take-while identity)
         (mapcat (fn [s] (->> s
                          (iterate butlast)
                          (take-while #(> (count %) 1))
                          (filter (partial apply <)))))
         (cons [])
         (reduce #(if (> (count %2) (count %1)) %2 %1))))

(fn [coll]
  (->> (partition 2 1 coll)
    (partition-by #(- (second %) (first %)))
    (filter #(= 1 (- (second (first %)) (ffirst %))))
    (reduce #(if (< (count %1) (count %2)) %2 %1) [])
    flatten
    distinct))
(fn [coll]
  (letfn ((inc? [[a b]] (= a (dec b))))
    (distinct
     (apply concat
            (first
             (sort #(> (count %1) (count %2))
                   (filter #(inc? (first %))
                           (partition-by inc?
                                         (partition 2 1 coll)))))))))
(fn[s]
  (let [x (apply max-key count (reverse
                                (loop [v (rest s) r [[(first s)]]]
                                  (if (empty? v) r
                                   (recur (rest v)
                                     (if
                                       (< (last(last r)) (first v))
                                       (assoc r (dec (count r)) (conj (last r) (first v)))
                                      (conj r [(first v)])))))))]
   (if (> (count x) 1) x [])))
(fn liss
  ([coll] (if (empty? coll) [] (liss (rest coll) [] 0 (vector (first coll)) 1 (first coll))))
  ([coll bestseq bestlen actseq actlen lastelem]
   (cond
     (empty? coll)
     (let [seq (if (> actlen bestlen) actseq bestseq)
           len (count seq)]
          (if (> len 1) seq []))
     (< lastelem (first coll))
     (liss (rest coll) bestseq bestlen (conj actseq (first coll)) (inc actlen) (first coll))
     true
     (liss (rest coll)
           (if (> actlen bestlen) actseq bestseq)
           (max actlen bestlen)
           (vector (first coll))
           1
           (first coll)))))

(fn longest-increasing-sub-seq
  ([x]
   (longest-increasing-sub-seq (rest x) [(first x)] [(first x)]))
  ([x y z]
   (println z)
   (if (= 0 (count x))
     (if (> (count z) 1)
       z
       [])
     (if (= (first x) (inc (last y)))
       (if (= (count y) (count z))
         (recur (rest x) (conj y (first x)) (conj y (first x)))
         (recur (rest x) (conj y (first x)) z))
       (recur (rest x) [(first x)] z)))))
#(apply max-key %
  (reverse
   (for [x (%2 (% %3)) % (%2 x (- (% %3) 1))
         :let [% (subvec %3 x (+ % 2))]]
     (if (apply < %) % []))))
count range
(fn [s]
 (loop [i 0 max []]
  (cond
   (>= i (dec (count s))) max
   (< (s i) (s (inc i)))
   (let [lmax
         (into [(s i)]
          (for [j (range (inc i) (count s))
                :while (< (s (dec j)) (s j))] (s j)))]
     (if (> (count lmax) (count max))
       (recur (inc i) lmax)
       (recur (inc i) max)))
   true (recur (inc i) max))))
(fn lis [lt]
  (letfn [(split [lt u bs acc]
                 (if (seq lt)
                   (let [ltf (first lt)
                         ltr (rest lt)]
                     (if (<= ltf u)
                       (split
                         ltr ltf [] (conj acc [(inc (count bs))
                                               (conj bs u)]))
                       (split
                         ltr ltf (conj bs u) acc)))
                   (conj acc [(inc (count bs)) (conj bs u)])))
          (max-len [lt m ml]
                   (if (seq lt)
                     (let [f (first lt), r (rest lt), fl (first f)]
                       (if (> fl ml)
                         (max-len r (rest f) fl)
                         (max-len r m ml)))
                     (first m)))]
    (let [res-split (split (rest lt) (first lt) [] [])
          ls (max-len (rest res-split) (rest (first res-split)) (first (first res-split)))]
      (if (= (count ls) 1)
        []
        ls))))
(fn longest-increasing-subseq [coll]
  (loop [res [] cur [(first coll)] c (rest coll)]
    (if (empty? c)
      res
      (let [f (first c)]
        (if (> f (last cur))
          (let [curf (conj cur f)]
            (if (> (count curf) (count res))
              (recur curf curf (rest c))
              (recur res curf (rest c))))
          (recur res [f] (rest c)))))))
(fn [as]
   (loop [as as curr-s '() longest-s '()]
     (if (nil? (seq as))
       (let [xs (if (> (count curr-s) (count longest-s)) curr-s longest-s)]
         (if (> (count xs) 1)
           (reverse xs)
           []))
       (cond
        (or (nil? (seq curr-s)) (= (inc (first curr-s)) (first as)))
        (recur (rest as) (conj curr-s (first as)) longest-s)
        (> (count curr-s) (count longest-s))
        (recur as '() curr-s)
        :else
        (recur as '() longest-s)))))
(fn largest-increasing-subseq [C]
  (loop [C C [cur largest-so-far] [[] []]]
    (if (empty? C)
      largest-so-far
      (recur (rest C)
             (let [x (first C)]
               (if (and (not-empty cur)
                        (> x (last cur)))
                 (let [cur (conj cur x)]
                   [cur (if (> (count cur) (count largest-so-far))
                          cur
                          largest-so-far)])
                 [[x] largest-so-far]))))))
(fn [coll]
    (:champ (reduce (fn [{prev :prev :as results} current]
                      (-> results
                          (update-in [:current] (if (> current prev)
                                                  conj
                                                  (fn [_ x] [x])) current)
                          (assoc :prev current)
                          ((fn [{:keys [current champ] :as results}]
                             (assoc results :champ (if (and (> (count current)
                                                               (count champ))
                                                            (> (count current) 1))
                                                     current
                                                     champ))))))
                    {:prev Double/NEGATIVE_INFINITY
                     :current []
                     :champ []}
                    coll)))
#(:longest
   (reduce
     (fn [{:keys [longest cur] :as res} n]
       (let [{:keys [cur] :as updated} (if (or (not (seq cur))
                                               (> n (last cur)))
                                         (update-in res [:cur] conj n)
                                         (assoc res :cur [n]))
             longest-cnt (count longest)
             cur-cnt (count cur)]
         (if (and (> cur-cnt 1)
                  (> cur-cnt longest-cnt))
           (assoc updated :longest cur)
           updated)))
     {:longest [] :cur []} %))
(fn [coll]
  (let [a (last (sort-by #(count %)(filter #(< 1 (count %))
                                    (reduce (fn [col x](if (= (last (last col))(dec x))
                                                        (conj (vec (drop-last col))(conj (last col) x))
                                                        (conj col [x])))[] coll))))]
       (if (nil? a) [] a)))
(fn [v]
  (if (empty? v)
    []
    (let [n (count v)
          f (fn [start i previous [best-start best-len :as best]]
              (let [new-best (fn []
                               (let [new-len (- i start)]
                                 (if (and (> new-len 1) (< best-len (- i start)))
                                   [start (- i start)]
                                   [best-start best-len])))]
                (if (= i n)
                  (let [[start length] (new-best)]
                    (subvec v start (+ start length)))
                  (let [current (nth v i)]
                    (if (> current previous)
                      (recur start (inc i) current best)
                      (recur i (inc i) current (new-best)))))))]
      (f 0 0 (nth v 0) [0 0]))))
(fn [a]
  (loop [prev nil, n (first a), s a, best nil, curr nil]
    (if
      (empty? s)
      (if (> (count curr) (count best)) (vec curr) (vec best))
      (recur (first s) (second s) (rest s)
        (if (> (count curr) (count best)) curr best)
        (if
          (and (not (nil? prev)) (> n prev))
          (if
            (nil? curr)
            (list prev n)
            (concat curr (list n)))
          nil)))))
(fn stiga [c]
  (remove #(= % "bajs")
    (distinct
     (flatten
         (loop [col (map #(if (= (first %) (dec (last %))) % "bajs")
                     (partition 2 1 c))]
          (if (not-empty (partition 2 1 (remove #(= % "bajs") col)))
            (recur (map
                    #(if (= (last (first %)) (first (last %))) % "bajs")
                    (partition 2 1 col)))
            col))))))
#(letfn [(finds [acc xs n longest]
          (let [m (first xs)]
            (if (empty? xs)
             longest
             (if (= (dec m) n)
                 (if (> (count (conj acc m)) (count longest))
                   (recur (conj acc m) (rest xs) m (conj acc m))
                   (recur (conj acc m) (rest xs) m longest))
                 (recur [m] (rest xs) m longest)))))]
        (let [res (finds [(first %1)] (rest %1) (first %1) [(first %1)])]
           (if (> (count res) 1)
               res
               [])))
(fn [s]
    (loop [longest [(first s)]
           accum [(first s)]
           s (rest s)]
      (if (seq s)
        (if (= (first s) (+ (last accum) 1))
          (let [accum (conj accum (first s))]
            (if (> (count accum) (count longest))
              (recur accum accum (rest s))
              (recur longest accum (rest s))))
          (recur longest [(first s)] (rest s)))
        (if (> (count longest) 1)
          longest
          []))))
(fn solucion-53 [xss]
  (letfn [(subsucesiones [xs]
            (cond (= (count xs) 0) ()
                  (= (count xs) 1) (list xs)
                  true (let [[x y & zs] xs
                             [us & vss :as p] (subsucesiones (rest xs))]
                         (if (< x y)
                           (cons (cons x us) vss)
                           (cons (list x) p)))))
          (ordenadas [xss]
            (sort-by (fn [xs] (- (count xs))) xss))
          (masLarga [xs]
            (first (ordenadas xs)))]
    (let [ys (masLarga (subsucesiones xss))]
      (if (< (count ys) 2) [] ys))))
(fn longest [coll]
  (loop [result [] current [] coll coll]
    (cond (empty? coll)
          (reverse
            (if (and (> (count current) 1)
                     (> (count current) (count result)))
                current
                result))
          (or (empty? current)
              (= (first coll) (inc (first current))))
          (recur result
                (cons (first coll) current)
                (next coll))
          :else (recur (if (and (> (count current) 1)
                                (> (count current) (count result)))
                          current
                          result)
                      []
                      coll))))
(fn [col]
  (reduce
    #(if (and (> (count %2) 1)
              (> (count %2) (count %)))
         %2
         %)
    []
    (reductions
      (fn [sub i]
        (if (= (- i (last sub)) 1)
          (conj sub i)
          [i]))
      [(first col)]
      (rest col))))
(fn longest-increasing-subseq [xs]
  (letfn [(inc-subseq [xs accum]
            (if (or (empty? xs) (and (seq accum) (<= (first xs) (last accum))))
              (if (>= (count accum) 2) accum [])
              (recur (rest xs) (conj accum (first xs)))))]
    (loop [xs xs longest []]
      (if (empty? xs)
        longest
        (let [s (inc-subseq xs [])]
          (if (> (count s) (count longest))
            (recur (rest xs) s)
            (recur (rest xs) longest)))))))
(fn p53 [xs]
  (letfn [(tails [xs] (if (empty? xs) nil
                       (lazy-seq (cons xs (tails (drop 1 xs))))))
          (increasing-seq [xs] (loop [r [] xs xs l -1]
                                (cond (empty? xs) r
                                          (<= (first xs) l) r
                                          :else (recur (conj r (first xs)) (rest xs) (first xs)))))]

    (loop [longest [] seqs (filter #(> (count %) 1) (tails xs))]
      (if (empty? seqs) (if (> (count longest) 1) longest [])
       (let [s (increasing-seq (first seqs))]
          (recur (if (> (count s) (count longest)) s longest)
            (rest seqs)))))))
(fn [[x & xs]]
  (let [[ys zs] (reduce
                  (fn [[last-acc, cur-acc] x]
                    (if (= (last cur-acc) (dec x))
                      [last-acc, (conj cur-acc x)]
                      (if (> (count cur-acc) (count last-acc))
                        [cur-acc, [x]]
                        [last-acc, [x]])))
                  [[] [x]]
                  xs)
        result (if (> (count ys) (count zs)) ys zs)]
    (if (> (count result) 1) result [])))
#(loop [i 1 start 0 max-len 1 max-start 0]
   (cond (= i (count %))
     (if (> max-len 1)
       (take max-len (drop max-start %))
       [])
     (not (= (nth % i) (inc (nth % (dec i)))))
     (recur (inc i) i max-len max-start)
     (> (inc (- i start)) max-len)
     (recur (inc i) start (inc (- i start)) start)
     :else
     (recur (inc i) start max-len max-start)))
(fn [[f & v :as q]]
  (loop [[b & c] v, a f, s 0, r 0, m 0, i 1]
    (if b
      (let [e (> b a) k (- i s -1) j (+ i 1)]
        (if e
          (recur c b s (if (> k m) s r) (max k m) j)
          (recur c b i r m j)))
      (take m (drop r q)))))
#(apply max-key %
        (into ()
              (for [x (%2 (% %3)) y (%2 (+ 2 x) (+ 1 (% %3)))
                    :let [l (subvec %3 x y)]]
                (if (apply < l) l []))))
count range
(fn [q]
    (apply max-key count []
           (reverse
            (filter
             #(apply < %)
             (for [x (range (count q))
                   y (range (+ 2 x) (+ 1 (count q)))]
               (subvec q x y))))))
(fn [f e n c]
  (loop [r [] c c i []]
    (if (empty? c)
      r
      (let [j (conj i (f c))]
        (if (and (> (n j) (n r)) (> (n j) 1))
          (if (= (inc (f c)) (f (e c)))
            (recur j (e c) j)
            (recur j (e c) []))
          (if (= (inc (f c)) (f (e c)))
            (recur r (e c) j)
            (recur r (e c) [])))))))
first rest count
(fn [coll]
  (or (first (filter #(apply < %)
                     (#(let [n (inc (count %))]
                         (for [l (reverse (range 2 n))
                               i (range 0 (- n l))]
                           (take l (drop i %))))
                       coll))) []))
(fn[a-seq]
  (letfn [(max-of [s1 s2] (if (> (count s1) (count s2)) s1 s2))]
    (loop [ret []
           curr [(first a-seq)]
           ss (rest a-seq)]
      (if (empty? ss)
        (let [ret (max-of curr ret)]
          (if (> (count ret) 1) ret []))
        (if (> (first ss) (last curr))
          (recur ret (conj curr (first ss)) (rest ss))
          (recur (max-of curr ret) [(first ss)] (rest ss)))))))
(fn
  [s]
  (
    (fn inner [longest cur sub-seq]
      (if (> (count sub-seq) 1)
        (if (> (first sub-seq) (last cur))
          (let [new-cur (conj cur (first sub-seq))]
            (if (> (count new-cur) (count longest))
              (if (>= (count new-cur) 2)
                (inner new-cur new-cur (rest sub-seq))
                (inner longest new-cur (rest sub-seq)))

              (inner longest new-cur (rest sub-seq))))


          (inner longest [(first sub-seq)] (rest sub-seq)))

        (if (> (first sub-seq) (last cur))
          (if (> (+ (count cur) 1) (count longest))
            (conj cur (first sub-seq))
            longest)

          longest)))




    [] [(first s)] (rest s)))

(fn [xs]
  (let [cnt (count xs)]
    (if (>= 1 cnt)
      xs
      (let [is-inc #(< (xs (dec %)) (xs %))
            release #(if (< (count %) 2) '() %)
            subvec #(subvec xs %1 %2)
            last (dec cnt)]
        (loop [start 0 end 0
               curstart 0 curend 1]
          (if (== curend last)
            (if (is-inc curend)
              (if (< (- end start) (- curend  curstart))
                (release (subvec curstart (inc curend)))
                (release (subvec start    (inc end))))
              (if (< (- end start) (- (dec curend) curstart))
                (release (subvec curstart curend))
                (release (subvec start (inc end)))))
            (if (is-inc curend)
              (recur start end curstart (inc curend))
              (if (< (- end start) (- curend 1 curstart))
                 (recur curstart (dec curend) curend (inc curend))
                 (recur start end curend (inc curend))))))))))
(fn longest-increasing [col]
        (loop [col col current [(first col)] longest []]
          (if-not (seq col)
            longest
            (let [current (if (= (inc (last current)) (first col))
                            (conj current (first col))
                            [(first col)])
                  longest (if (and (> (count current) (count longest))
                                   (> (count current) 1))
                            current
                            longest)]
              (recur (rest col) current longest)))))
(fn [coll]
  (loop [r [] acc [] prev nil coll coll]
    (if-let [[a & coll] coll]
      (if (nil? prev)
        (recur r [a] a coll)
        (if (= a (inc prev))
          (recur r (conj acc a) a coll)
          (recur (conj r acc) [a] a coll)))
      (if-let [r (first (sort #(compare (count %2) (count %1)) (filter #(> (count %) 1) (conj r acc))))]
        r
        []))))
(fn f [c r p s]
    (if (and (> (count c) (count r)) (> (count c) 1))
      (f c c p s)
      (if (empty? s)
        r
        (let [[a & b] s]
          (f (if (< p a)
              (conj c a)
              (vector a))
            r a b))))) [] [] 0
(fn longest-increasing-sub-section
  [coll]
  (letfn [(next? [coll n]
           (= (dec n) (peek coll)))
          (improve [[best current] n]
              (if (next? current n)
                  (let [new-current (conj current n)
                        new-best (if (> (count new-current) (count best))
                                  new-current
                                  best)]
                      [new-best new-current])
                  [best [n]]))]
    (first (reduce improve [[] []] coll))))
(fn [s]
  ; Slow (n^2) version
  (let [tails #(take-while seq (iterate rest %))
        inc-seq (fn [s]
                  (loop [[x & xs :as l] s
                         m x
                         result []]
                    (if (and (seq l) (or (empty? result) (> x m)))
                      (recur xs x (conj result x))
                      result)))
            lseq (reduce
                  #(if (>= (count %1) (count %2)) %1 %2)
                  []
                  (map inc-seq (tails s)))]
    (if (> (count lseq) 1) lseq [])))

(fn [xs]
    (letfn [(reducer [[iseqs iseq ix] x]
              (if (= (dec x) ix)
                [iseqs (conj iseq x) x]
                [(conj iseqs iseq) [x] x]))

            (max-length [xss]
              (reduce (fn [xs ys]
                        (if (< (count xs) (count ys))
                          ys
                          xs))
                      []
                      xss))]
      (let [[iseqs iseq _] (reduce reducer
                                   [[] [(first xs)] (first xs)]
                                   (rest xs))]
        (max-length
         (filter #(> (count %) 1)
                 (conj iseqs iseq))))))
(fn [[h & r]]
  (or
    (last
      (filter #(> (count %) 1)
        (sort-by count
          (reduce (fn [[h & r :as a] e]
                   (if (< (peek h) e)
                       (conj r (conj h e))
                       (conj a [e]))) (list [h]) r))))
   []))
(letfn [
        (increasing-tails [lst]
          (let [fst (first lst)
                snd (second lst)
                rst (rest lst)]
            (cond
              (nil? snd) (list fst)
              (nil? fst) '()
              (< fst snd) (cons fst
                           (increasing-tails rst))
              :else (list fst))))

        (get-suffixes [lst]
          (if (empty? lst) '()
            (cons lst
              (get-suffixes (rest lst)))))

        (longest-lst [a b]
          (let [len-a (count a)
                len-b (count b)]
            (cond
              (and
                (> 2 len-a) (> 2 len-b)) '()
              (< len-a len-b) b
              :else a)))]

 (fn longest-increasing-sub-seq [lst]
   (reduce longest-lst
     (map increasing-tails
       (get-suffixes lst)))))
(fn liss [coll]
  (loop [lsf    []
         curr   []
         prev   nil
         coll   coll]
    (if (seq coll)
      (let [h     (first coll)
            r     (rest coll)
            c     (count curr)]
        (if (and (not (nil? prev)) (> h prev))
          (recur lsf (conj curr h) h r)
          (recur (if (and (> c 1)
                          (> c (count lsf)))
                   curr
                   lsf)
                 [h]
                 h
                 r)))
      (if (and (> (count curr) 1) (> (count curr) (count lsf))) curr lsf))))
(fn
  [coll]
  (let [pcoll (for [[f n] (partition 2 1 coll)]
                (when (< f n)
                  [f n]))
        liss (->> pcoll
                  (partition-by nil?)
                  (remove (comp nil? first))
                  (sort-by (comp - count))
                  first)]

    (if (seq liss)
      (cons (ffirst liss) (map second liss))
      [])))
(fn sub-seq
  ([s](sub-seq s 2 [] [] -1))
  ([s len p c ps]
   (if s
     (let [x (first s) more (next s) cur (conj c x)]
       (if (> x ps)
         (recur more len p cur x)
         (if (and (>= (count c) len) (> (count c) (count p)))
           (recur s len c [] -1)
           (recur s len p [] -1))))

     (if (>= (count c) len) (if (> (count c) (count p)) c p) []))))

(fn  [xs]
     (let [xs1 (reductions (fn [{:keys [prev]} b]
                              {:result (= (- b prev) 1)
                                       :prev b})
                           {:prev (first xs)}
                           (rest xs))
               [xs2 & _] (sort-by (comp - count)
                                  (filter #((complement nil?) (first %))
                                          (partition-by nil?
                                                        (map-indexed (fn [i {:keys [result]}]
                                                                        (when result
                                                                          i))
                                                                     xs1))))]
       (if (seq xs2)
           (subvec xs
                   (dec (first xs2))
                   (inc (last xs2)))
         [])))
(fn [x]
    (loop [res [], current-subseq [], y (rest x), current (first (rest x)), previous (first x)]
     (cond (= current nil) (if (> (count current-subseq) (count res)) current-subseq res)
             (and (= [] current-subseq) (> current previous)) (recur  res (conj (conj current-subseq previous) current) (rest y) (first y) current)
       (> current previous) (recur res (conj current-subseq current) (rest y) (first y) current)
             (> (count current-subseq) (count res)) (recur current-subseq [] (rest y) (first y) current)

             :else (recur res [] (rest y) (first y) current))))
(fn longest-sequence- [coll]
  "53. Given a vector of integers, find the longest consecutive sub-sequence of increasing numbers."
  (->> coll
       (partition 2 1) ; split into overlapping pairs
       (partition-by #(let [[l r] %] (> r l))) ; split into ascending and descending runs
       (filter #(let [[l r] (first %)] (> r l))) ; remove descending runs
       (map flatten) ; join ascending runs into a flat sequence of pairs
       (map #(concat (take-nth 2 %) (list (last %)))) ; remove the overlaps from the pairs
       (group-by count) ; map by length of run
       (sort) ; order by length
       (last) ; take the longest set of runs
       (second) ; take the runs themselves
       (first) ; take the first of the longest runs
       (vec))) ; convert it to a vector

(fn longest-seq
  [coll]
  (loop [res [(first coll)]
         curr [(first coll)]
         coll (rest coll)]
    (if (seq coll)
      (if (< (last curr) (first coll))
        (if (> (inc (count curr)) (count res))
          (recur (conj curr (first coll)) (conj curr (first coll)) (rest coll))
          (recur res (conj curr (first coll)) (rest coll)))
        (recur res [(first coll)] (rest coll)))
      (if (= 1 (count res))
        []
        res))))
(fn [s]
  (letfn
    [(by-count [a b]
       (let [ca (count a)
             cb (count b)]
         (cond
          (< (max ca cb) 2) []
          (= ca cb) a
          (< ca cb) b
          (> ca cb) a)))]
    (loop [longest []
           cand    []
           [h & t :as l] s]
      (if (empty? l)
         (by-count longest cand)
         (if (or (empty? cand)
                (< (last cand) h))
            (recur longest (conj cand h) t)
            (recur (by-count longest cand)
                   [h]
                   t))))))
(fn [v]
    (letfn [(good-pair? [p] (= (inc (p 0)) (p 1)))
            (groups [coll] (partition-by good-pair? (map vector coll (rest coll))))
            (good-runs [colls] (filter #(good-pair? (first %)) colls))
            (longest [colls] (reduce #(if (> (count %1) (count %2)) %1 %2) [] colls))
            (regroup [colls] (conj (reduce #(conj %1 (first %2)) [] colls) (last (last colls))))]

           (let [r (regroup (longest (good-runs (groups v))))]
             (if (nil? (first r)) [] r))))


#(loop [v (next %)
        d []
        c [(first %)]]

  (if (not v)
    (if (> (count d) 1) d [])
    (let [nc (if (= (+ (last c) 1) (first v))
                (conj c (first v))
                [(first v)])]
     (recur (next v)
            (if (> (count nc) (count d)) nc d)
            nc))))
(fn [x] (let [x (reverse x)
              y (filter ffirst (partition-by first
                                (map (juxt > list) x (rest x))))]
             (if-not (empty? y)
              (let [l (apply max-key count  y) m (map second l) f (ffirst m)]
                 (vec (reverse (cons f (map second m)))))
              [])))
(fn [s]
  (let [c (reductions #(if %2 (inc %1) 0) 0
             (map > (rest s) s))
        a (apply max c)
        i (.indexOf c a)]
    (if (> a 0)
      (map s (range (- i a) (+ i 1)))
      [])))
#(let [k count
              m (fn [a b] (if (and (> (k a) 1) (> (k a) (k b))) a b))]
      (apply m
             (reduce
              (fn [[s l] v] (if (or (not (seq s)) (> v (last s)))
                                [(conj s v) l]
                                [[v] (m s l)]))
              [[] []] %)))
(fn longest-increasing-sub-seq [coll]
  (let [sub-seqs-increasing   (filter #(= % (sort (set %)))
                               (for [i (range (count coll))
                                     j (range (inc i) (count coll))]
                                (take (- (inc j) i) (drop i coll))))
        max-size              (if (not (empty? sub-seqs-increasing))
                               (apply max (map count sub-seqs-increasing))
                               0)
        candidates            (filter #(= (count %) max-size) sub-seqs-increasing)
        big-enough-candidates (filter #(>= (count %) 2) candidates)]

    (vec (first big-enough-candidates))))
    ;big-enough-candidates
    ;sub-seqs-increasing
    ;max-size
    ;candidates

(fn largest-subseq [input]
  (let
    [is-increasing (fn[subseq]
                    (reduce #(and % %2) (map < subseq (rest subseq))))
     size (count input)]

    (loop [subsize size
           offset  0]
         (if (= 1 subsize)
             []
             (let [end-drop (- size subsize offset)
                   subseq (drop offset (drop-last end-drop input))]
                (if (is-increasing subseq)
                    subseq
                    (if (zero? end-drop)
                        (recur (dec subsize) 0)
                        (recur subsize (inc offset)))))))))
(let [longer (fn [[xs xn :as x] [ys yn :as y]]
               (if (> yn xn) y x))
      append (fn [[xs n] x]
               [(concat xs [x]) (max 2 (inc n))])
      f (fn [[y lng cur] x]
          (if (> x y)
            [x lng (append cur x)]
            [x (longer lng cur) [[x] 0]]))]
  (fn [[x & xs]]
    (let [[_ x y] (reduce f [x [[] 0] [[x] 0]] xs)]
      (first (longer x y)))))
(fn [x]
 (let [ss (map (comp distinct flatten)
           (partition-by (fn [[a b]] (<= b a)) (partition 2 1 x)))]
    (concat (first (sort-by #(count %) >
                    (filter (fn [t] (pos? (reduce #(- %2 %) t))) ss)))
      [])))
(fn [coll]
  (->> (partition 2 1 coll)
    (partition-by #(- (second %) (first %)))
    (filter #(= 1 (- (second (first %)) (ffirst %))))
    (reduce #(if (< (count %1) (count %2)) %2 %1) [])
    flatten
    distinct))
(fn [l]
  (reduce
    (fn [r, l]
      (if (>= (count r) (count l)) r l))

    []
    (remove #(= 1 (count %))
      (reduce
        (fn [ac, v]
          (if
            (<= v (last (last ac)))
            (conj ac (vector v))
            (conj (vec (butlast ac)) (conj (last ac) v))))

        (vector (vector (first l)))
        (rest l)))))

(fn [x]
    (loop [c x
           r []
           s []]
          (let [[p d] c]
               (if (empty? c)
                  (reduce #(if (> (count %) (count %2)) % %2 ) r)
                 (if (= d (inc p))
                   (recur (rest c) r (if (empty? s) (conj s p d)(conj s d)))
                   (recur (rest c) (conj r s) []))))))



(partial (fn [c l r]
             (if (empty? r)
               (if (> (count l) 1) l [])
               (let [n (first r)
                     s (when-not (empty? c) (> n (last c)))
                     nc (if s (concat c [n]) [n])
                     nl (max-key count nc l)]

                 (recur nc nl (rest r))))) [] [])
(fn [a] (let [b (filter #(= (inc (first (first %))) (second (first %))) (partition-by #(= (inc (first %)) (second %)) (partition 2 (interleave a (rest a))))) ] (if (empty? b) [] (#(cons (first (first %)) (map second %))  (apply max-key count (reverse b))))))
(fn [a-vec]
  (let [result-vec (reduce
                    (fn [max a] (if (> (count max) (count a)) max a))
                    (reduce (fn [result data]
                              (if (= (dec data) (last (first result)))
                                  (conj (rest result)
                                        (conj (first result) data))
                                  (conj result [data])))
                            '() a-vec))]
    (if (= 1 (count result-vec)) [] result-vec)))
(fn [a]
  (vec (first (sort-by count > (remove #(< (count %) 2)
                                  (loop [a (seq a) b [] c []]
                                    (if a
                                      (let [d (first a)]
                                        (if (= (dec d) (peek b))
                                          (recur (next a) (conj b d) c)
                                          (recur (next a) [d] (conj c b))))
                                      (conj c b))))))))
(fn [s]
    (loop [rem s max []]
      (if (empty? rem)
        max
        (let [c (take-while (fn [[a b]] (> b a)) (partition 2 1 rem))
              v (subvec (vec rem) 0 (if (zero? (count c)) 0 (inc (count c))))]
          (if (> (count v) (count max))
            (recur (rest rem) v)
            (recur (rest rem) max))))))
(fn [coll]
  (let [increasing-sub-seqs
        (fn increasing-sub-seqs
          [current coll]
          (if (seq coll)
            (let [a (first coll)]
              (if (and current (= (peek current) (dec a)))
                (increasing-sub-seqs (conj current a) (rest coll))
                (if (and current (<= 2 (count current)))
                  (cons current (increasing-sub-seqs [a] (rest coll)))
                  (increasing-sub-seqs [a] (rest coll)))))
            (when (and current (<= 2 (count current)))
              (cons current nil))))]
    (reduce
     (fn [r x]
       (if (> (count x) (count r))
         x r))
     []
     (increasing-sub-seqs nil coll))))
(comp first reduce) (fn [[x y] n] (let [z (if (= (last y) (dec n)) (conj y n) [n])]
                                   (if (< (max 1 (count x)) (count z)) [z z]
                                          [x z]))) [[] []]
(fn [v]
  (let [[maybe n cur _]
        (let [v1 (first v)]
          (reduce
             (fn
              [[longest maxlen current prv] elt]
              (if (>= prv elt)
                  (if (> (count current) maxlen)
                      [current (count current) [elt] elt]
                      [longest maxlen [elt] elt])
                  [longest maxlen (conj current elt) elt]))
             [[v1] 1 [v1] v1]
             (rest v)))]
      (cond (< n (count cur)) cur (= 1 n)[]
       :else maybe)))

(fn [coll]
  (let [zipped (map list coll (concat (drop 1 coll) (list -1)))]
    (loop [best    nil
           left    zipped]
      (if (empty? left)
        (if (> (count best) 0)
          (if (apply < (last best))
            (concat (map first best) (list (second (last best))))
            (map first best))
          (list))
        (let [[best1 left]  (split-with
                             #(apply < %) (second (split-with
                                                   #(apply >= %) left)))
              thebest       (if (> (count best1) (count best)) best1 best)]
          (recur thebest left))))))
(fn [s]
    (reduce (fn [v1 v2]
              (let [s (distinct (flatten v2))]
                (if (> (count s) (count v1))
                  s
                  v1))) []
         (filter (fn [[s]](= 1 (- (second s) (first s))))
            (partition-by (fn [[f l]] (- f l))
                          (map vector s (rest s))))))
(fn [xs]
  (let [
        zip (fn zip [xs ys]
             (lazy-seq
               (when-first [x xs]
                 (when-first [y ys]
                   (cons [x y] (zip (rest xs) (rest ys)))))))
        fst (fn [pair] (nth pair 0))
        snd (fn [pair] (nth pair 1))
        pairwise-partition-by (fn [f xs]
                               (let [f* (partial apply f)]
                                (when (seq xs)
                                  ((fn impl [pairs]
                                    (when (seq pairs)
                                      (let [[this-block the-rest] (split-with f* (rest pairs))]
                                        (lazy-seq
                                          (cons (cons (snd (first pairs)) (map snd this-block))
                                                (impl the-rest))))))
                                   (cons [nil, (first xs)] (zip xs (rest xs)))))))
        longest (fn [acc xs]
                 (if (< (count acc) (count xs)) xs acc))]

   (->> xs
     (pairwise-partition-by <)
     (filter #(< 1 (count %)))
     (reduce longest '()))))
#(let [ret
             (last (sort-by count
                    (reduce
                      (fn [col x]
                       (cond
                         (empty? col)
                         (conj col [x])
                         (= x (-> col first last inc))
                         (conj (rest col) (conj (first col) x))
                         :default
                         (conj col [x])))
                      () %)))]
      (if (= 1 (count ret)) [] ret))

(fn [coll]
  (loop [longest [] current [(first coll)] rst (rest coll)]
    (if (empty? rst)
        (let [longest (if (> (count current) (count longest))
                          current
                          longest)]
          (if (> (count longest) 1) (reverse longest) []))
        (if (= (first rst) (inc (first current)))
            (recur longest (cons (first rst) current) (rest rst))
            (recur (if (> (count current) (count longest))
                       current
                       longest)
                   [(first rst)]
                   (rest rst))))))
(fn [x]
    (let [add-iff-grt (fn [yy xx]
                       (if (or
                            (empty? (first yy))
                            (< (last (first yy)) xx))
                         (conj (next yy) (conj (first yy) xx))
                         (conj yy [xx])))]

      (reduce #(if (and (> (count %2) 1) (>= (count %2) (count %1))) %2 %1)
              [] (reduce #(add-iff-grt %1 %2) [[]] x))))

#(let [result (reverse (filter (fn [t] (> (count t) 1))
                        ((fn [l] (loop [prev -1, curr (first l), in l, out [], part []]
                                  (if (empty? in)
                                    (conj out part)
                                    (if (<= curr prev)
                                      (recur curr (fnext in) (next in) (conj out part) [curr])
                                      (recur curr (fnext in) (next in) out (conj part curr)))))) %)))]
  (if (empty? result)
    []
    (apply max-key count result)))
(fn [s]
 (letfn [(inc-subs [s]
                   (if (or (empty? s)
                           (= (count s) 1))
                     s
                     (let [[a b & r] s]
                       (if (< a b)
                         (cons a (inc-subs (cons b r)))
                         [a]))))]
   (reduce
     (fn [max i]
       (let [c (inc-subs (drop i s))
             cc (count c)]
         (if (and (>= cc 2)
                  (< (count max)
                     (count c)))
           c
           max)))
     []
     (range (count s)))))
(fn [coll]
  (let [
        find-subseqs (fn [acc item]
                      (let [
                            current-seq (last acc)
                            current-index (- (count acc) 1)
                            last-val (last current-seq)]
                          (cond
                            (empty? current-seq) (assoc acc current-index [item])
                              (= item (+ 1 last-val)) (assoc acc current-index (conj current-seq item))
                              :else (conj acc [item]))))
        all-seqs (reduce find-subseqs [[]] coll)
        max-seq  (last (sort-by #(count %) all-seqs))
        result-seq (if (= (count max-seq) 1) [] max-seq)]
    result-seq))
(fn [w] (let [n (count w)]
         (loop [a 0 i 1 ba 0 bn 1]
           (if (< i n)
            (let [i2 (inc i) bn2 (- i2 a)]
             (if (> (w i) (w (dec i)))
               (if (> bn2 bn)
                (recur a i2 a bn2)
                (recur a i2 ba bn))
               (recur i i2 ba bn)))
            (if (>= bn 2) (subvec w ba (+ ba bn)) [])))))
(fn long-incr-subseq [c]
  (loop [coll c best [] cur []]
    (cond (empty? coll) (if (> (count cur) (count best))
                          (if (>= (count cur) 2) cur [])
                          (if (>= (count best) 2) best []))
          (empty? cur) (recur (rest coll) best (conj cur (first coll)))
          (>= (last cur) (first coll))
          (if (> (count cur) (count best))
            (recur (rest coll) cur [(first coll)])
            (recur (rest coll) best [(first coll)]))
          :else
          (recur (rest coll) best (conj cur (first coll))))))
(fn [x]
  (let [f #(take-while seq (iterate % %2))]
    (->> (f next x)
         (mapcat #(f butlast %))
         (filter #(and (< 1 (count %)) (apply < %)))
         reverse
         (sort-by count)
         last
         vec)))
#(let [r
       (reduce
        (fn [c d] (if (> (count c) (count d)) c d))
        (reduce
          (fn [s e]
            (if (= e (inc (last (last s))))
              (assoc s (dec (count s)) (conj (last s) e))
              (conj s (vector e))))

          [[-1]] %))]
  (if (= 1 (count r)) [] r))
(fn [col]
    (let [s>f?  #(> (second %) (first %))
          s     (filter #(s>f? (first %))
                        (partition-by s>f? (partition 2 1 col)))]
      (if (empty? s) [] (let [ls (-> (group-by count s) sort last val first)]
                          (concat [(ffirst ls)] (map second ls))))))
(fn [col]
  (loop [r [] c col]
    (if (empty? c)
      (if (= 1 (count r)) [] r)
      (let [ls ((fn [s]
                  (loop [res [(first s)] rem (rest s)]
                    (if (= (first rem) (inc (last res)))
                      (recur (conj res (first rem))
                             (rest rem))
                      res))) c)]
        (recur (if (> (count ls) (count r)) ls r)
               (rest c))))))
(fn [ns]
  (letfn [(r [[x & m]]
            (if (and m (< x (first m)))
              (cons x (r m))
              (if x [x] [])))
          (rs [s] (if s (conj (rs (next s)) (r s)) []))]
    (apply max-key count [] (remove #(< (count %) 2) (rs ns)))))
(fn [l]
   (let [ p (partition 2 1 l)
          s (fn[[a b]] (= (inc a) b))
          a (partition-by s p)
          i (filter #(s (first %)) a)
          o (sort-by #(- 0 (count %)) i)]
     (distinct (flatten (first o)))))
(fn inc-subs [xs]
  (let [rests (fn rests
                ([xs] (rests xs []))
                ([xs ac]
                 (if (empty? xs)
                   ac
                   (recur (rest xs) (conj ac xs)))))
        take-inc (fn take-inc [xs]
                   (loop [ys (rest xs)
                          ac [(first xs)]]
                     (if (empty? ys)
                       ac
                       (if (< (peek ac) (first ys))
                         (recur (rest ys) (conj ac (first ys)))
                         ac))))
        rests (rests xs)
        incs (map take-inc rests)
        incs (filter #(>= (count %) 2) incs)]
    (if (empty? incs)
      []
      (let [max-inc (apply max (map count incs))
            maxs (filter #(= (count %) max-inc) incs)]
        (first maxs)))))
(fn [s]
   (first (reduce #(let [[m ml c cl ct] %]
                     (if (or (= ct :d) (> %2 ct))
                       (if (and (>= cl ml) (> cl 0))
                         [(conj c %2) (+ cl 1) (conj c %2) (+ cl 1) %2]
                         [m ml (conj c %2) (+ cl 1) %2])
                       [m ml [%2] 1 %2]))
                  [[] 0 [] 0 :d]
                  s)))
(fn [coll]
  (let [sb (first (reduce
                   (fn [[r1 c1] e]
                     (let [c2 (if (< (last c1) e) (conj c1 e) [e])
                           r2 (if (< (count r1) (count c2)) c2 r1)]
                       [r2 c2]))
                   [[][(first coll)]]
                   (rest coll)))]
    (if (< 1 (count sb)) sb [])))
(fn [coll]
  (let [coll (vec coll)
          len (count coll)
          is-valid-next? (fn [idx] (= (inc (coll (dec idx))) (coll idx)))
          latter-if-better (fn [best curr] (if (> (count curr) (count best)) curr best))
          at-least-two (fn [xs] (if (>= (count xs) 2) xs []))]
      (loop [start-idx 0
             idx 1
             curr-seq [(coll 0)]
             best-seq []]
        (cond
         (= idx len)
         (at-least-two (latter-if-better best-seq curr-seq))

         (is-valid-next? idx)
         (recur start-idx (inc idx) (conj curr-seq (coll idx)) best-seq)

         :else
         (let [start-idx* (inc start-idx)]
           (recur start-idx* (inc start-idx*) [(coll start-idx*)] (latter-if-better best-seq curr-seq)))))))
(fn longest-inc-seq [s]
   (letfn [(increasing? [s] (reduce (fn [x y] (if (and x (< x y)) y false)) -1 s))]
     (let [seqs (reverse (filter increasing? (mapcat #(partition % 1 s) (range 2 (inc (count s))))))]
       (if (empty? seqs)
         []
         (apply max-key count seqs)))))
(fn [s]
    (loop [guess [(first s)]
           ret guess
           xs (next s)]
      (if xs
        (if (= (inc (peek guess)) (first xs))
          (recur (conj guess (first xs)) ret (next xs))
          (if (>= (count ret) (count guess))
            (recur [(first xs)] ret (next xs))
            (recur [(first xs)] guess (next xs))))
        (if (>= (count ret) 2)
          (if (>= (count ret) (count guess))
            ret
            guess)
          []))))
(fn [coll]
 (let [ordered? #(apply < %)
       pairs (partition-by ordered?
              (rest (partition-all 2
                     (interleave (cons 0 coll) coll))))
       filtered (filter #(ordered? (first %)) pairs)
       max-size (count (last (sort-by count filtered)))
       goal (some #(if (= (count %) max-size) %) filtered)]
    (if (pos? max-size)
     (cons (ffirst goal) (map last goal)) [])))
(fn [sq]
  (reverse
    ((fn lss [l c sq]
       (if (empty? sq)
         l
         (if (and (number? (first c)) (> (first sq) (first c)))
           (let [cn (cons (first sq) c)]
             (lss (if (> (count cn) (max (count l) 1))
                    cn l)
                  cn (rest sq)))
           (lss l (list (first sq)) (rest sq)))))
     () () sq)))
(fn [orig]
  (first (reverse (sort (map (fn [coll]
                              (vec (first
                                    (filter #(and (next %)
                                                  (= % (sort %))
                                                  (= (count %) (count (set %))))
                                      (reverse
                                        (map #(first (partition % coll))
                                              (range 1 (inc (count coll)))))))))
                         (drop-last
                           (map #(drop % orig) (range 0 (count orig)))))))))
(fn longest [l-orig]
  (loop [best [], cur [], l l-orig, p nil]
    (if (empty? l)
      best
      (let [
            new-p (first l)
            new-l (rest l)
            new-cur (cond
                     (= p nil) []
                     (and (empty? cur) (< p new-p)) [p new-p]
                     (< p new-p) (conj cur new-p)
                     :else [])
            new-best (if (< (count best) (count new-cur)) new-cur best)]

       (recur new-best new-cur new-l new-p)))))
(fn [c] (reduce #(if (and (< (count %1) (count %2)) (> (count %2) 1))
                  %2
                  %1) [] (loop [c c r [] ir [] l 0]
                             (if (seq c)
                               (if (= (first c) (inc l))
                                 (recur (rest c) r (conj ir (first c)) (first c))
                                 (recur (rest c) (conj r ir) [(first c)] (first c)))
                               (conj r ir)))))
(fn [s]
  (let [runs (for [t (take-while seq (iterate next s))
                   h (reductions conj [] t)
                   :when (and (next h)
                              (apply < h))]
                h)]
       (apply max-key count [] (reverse runs))))
(fn [ns]
  (let [
        reds
        (reductions
              (fn [coll it]
                  (if (or (empty? coll) (> it (last coll)))
                      (conj coll it)
                      [it]))
              []
              ns)
        n (apply max (map count reds))
        ans (first (filter #(= n (count %)) reds))]
      (if (> (count ans) 1) ans [])))
(fn [seq]
  (loop [i 1 ut [] pres [(first seq)]]
    (if (== i (count seq))
      (if (> (or (count ut) (count pres)) 1)
        (if (> (count pres) (count ut))
          pres
          ut)
       [])
      (let [el (nth seq i)]
        (if (> el (last pres))
          (recur (inc i) ut (conj pres el))
          (recur (inc i) (if (> (count pres) (count ut)) pres ut) [el]))))))
(fn [a] (or (first (filter #(apply < %) (mapcat #(partition % 1 a) (range (count a) 1 -1)))) []))
(fn [v]
  (vec
   (reverse
    (first
     (sort #(> (count %1) (count %2))
           (filter #(< 1 (count %))
                   (reduce (fn[[[c _] :as all] e]
                            (if (= (dec e) c)
                              (cons (cons e (first all)) (rest all))
                              (cons [e] all)))
                           [] v)))))))
(fn [coll]
  (let [inc-seq-fn (fn inc-seqs [xs]
                      (if (empty? xs) '()
                       (let [diff      (map #(> %1 %2) xs (cons (dec (first xs)) xs))
                             len       (count (take-while true? diff))]
                         (cons (take len xs) (inc-seqs (drop len xs))))))
        inc-seqs (inc-seq-fn coll)
        max-len (apply max (map count inc-seqs))]
    (if (< max-len 2) '()
        (some #(when (= (count %) max-len) %) inc-seqs))))
(fn [r c x]
  (->>
    (for [j (r (c x))
          k (r (+ 1 j) (+ 1 (c x)))
          :let [s (subvec x j k)]
          :when (apply < s)]
      (if (> (c s) 1)
          s []))
    reverse
    (sort-by c)
    last))
range count
#(or
  (first
    (sort-by
      (comp - count)
      (for [f [(fn [[a b]] (= b (+ 1 a)))]
            p (partition-by f (map list % (next %)))
            r [`[~@(map first p) ~(last (last p))]]
            :when (f r)]
        r)))
  [])
(fn [coll]
    (let [divide-with ; split coll between a and b when (f a b)
          (fn [f coll]
            (reduce (fn [acc x]
                      (if (or (empty? (last acc)) (f (last (last acc)) x))
                        (conj acc [x])
                        (conj (pop acc) (conj (last acc) x))))
                    [] coll))
          longest ; yield the longest coll in colls
          (fn [colls]
            (reduce (fn [ans coll] (if (> (count coll) (count ans)) coll ans)) colls))
          longest-seq (longest (divide-with >= coll))]
      (if (> (count longest-seq) 1) longest-seq [])))
(fn longest-increasing-subseq
  [coll]
  (loop [best []
         current []
         coll (partition-all 2 1 coll)]
    (if-let [s (seq coll)]
      (let [[x & xs] s
            new-current (conj current (first x))
            new-best (if (>= (count best) (count new-current)) best new-current)]
        (if (apply < x)
          (recur new-best new-current xs)
          (recur new-best [] xs)))
      (if (<= 2 (count best)) best []))))
(fn maxsub
  [s]
  (loop [
         tails   s
         max-val -1
         cur-len -1
         max-len -1
         max-seq []
         tmp-seq []]
   (let [val (first tails) rem (rest tails)]
     (if (empty? tails)
       (let [max (if (> (count tmp-seq) (count max-seq)) tmp-seq max-seq)] (if (> (count max) 1) max []))
       (if (> val max-val)
         (if (= cur-len max-len)
           (recur rem val (inc cur-len) (inc cur-len) (conj tmp-seq val) (conj tmp-seq val))
           (recur rem val (inc cur-len) max-len max-seq (conj tmp-seq val)))
         (recur rem -1 -1 max-len max-seq (conj [] val)))))))





(fn [coll]
  (->> (partition 2 1 coll)
    (partition-by #(- (second %) (first %)))
    (filter #(= 1 (- (second (first %)) (ffirst %))))
    (reduce #(if (< (count %1) (count %2)) %2 %1) [])
    flatten
    distinct))
(fn [s]
  (let
      [subvecs (for [x (range (inc (count s)))
                     y (reverse (range (inc x) (inc (count s))))]
                 (subvec s x y))
       increasing-subvecs (filter #(and (apply < %) (> (count %) 1)) subvecs)
       max-count (apply max 2 (map count increasing-subvecs))
       biggest-increasing-subvecs (filter (comp (partial = max-count)
                                                count)
                                          increasing-subvecs)]
    (if (empty? biggest-increasing-subvecs) [] (first biggest-increasing-subvecs))))
#(->> %3
      (reductions % nil)
      (sort-by count)
      last
      %2)

#(if (and % (= 1 (- %2 (peek %)))) (conj % %2) [%2])

#(if (second %) % [])
(fn [s]
  (letfn [(subseqs [s] (set (mapcat (fn [i] (map #(subvec s i %)
                                                (range i (inc (count s)))))
                                    (range (count s)))))
          (inc-subseqs [s] (filter #(and (> (count %) 1) (apply < %))
                                   (subseqs s)))
          (maxlength [s] (apply max (map count s)))
          (longest [s] (first (filter #(= (count %) (maxlength s)) s)))]
    (let [ss (inc-subseqs s)] (if (empty? ss) [] (longest ss)))))
(fn [s]
  (loop [nums s longest-so-far [] longest []]
    (if (empty? nums)
      longest
      (let [[curr & rest] nums]
        (if (or (empty? longest-so-far)
                (not (= curr (inc (last longest-so-far)))))
          (recur rest [curr] longest)
          (if (= curr (inc (last longest-so-far)))
            (let [new-longest-so-far (conj longest-so-far curr)
                  new-longest-len (count new-longest-so-far)]
              (if (and (> new-longest-len (count longest))
                       (> new-longest-len 1))
                (recur rest new-longest-so-far new-longest-so-far)
                (recur rest new-longest-so-far longest)))))))))
(fn [x]
  (loop [best-idx 0
         best-len 0
         current-idx 0
         current-len 0]
    (cond
     ;; ate the whole thing, return our best
     (= current-idx (dec (count x)))
     (if (> best-len 0) (subvec x best-idx (inc (+ best-idx best-len))) [])

     (= (+ current-idx current-len) (dec (count x)))
     (recur best-idx best-len (inc current-idx) 0)

     ;; we're increasing at current-idx
     (> (nth x (+ current-idx (inc current-len))) (nth x (+ current-idx current-len)))
     (if (> (inc current-len) best-len)
       (recur current-idx (inc current-len) current-idx (inc current-len))
       (recur best-idx best-len current-idx (inc current-len)))

     :else
     (recur best-idx best-len (inc current-idx) 0))))

(fn [seq]
  (apply vector (first
                 (sort
                   #(> (count %1) (count %2))
                   (filter
                     (fn [s]
                       (if (< (count s) 2)
                         true
                         (if (= (inc (first s)) (second s))
                           (recur (rest s))
                           false)))
                     (for
                       [i (range 2 (inc (count seq)))
                        x (partition i 1 seq)] x))))))
#(first (reduce
         (fn [acc x]
           (let
             [ns
               (if (or (nil? (last (last acc))) (> x (last (last acc))))
                 (conj (last acc) x)
                 [x])]


             (if (and (> (count ns) (count (first acc))) (> (count ns) 1))
               [ns ns]
               [(first acc) ns])))



         [[] []]
         %))

(fn [s]
  (let [countdown (range (count s) 0 -1)
        pairs (partition 2 (interleave countdown s))
        grouped-pairs (partition-by #(reduce + %) pairs)
        ordered-pairs (sort-by #(- (count %)) grouped-pairs)
        longest-seq (map second (first ordered-pairs))]
    (if (> (count longest-seq) 1) longest-seq [])))
#((reduce (fn [[p r c] x] (if (and x p (> x p)) [x r (conj c x)] [x (if (and (> (count c) 1) (> (count c) (count r))) c r) [x]]) ) [nil [] []] (conj % nil)) 1)
(fn [x]
  (map second
   (first(sort-by count >
          (filter #(> (count %) 1)
            (sort-by count > (partition-by first
                              (map #(list (- %2 %)%2)(range) x))))))))
(fn [c]
  ((fn [best-inc curr-inc c]
      (if (empty? c)
          best-inc
        (let [curr-inc (if (or (empty? curr-inc) (> (first c) (last curr-inc)))
                           (conj curr-inc (first c))
                          [(first c)])
              best-inc (if (and (> (count curr-inc) 1) (> (count curr-inc) (count best-inc)))
                           curr-inc
                          best-inc)]
          (recur best-inc curr-inc (rest c)))))
   [] [] c))
(fn [coll]
  (or (some
       (fn [n]
         (some (fn [m]
                 (let [v (subvec coll m (+ m n))]
                   (if (and (> (count v) 1) (apply < v))
                     v)))
               (range (- (inc (count coll)) n))))
       (range (count coll) 1 -1)) []))
(fn increasing-subseq
  [s]
  (letfn [(insert-between [p v l]
              (if (empty? (rest l))
               l
               (let [[a b] l]
                (lazy-seq (concat (if (p a b) [a v] [a])
                                  (insert-between p v (rest l)))))))]
     (let [separated-list (insert-between #(<= %2 %1) :mark s)
             sub-seqs (filter #(not= [:mark] %) (partition-by number? separated-list))]
      (if (= (count s) (inc (count (filter #(= :mark %) separated-list))))
          []
          (first (sort-by #(/ 1 (count %)) sub-seqs))))))
(fn [seq]
    (or (first (filter #(= % (distinct (sort %)))
                        (mapcat #(partition % 1 seq)
                                 (reverse (range 2 (inc (count seq))))))) []))
(fn [coll]
    (loop [coll coll
           prev nil
           answer []
           running []]
      (cond
       (nil? coll) (let [v (if (> (count answer) (count running)) answer running)]
                     (if (> (count v) 1) v []))
       (or (nil? prev) (= (inc prev) (first coll))) (recur (next coll) (first coll) answer (conj running (first coll)))
       (> (count running) (count answer)) (recur (next coll) (first coll) running [(first coll)])
       :else (recur (next coll) (first coll) answer running))))
(fn [col]
  (let [[a b] (reduce
                (fn [[a b] x](if (empty? b) [a [x]]
                               (let [y (peek b)]
                                 (if (> x y) [a (conj b x)]
                                   (if (< (count a) (count b))
                                     [b [x]]
                                     [a [x]])))))
                [[][]] col)]
    (if (< (count a) (count b)) b
     (if (= 1 (count a)) [] a))))
(fn longest [s]
  (loop [l (rest s) acc [(first s)] res [(first s)]]
    (if (empty? l)
      (if (> (count res) 1) res [])
      (let [
            fi (first l)
            la (last acc)
            newacc (if (> fi la) (conj acc fi) [fi])]
        (recur
          (rest l)
          newacc
          (if (> (count newacc) (count res))
            newacc
            res))))))
(fn [[x & xs]]
  (loop [src xs, acc [x], res []]
    (if (empty? src)
      (let [c (conj res acc)
            mc (apply max (map count c))]
        (if (< mc 2) [] (first (filter #(= mc (count %)) c))))
      (let [v (first src)]
        (if (= v (inc (peek acc)))
          (recur (rest src) (conj acc v) res)
          (recur (rest src) [v] (conj res acc)))))))
(fn liss [s]
  (letfn [(iss [s]
           (loop [rv [(first s)] tmp (rest s)]
             (if (= (first tmp) (inc (last rv)))
               (recur (conj rv (first tmp)) (rest tmp))
               rv)))]
    (let [ss (map iss (take (count s) (iterate rest s)))
          m (apply max (map count ss))
          l (first (filter #(= (count %) m) ss))]
      (if (> (count l) 1)
        l
        []))))
(fn [v]
  (let [r
        (map #(% 1)
          (first
            (sort-by count >
              (partition-by #(+ (% 0) (% 1))
                (keep-indexed #(vector (- (count v) %) %2) v)))))]
    (if (> (count r) 1)
      r
      [])))
(fn [s]
  (let [
        subseqs (filter
                 #(not= 1 (count %))
                 (mapcat
                   (partial reductions conj [])
                   (tree-seq
                     (complement empty?)
                     (comp list rest) s)))
        inc? (fn [s] (or (empty? s) (= s (range (first s) (inc (last s))))))]
   (last (sort-by count (filter inc? subseqs)))))
(fn [coll]
  (loop [c (next coll) ret (list (vector (first coll)))]
   (if c
     (recur (next c) (if (= (-> ret first last inc) (first c))
                      (cons (conj (first ret) (first c)) (rest ret))
                      (cons (vector (first c)) ret)))
    (let [r (last (sort-by count ret))]
     (if (< 1 (count r))
      r
      [])))))
(fn [s]
  (vec (apply max-key count
        (reverse (map
                  (fn [s]
                      (into (sorted-set) (flatten
                                          (take-while (fn [[a b]] (= (inc a) b))
                                                      (partition 2 1 s)))))
                  (take (count s) (iterate rest s)))))))
(fn [x]
  (loop [data x cur-data [] max-data []]
    (if-not (first data)
      (if (= 1 (count max-data))
       '()
       (reverse max-data))
      (if (= (inc (first data)) (fnext data))
       (recur (rest data) (cons (first data) cur-data) max-data)
       (if (>= (count cur-data) (count max-data))
          (recur (rest data) '() (cons (first data) cur-data))
          (recur (rest data) '() max-data))))))
(fn lng[c]
  (loop [acc [[]] l c]
    (if (empty? l)
      (if (= 1 (count(last(sort-by count acc))))
        []
        (last(sort-by count acc)))
      (recur (if (= (last(last acc)) (- (first l) 1))
               (conj acc (concat (last acc) [(first l)]))
               (conj acc [(first l)])) (rest l)))))
(fn [coll]
  (reduce
    (fn [x y] (let [y-count (count y)] (if (or (< y-count 2) (> (count x) y-count)) x y)))
    []
    (reduce
      (fn [subseqs item]
        (let [first-vec (first subseqs)]
          (if-let [last-item (last first-vec)]
            (if (= (inc last-item) item)
              (cons (conj first-vec item) (rest subseqs))
              (cons [item] subseqs))
            [[item]])))
      nil
      coll)))
(fn [v]
  (letfn [(will-fit? [v n-r]
            (or (= 0 (count v))
                (= (inc (last v)) n-r)))
          (double-or-nothing [coll]
            (if (< (count coll) 2)
              '() coll))
          (longer-of [coll1 coll2]
            (if (< (count coll1) (count coll2)) coll2 coll1))]
    (first
      (reduce
        (fn [[record-holder current-contender] new-recruit]
          (let [c-c (if (will-fit? current-contender new-recruit)
                      (conj current-contender new-recruit)
                      [new-recruit])]
            [(double-or-nothing (longer-of record-holder c-c)) c-c])) [[][]] v))))
(fn lsubseq
  ([s] (lsubseq s '() '()))
  ([s longest current]
   (let [fs (list (first s))]
    (if (empty? s) longest
     (if (empty? current) (lsubseq (rest s) longest fs)
      (if (> (first s) (last current))
        (if (>= (count current) (count longest))
          (lsubseq (rest s) (concat current fs) (concat current fs))
          (lsubseq (rest s) longest (concat current fs)))
       (lsubseq (rest s) longest fs)))))))
(partial
  (let [longest (fn longest [c]
                 (if (not (seq c)) []
                  (let [l (longest (next c))]
                   (if (>= (count (first c)) (count l))
                     (first c)
                     l))))]
   (fn [results tmp c]
     (if (not (seq c))
      (let [l (longest (conj results tmp))]
        (if (= (count l) 1) [] l))
      (if (or (empty? tmp)
            (< (last tmp) (first c)))
        (recur results (conj tmp (first c))
          (next c))
        (recur (conj results tmp)
          [(first c)] (rest c))))))
 [] [])
(letfn [(s [l] (let [c (count l)] (for [a (range (- c 2)) n (range 2 (+ 1 c))] (take n (drop a l)))))
        (i [l] (every? #{1} (map - (rest l) (butlast l))))]
  #(if-let [c (seq (filter i (s %)))]
    (apply max-key count c)
    []))
(fn [c]
    (loop [best []
           current []
           i 0]
      (if (= i (count c))
        (if (> (count best) 1) best [])
        (if (or (empty? current) (= (last current) (dec (nth c i))))
          (let [next (conj current (nth c i))
                best (if (> (count next) (count best)) next best)]
            (recur best next (inc i)))
          (recur best [(nth c i)] (inc i))))))
; Man, this is not my best work quite frankly.
#(loop [i (count %)]
   (if-let [a (first (filter (partial apply <) (partition i 1 %)))]
     a
     (if (= i 2)
       []
       (recur (dec i)))))
(fn ! [s]
  (let [a (map-indexed #(hash-map %1 %2) s)
        b (group-by #(- (first (keys %)) (first (vals %))) a)
        c (sort-by #(count (fnext %)) b)
        d (fnext (last c))
        e (reverse (reduce into (map vals d)))]
       (if (= (count e) 1) [] e)))
(fn longest-increasing-subseq [coll]
  (letfn [
          (longest [colls]
            (if (empty? colls)
              []
              (last (sort-by count (reverse colls)))))

          (increasing-subseqs [coll]
            (remove empty?
                    (map (partial remove coll?)
                         (partition-by
                           #(and (coll? %) (apply >= %))
                           (interleave coll
                                       (partition 2 1 [Integer/MAX_VALUE] coll))))))]

    (longest
      (remove #(< (count %) 2)
               (increasing-subseqs coll)))))
(fn [xs]
  (let [ms (last (sort-by count (rseq (reduce #(let [s (peek %)] (if (> %2 (peek s)) (conj (pop %) (conj s %2)) (conj % (vector %2)))) [[(first xs)]] (rest xs)))))]
    (if (> (count ms) 1) ms [])))
(fn [s]
   (letfn [(incr-subseq [xs]
              (loop [r (rest xs) acc [(first xs)]] ; fails on empty seq
                (cond
                  (empty? r) acc
                  (<= (first r) (last acc)) acc ;decreased
                  :else (recur (rest r) (conj acc (first r))))))
           (incr-subseq-at [i xs] (incr-subseq (drop i xs)))
           (better? [xs ys] (and (> (count xs) 1) (> (count xs) (count ys))))]

     (reduce
       (fn [curr poss] (if (better? poss curr) poss curr))
       []
       (map incr-subseq-at (range (count s)) (repeat s)))))
(fn liss [ls cs s]
  (if (empty? s)
    (if (and (< (count cs) 2) (< (count ls) 2))
      []
      (if (> (count cs) (count ls)) cs ls))
    (if (= (last cs) (dec (first s)))
      (liss ls (conj cs (first s)) (rest s))
      (liss (if (> (count cs) (count ls)) cs ls) (vector (first s)) (rest s)))))
[] []
(fn [coll]
  (distinct
    (flatten
      (reduce #(if (> (count %1) (count %2)) %1 %2)
        (for [n (range (count coll))]
          (take-while #(= (first %) (dec (second %)))
            (drop n (map vector coll (drop 1 coll)))))))))
(fn [col]
  (let [validate_col (fn [c] (if (< (count c) 2) [] c))]
   (loop [s1 [] s2 [(first col)] lastv (first col) data (rest col)]
     (if (empty? data)
         (if (< (count s1) (count s2))
             (validate_col s2)
             (validate_col s1))
         (let [is-inc (< lastv (first data))]
           (recur (if is-inc
                      s1
                      (if (< (count s1) (count s2))
                          s2
                          s1))
                  (if is-inc (conj s2 (first data) ) [(first data)])
                  (first data)
                  (rest data)))))))
(fn [ns]
  (letfn [(r [[x & m]]
            (if (and m (< x (first m)))
              (cons x (r m))
              (if x [x] [])))
          (rs [s] (if s (conj (rs (next s)) (r s)) []))]
    (apply max-key count [] (remove #(< (count %) 2) (rs ns)))))
(fn [l]
  (let [sl (reduce (fn [a e]
                    (if (> e (first (first a)))
                      (conj (rest a) (conj (first a) e))
                      (conj a (list e))))
            (list (list (first l)))
            (rest l))
        sm (group-by count sl)
        ln (reverse (last (sm (first (sort > (keys sm))))))]
   (if (> (count ln) 1)
     ln
     '())))
(fn [l]
  (let [ps (partition-by #(- % (nth l %)) (range (count l)))
        lss (apply max-key count ps)]
    (if (> (count lss) 1) (map #(nth l %) lss) [])))
(fn longest-increasing-sub-seq [coll]
  ((fn [longest candidate coll]
     (if (seq coll)
       (let [n (first coll)]
         (if (or (empty? candidate) (> n (first candidate)))
           (recur longest (cons n candidate) (rest coll))
           (if (> (count candidate) (count longest))
             (recur candidate (list n) (rest coll))
             (recur longest (list n) (rest coll)))))
       (let [l (reverse (if (> (count candidate) (count longest))
                         candidate longest))]
         (if (> (count l) 1) l []))))
   '() '() coll))
(fn [array]
  (let [result (apply max-key count (reverse (reduce
                                              (fn [[longest current] elem]
                                                (if (> elem (last current))
                                                  [longest (conj current elem)]
                                                  (if
                                                    (> (count current) (count longest))
                                                    [current [elem]]
                                                    [longest [elem]])))



                                              [[] [(first array)]]
                                              array)))]

   (if (< 1 (count result))
     result
     [])))


(fn f
  ([a] (f a 0 0 0 [] []))
  ([a p c m cr mr]
   (if-let [[a & r] (seq a)]
     (if (> a p)
       (recur r a (inc c) m (conj cr a) mr)
       (if (and (> c m) (> c 1))
         (recur r a 1 c [a] cr)
         (recur r a 1 m [a] mr)))
     (if (and (> c m) (> c 1)) cr mr))))
(fn [x] (vec (first (sort-by #(* -1 (count %)) (map (comp distinct flatten) (filter #(apply < (first %)) (partition-by (partial apply <) (partition 2 1 x))))))))
(fn [[h & t :as s]]
    (->>
      (map #(list (- % %2) %2 %) t s)
      (partition-by (comp pos? first))
      (sort-by (comp - count))
      (filter (comp pos? ffirst))
      first
      (mapcat next)
      distinct))
#(letfn [(lcs [x i]
          (conj (map last (take-while (fn [[a b]] (< a b)) (map vector (drop i x) (drop (inc i) x)))) (x i)))]
  (reduce (fn [x y] (if (and (< 1 (count y)) (< (count x) (count y))) y x)) (list) (map (partial lcs %) (range (count %)))))
(fn [coll]
  (reduce
    #(if (< (count %1) (count %2)) %2 %1)
    []
    (filter #(< 1 (count %))
      (reduce #(let [l (last (last %1))]
                (if (and l (< l %2))
                  (conj %1 (conj (last %1) %2))
                  (conj %1 [%2])))
              [] coll))))
(fn [x]
  (reduce #(if (> (count %2) (max 1 (count %))) %2 %)
    []
    (partition-by #(< % 0)
      (loop [a (first x)
             b (second x)
             c (-> x rest rest)
             v []]
       (if (> (count c) 0)
         (recur (if (if (> a 0) (< a b) (< (- 0 (inc a)) b)) b (- 0 (inc b)))
                (first c)
                (rest c)
                (concat v (if (> a 0) [a] [a (- 0 (inc a))])))
         (concat v (if (or (< a 1) (< a b)) [a b] [a (- 0 (inc b)) b])))))))
(fn longest [s]
  (first
   (reduce (fn [[acc cur] elem]
            (if (seq cur)
                (if (< (last cur) elem)
                    (if (>= (count cur) (count acc))
                        [(conj cur elem) (conj cur elem)]
                        [acc (conj cur elem)])
                    [acc [elem]])
             [acc [elem]]))
    [[] []] s)))
#(letfn [(y [l c m]
          (letfn [(sm [a b] (if (> (count a) (count b)) a b))]
           (if (seq l)
               (let [[h & t] l]
                 (if (> h (last c))
                     (recur t (conj c h) m)
                     (recur t [h] (if (> (count c) 1) (sm c m) m))))
               (if (> (count c) 1) (sm c m) []))))] (y (rest %) [(first %)] []))
#(letfn [(y [l c m]
          (letfn [(sm [a b] (if (> (count a) (count b)) a b))]
           (if (seq l)
               (let [[h & t] l]
                 (if (> h (last c))
                     (recur t (conj c h) m)
                     (recur t [h] (if (> (count c) 1) (sm c m) m))))
               (if (> (count c) 1) (sm c m) []))))] (y (rest %) [(first %)] []))
(fn f [s]
   (loop [[x y] s, [z & t] s, r {}, i 0]
     (if y (if (= (inc x) y)
             (recur t t (assoc r i (conj (r i) x y)) i)
             (recur t t (assoc r i (conj (r i) x)) (inc i)))
       (let [res (seq (set (apply max-key #(count (set %)) (vals r))))] (if (> (count res) 1) res [])))))
#((fn longest-subseq
   ([v] (longest-subseq (rest v) [(first v)] []))
   ([v p b]
    (let [f (first v)
          r (rest v)
          l (last p)
          cp (count p)
          cb (count b)]
      (cond (empty? v)
            (if (> cp cb)
              (if (> cp 1) p [])
              (if (> cb 1) b []))
            (> f l) (longest-subseq r (conj p f) b)
            (> (count p) (count b)) (longest-subseq r [f] p)
            :else (longest-subseq r [f] b))))) %)
(letfn [(get-growing ([s] (get-growing s [] []))
                    ([s r prs] (cond (empty? s) (if (> (count r) 1) (conj prs r) prs)
                                     (empty? r) (recur (rest s) (conj r (first s)) prs)
                                     (> (first s) (last r)) (recur (rest s) (conj r (first s)) prs)
                                     :else (recur s [] (if (> (count r) 1) (conj prs r) prs)))))
        (sel-max [coll]
          (if (not (empty? coll))
            (reduce (fn [a b] (if (> (count b) (count a)) b a)) coll)
            []))]
  #(sel-max (get-growing %1)))
(fn [a] (let[
             b (map vector a (next a))
             c (partition-by #(apply >= %) b)
             g (for [lst c] (concat (map first lst) [(-> lst last last)]))
             h (filter (fn [[f s]] (< f s)) g)]
            (if (seq h) (apply max-key count (reverse h))[])))
#(loop [xs (rest %) current [(first %)] result []]
   (let [new-result (if (and (>= (count current) 2)
                             (> (count current) (count result)))
                        current
                        result)]
     (cond (empty? xs) new-result
           (> (first xs) (last current))
           (recur (rest xs) (conj current (first xs)) result)
           :else
           (recur (rest xs) [(first xs)] new-result))))
(fn [coll]
  (let [increasing? (fn [c] (apply < c))]
    (loop [ret [] current [] c coll]
      (cond
       (and (empty? c) (< (count ret) 2))
       []
       (empty? c)
       ret
       (and (increasing? (conj current (first c))) (< (count ret) (inc (count current))))
       (recur (conj current (first c)) (conj current (first c)) (rest c))
       (increasing? (conj current (first c)))
       (recur ret (conj current (first c)) (rest c))
       :else
       (recur ret [(first c)] (rest c))))))
#(let [C count A (C %) R range] (nth (concat (for [t (R A 1 -1) o (R 0 (- A (dec t))) :let [S (take t (drop o %))] :when (and (= S (sort S)) (= (C S) (C (set S)))) ] S) [[]]) 0))
(fn find-inc-seq
    ([seq] (find-inc-seq [] [] seq))
    ([cur-seq cur-longest-seq a-seq]
     (cond
          (empty? a-seq)
          (let [ret (if (> (count cur-seq) (count cur-longest-seq)) cur-seq cur-longest-seq)]
               (if (> (count ret) 1) ret []))

          (empty? cur-seq) (find-inc-seq (conj [] (first a-seq)) cur-longest-seq (rest a-seq))
          :default (if (> (first a-seq) (last cur-seq))
                    (find-inc-seq
                           (conj cur-seq (first a-seq))
                           cur-longest-seq
                           (rest a-seq))
                    (find-inc-seq
                           []
                           (if (> (count cur-seq) (count cur-longest-seq)) cur-seq cur-longest-seq)
                           a-seq)))))




;; This is pretty grim, but it works.  It'll be interesting to see
;; what comes from the people who actually know what they're doing...
;;
(fn [x]
  (let [y (filter #(> (first %) 1)
            (map #(vector (inc (count %)) (second (first %)))
              (filter #(= true (first (first %)))
                (partition-by #(first %)
                  (map vector (map #(= %1 (dec %2)) x (rest x))
                              (range))))))]
    (if (empty? y)
      []
      (let [ymax (apply max (map first y))
            ypair (first (filter #(= ymax (first %)) y))
            ys (second ypair)]
        (subvec x ys (+ (first ypair) ys))))))
#(reduce
   (fn [biggest-run run]
     (if (and (-> run count dec pos?) (> (count run) (count biggest-run)))
       run
       biggest-run))
   []
   (reduce
      (fn [runs pair]
        (if (and (= (inc (first pair)) (second pair)) (= (last (last runs)) (first pair)))
          (conj (vec (drop-last runs)) (conj (last runs) (second pair)))
          (conj runs (vector (second pair)))))
     (vector (vector (first %)))
     (map list % (rest %))))
(fn f [b c l [h & t]]
  (let [C count
        d (if (< (C b) (C c)) c b)]
    (cond
      h
        (if (> h l)
          (f d (conj c h) h t)
          (f d [h] h t))
      (> (C d) 1)
      d
      1
      [])))
[] [] 9
(fn [v] (let [x (last (sort-by count (filter #(second (first %)) (partition-by #(second %) (map-indexed (fn [a b] [b (= b (inc (nth v a))) a]) (rest v)))))) i (last (first x))] (if x (subvec v i (+ i (inc (count x)))) [])))
(fn [inp]
  (let [push-back (fn[col x] (reverse (cons x (reverse col))))
        addl (fn [ccoll, x]
              (if (= 1 (count ccoll))
                (list (push-back (first ccoll) x))
                (concat (butlast ccoll) (list (push-back (last ccoll) x)))))
        partition-seq (fn [l]
                       (reduce
                            #(if (= (dec %2) (last(last %)))
                                 (addl % %2)
                                 (reverse (cons (list %2) (reverse %))))
                            '(()) l))
        run (apply max-key count (partition-seq inp))]
    (if (= 1 (count run)) [] run)))
(fn max-incr-subseq [col]
  (loop [c (rest col), cur [(first col)], res []]
    (if (seq c)
      (if (= (- (first c) (last cur)) 1)
        (recur (rest c) (conj cur (first c)) res)
        (recur (rest c) [(first c)] (conj res cur)))
      (let [res (first (sort-by (comp - count) (conj res cur)))]
        (if (= (count res) 1) [] res)))))
(fn subseq [coll]
  (->>
    (partition 2 1 coll)
    (partition-by (partial apply <))
    (sort #(> (count %1) (count %2)))
    (filter (or
              (comp (partial apply <) first)
              (comp (partial <= 1) count)))
    first
    (apply concat)
    distinct))

(fn f
  ([in] (f in [(first in)] [(first in)]))
  ([in o i]
   (if (empty? in)
     (let [out (if (> (count o) (count i)) o i)] (if (= (count out) 1) [] out))
     (f
       (rest in)
       (if (> (count o) (count i)) o i)
       (if (= (first in) (inc (last i)))
         (conj i (first in))
         [(first in)])))))
(fn longest-subseq [s]
  (letfn [
          (find-subseq [[h & t :as s]]
            (cond
             (empty? s) '()
             (not= (+ h 1) (first t)) [h]
             :else (cons h (find-subseq t))))]
    (->> (iterate rest s)
         (take-while (comp not empty?))
         (map find-subseq)
         (reduce #(if
                      (and
                        (> (count %2) 1)
                        (> (count %2) (count %1))) %2 %1) []))))
(fn [ns]
    (->> ns
         (iterate rest)
         (take-while seq)
         (map (partial reductions (fn [last-in-streak n]
                                    (when (= last-in-streak (dec n))
                                        n))))
         (map (partial take-while identity))
         (remove #(= (count %) 1))
         (reduce (partial max-key count) [])))
(fn [x] (let [r (reduce (partial max-key count) (reductions #(if (= (last %1) (dec %2)) (conj %1 %2) [%2]) [] x))]
         (if (> (count r) 1) r [])))
(fn [v]
  (reduce #(if (> (count %) (count %2)) % %2) []
          (filter #(> (count %) 1)
                  (reductions #(if (= (+ 1 (last %)) %2)
                                 (conj % %2)
                                 [%2])
                              [(first v)] (rest v)))))
(fn [coll]
  (flatten (take 1
            (filter
              (fn [c] (every? #(< (first %) (last %)) (partition 2 1 c)))
              (mapcat
                #(partition % 1 coll)
                (reverse (range 2 (count coll))))))))
(fn longest-incr-subseq [coll]
  (letfn [(take-increasing [pred coll]
            (lazy-seq
              (when-let [s (seq coll)]
                (when (pred (first s))
                  (cons (first s) (take-increasing #(< (first s) %) (rest s)))))))

          (increasing-parts [coll]
            (lazy-seq
              (when-let [s (seq coll)]
                (let [fst (first s)
                      run (take-increasing identity s)]
                  (cons run (increasing-parts (seq (drop (count run) s))))))))]

   (let [parts (increasing-parts coll)
         max-length (apply max (map count parts))]
     (if (< max-length 2)
       []
       (first (filter #(= (count %) max-length) parts))))))
(fn incr [x]
  (into []
        (first
          (sort #(> (count %1) (count %2))
            (filter #(and (>= (count %) 2) (apply < %))
                    (for [i (-> x count range)
                          j (-> x count inc range)]
                      (take j (drop i x))))))))




#(nth (for [n (range (+ 1 (count %)) 1 -1) p (partition n 1 %) :when (apply < p)] p) 0 [])
(fn [C [s & z]]
  ((fn g [a [h & t]]
    (if h
      (if (< (last a) h)
        (g (conj a h) t)
        (let [q (g [h] t)]
          (if (next a)
            (if (< (C a) (C q)) q a)
            q)))
      []))
   [s] `(~@z 0)))
count
(fn [xs]
  (let [rfn
        (fn [[max curr] x]
          (let [new-curr
                (if (or
                      (empty? curr)
                      (= (last curr) (dec x)))
                  (conj curr x) [x])

                new-max
                (if (> (count new-curr) (count max))
                  new-curr max)]

            [new-max new-curr]))
        ans (first (reduce rfn [[] []] xs))]
    (if (> (count ans) 1) ans [])))
(fn [l]
  (or
   (->> l
        (partition 2 1)
        (partition-by #(< (first %) (second %)))
        (filter #(<= (ffirst %) (second (first %))))
        (map (fn [x] (reduce #(conj % (second %2)) [(ffirst x)] x)))
        (sort #(> (count %) (count %2)))
        first)
   []))
(fn [col]
    (letfn [(part [parts val]
              (let [cur (last parts)]
                (if (> val (last cur))
                  (conj (vec (butlast parts)) (conj cur val))
                  (conj parts [val]))))]
      (let [parts (reduce part [[(first col)]] (rest col))]
        (reduce #(let [sz (count %2)] (if (and (> sz 1) (> sz (count %1))) %2 %1)) [] parts))))
(fn [coll]
  (loop [coll coll, n (count coll)]
    (let [subseqs (filter (partial apply <) (partition n 1 coll))]
      (cond
       (< n 2) ()
       (seq subseqs) (first subseqs)
       :else (recur coll (dec n))))))
(fn longest-increasing-sub
  [coll]
  (let [trth (map-indexed #(if (zero? %1) 0 (- %2 (nth coll (dec %1)))) coll)
        ct (map count (partition-by identity trth))
        mx (reduce max ct)
        ts (fn [ctl] (loop [m 0
                            sm 0]
                      (if (= m (count ctl))
                        sm
                        (if (= mx (nth ctl m))
                         (if (= (first (drop sm trth)) 1)
                           sm
                           (recur (inc m) (+ sm (nth ctl m))))
                         (recur (inc m) (+ sm (nth ctl m)))))))]
    (if (= 0 (ts ct))
      (if (> 2 (count (take  mx (drop (ts ct) coll))))
        []
        (take  mx (drop (ts ct) coll)))
      (if (> 2 (count (take (inc mx) (drop (dec (ts ct)) coll))))
        []
        (take (inc mx) (drop (dec (ts ct)) coll))))))
(fn [arr]
  (loop [resv []  t arr]
     (if (empty? t)
         (if (= (count resv) 1)
          []
          resv)
         (let [r (keep #(if % %) (map #(if (= % %2) %) (range (first t) (+ (count t) (first t)))
                                  t)),
               startidx (count r)]
          (recur (if(< (count r) (count resv))
                  resv
                  r)
                (drop startidx t))))))
(fn
  [xs]
  (->>
    (reduce
      (fn [[p [ys & more :as yss]] x] [x (if (< p x) (cons (cons x ys) more) (cons (list x) yss))])
      [(first xs) '(())]
      xs)
    second (filter #(-> % count (> 1))) (cons []) (apply max-key count) reverse vec))
(fn increasing-sub-seq [xs]
  (cond (empty? xs) '()
        (empty? (rest xs)) '()
        (>= (first xs) (second xs)) (increasing-sub-seq (rest xs))
        true (let [s0 ((fn sub-seq [ys]
                         (cond (empty? ys) '()
                               (empty? (rest ys)) (list (first ys))
                               (>= (first ys) (second ys)) (list (first ys))
                               true (cons (first ys)
                                          (sub-seq (rest ys))))) xs),
                   s1 (increasing-sub-seq (rest xs))]
               (cond (< (count s0) (count s1)) s1
                     true s0))))
(fn [xs]
    (let [xss (take-while seq (iterate rest xs))
          lenfn #(count (take-while pos? (map - (rest %) %)))
          lens-xss (map #(list (lenfn %) %) xss)
           [len xs] (first (sort-by first > lens-xss))]
        (if (zero? len) ()
         (take (inc len) xs))))
(fn [coll]
    (let [loc-longest-match (fn [contender default]
                              (if (and (> (count contender) (count default))
                                       (> (count contender) 1))
                                contender
                                default))]

      (loop [c    coll
             curr []
             best []]
        (cond
         (empty? c) (loc-longest-match curr best)
         (= (last curr) (dec (first c)))  (recur (rest c) (conj curr (first c)) best)
         :else (recur (rest c) [(first c)] (loc-longest-match curr best))))))

(fn [S]
  (if
    (empty? S)
    []
    (loop [s (rest S), cs [(first S)], cl 1, cm (first S), bs [], bl 1]
      (if (empty? s)
          bs
          (if (> (first s) cm)
              (let [cs (conj cs (first s)), cl (inc cl), cm (first s)]
                (if (> cl bl)
                    (recur (rest s) cs cl cm cs cl)
                    (recur (rest s) cs cl cm bs bl)))
              (recur (rest s) [(first s)] 1 (first s) bs bl))))))
(fn [a]
  (->>
    (reduce
      #(if (and (< (first %2) (second %2)) (= (last (last %)) (first %2)))
         (conj (vec (drop-last %)) (conj (last %) (second %2)))
         (conj % (vector (second %2))))
      (-> a first vector vector)
      (partition 2 1 a))
    (map #(if (= (count %) 1) [] %))
    reverse
    (apply max-key count)))
(fn [s]
    (loop [s s c [] l []]
      (if (empty? s)
        (if (> (count l) 1)
          l
          [])
        (if (> (first s) (or (last c) -1))
          (let [c* (conj c (first s))]
            (recur (rest s) c* (if (> (count c*) (count l)) c* l)))
          (recur s [] l)))))
#(loop [b [] c [(first %)] l (first %) r (rest %)]
       (if (seq r)
           (if (< l (first r))
               (recur b (conj c (first r)) (first r) (rest r))
               (recur (if (> (count c) (count b)) c b) [(first r)] (first r) (rest r)))
           (let [b (if (> (count c) (count b)) c b)]
                (if (> (count b) 1) b []))))

(fn [x] (->> x (rest) (reduce #(
                                if (>= (last (second %)) %2)
                                [(first %) [%2]]
                                (if (<= (count (first %)) (count (second %)))
                                  [(concat (second %) [%2]) (concat (second %) [%2])]
                                  [(first %) (concat (second %) [%2])]))

                       [[(first x)] [(first x)]]) (first) (#(if (= 1 (count %)) [] %))))

(fn [s]
  (or
    (->>
      (map list s (range))
      (partition-by #(apply - %))
      (map #(map first %))
      (filter #(> (count %) 1))
      (sort-by (comp - count))
      first)
    []))
(fn [nums]
    (let [zs (filter #(< 1 (count %))
                     ((fn lis [[x & xs]]
                        (let [[a b] (reduce (fn [[ms ns] n]
                                              (if (< (last ns) n)
                                                [ms (conj ns n)]
                                                [(conj ms ns) [n]]))
                                            [[] [x]] xs)]
                          (conj a b))) nums))]
      (if (empty? zs)
        []
        (reduce #(if (< (count %1) (count %2)) %2 %1) zs))))
#(loop [coll % curr [] longest []]
  (if (empty? coll)
    (if (> (count longest) 1)
      longest
      [])
    (if (or (empty? curr) (= (first coll) (-> curr last inc)))
      (let [newcurr (conj curr (first coll))]
        (if (> (count newcurr) (count longest))
          (recur (rest coll) newcurr newcurr)
          (recur (rest coll) newcurr longest)))
      (recur (rest coll) (vector (first coll)) longest))))
(fn [[init1 & lst]]
  (loop [[fst & rst] lst
         run (vector init1)
         longest []]
     (if (nil? fst)
      longest
      (if (= fst (inc (last run)))
        (recur rst (conj run fst) (if (> (inc (count run)) (count longest)) (conj run fst) longest))
        (recur rst [fst] longest)))))
(fn longest-sub [c]
        (let [c (map (fn [[a :as c]] (map
                                      #(vector % (= (- % a) %2))
                                      c (range)))
                     (partition-all (count c) 1 c))
              d (filter #(> (count %) 1)
                        (map #(take-while second %) c))]
          (if (empty? d) [] (map first (apply max-key count d)))))
#(loop [r [] rs 0 l %] (let [[h s t] (loop [h [(first l)] s 1 t (next l)] (if (or (nil? t) (<= (first t) (first h))) [(reverse h) s t] (recur (cons (first t) h) (inc s) (next t))))] (let [[nr ns] (if (and (> s rs) (> s 1)) [h s] [r rs])] (if (nil? t) nr (recur nr ns t)))))
(fn lcss [xs]
  (letfn [(incr [ys zs]
                (cond (empty? zs) (recur (rest ys) (list (first ys)))
                      (empty? ys) zs
                      (> (first ys) (last zs)) (recur (rest ys) (concat zs (list (first ys))))
                      :else zs))]
    (let [n (count xs)
          rs (take n (iterate rest xs))
          ts (map #(incr % nil) rs)
          vs (apply (partial max-key count) (reverse ts))]
      (if (< 1 (count vs)) vs []))))
(fn [in] (let [seqs (mapcat #(partition % 1 in) (range (count in) 1 -1))] (or (some #(if (= % (take (count %) (iterate inc (first %)))) %) seqs) [])))
(fn longest-inc-sub [l]
    (let [longest-seq
          (fn [a b]
            (if (> (count a) (count b)) a b))]
      (reverse (loop [l l last_num 0 longest '() working '()]
                 (if (empty? l)
                   (let [f (longest-seq longest working)]
                     (if (> (count f) 1)
                       f
                       '()))
                   (if (= (first l) (inc last_num))
                     (recur (rest l) (first l) longest (cons (first l) working))
                     (let [newlong (longest-seq working longest)]
                       (recur (rest l) (first l) newlong (list (first l))))))))))
(fn [c1]
  (loop [c (rest c1)
         s []
         curr-s [(first c1)]]
    (if (not (seq c))
      s
      (let [x (first c)]
        (if (= (inc (last curr-s)) x)
          (let [ncurr-s (conj curr-s x)]
            (if (> (count ncurr-s) (count s))
              (recur (rest c) ncurr-s ncurr-s)
              (recur (rest c) s ncurr-s)))
          (recur (rest c) s [x]))))))
(fn prob-0053
  [xs]
  (let [
        take-group (fn [f in-xs]
      ;; TODO Use lazy-seq
                    (let [seq-xs (seq in-xs)
                          x      (first seq-xs)]
                      (if (empty? seq-xs)
                        nil
                        (loop [xs   (rest seq-xs)
                               lhs  x
                               rans [x]]
                          (let [rhs (first xs)]
                            (if (or (empty? xs) (not (f lhs rhs)))
                              (reverse rans)
                              (recur (rest xs) rhs (cons rhs rans))))))))

        dyad-partition-by (fn [f in-xs]
      ;; TODO Use lazy-seq
                           (loop [xs   (seq in-xs)
                                  rans []]
                             (let [grp  (take-group f xs)]
                               (if (empty? grp)
                                 (reverse rans)
                                 (recur (drop (count grp) xs) (cons grp rans))))))

        select-by (fn [f-eval f-select xs]
      ;; TODO: Could make this more efficient by caching the result of the eval in lhs.
                   (reduce #(if (f-select (f-eval %1) (f-eval %2)) %1 %2) xs))

        longest (fn [xs]
                 (select-by count >= xs))]

   (let [sq (longest (dyad-partition-by < xs))]
     (if (< (count sq) 2)
       []
       sq))))
(fn [s]
  (let [succ (fn [[a b]] (= b (inc a)))
        r
        (last (sort-by count
               (remove #(or (< (count %) 1)
                         (not (succ (first %))))
                 (partition-by
                   succ
                   (map vector s (drop 1 s))))))]
    (concat (map first r) (rest (last r)))))
(fn break [s]
  (let [subseqs
        (letfn [(lp [cur [hd & rst]]
                  (cond (nil? hd) [cur]
                        (empty? cur) (lp [hd] rst)
                        (> hd (last cur)) (lp (conj cur hd) rst)
                        :else (cons cur (lp [hd] rst))))]
          (lp [] s))
        result (first (sort-by count > subseqs))]
    (if (> (count result) 1) result [])))
(fn [x]
  (letfn [(increasing-seq [[f s & t :as c]]
           (if (seq c)
               (if (seq? f)
                   (if (= (inc (last f)) s)
                    (increasing-seq (cons (concat f (list s)) t))
                    (cons f (increasing-seq (rest c))))
                   (if (= (inc f) s)
                    (increasing-seq (cons (list f s) t))
                    (increasing-seq (rest c))))))]
    (let [li (last (sort-by count (increasing-seq x)))]
      (if (seq li) li []))))
#(nth
   (reduce
    (fn [[v m] e]
      (let [n {e (if (m (dec e))
                   (conj (m (dec e)) e)
                   [e])}]
        [(let [f (first (vals n))
               c (count f)]
           (if (and (<= 2 c) (< (count v) c)) f v))
         n]))
    [[] {}]
    %)
   0)
(fn [ts]
  (let [ret (apply max-key count (reduce (fn [[cur max] e] (if (or (empty? cur) (< (last cur) e)) [(conj cur e) max] [[e] (if (> (count cur) (count max)) cur max)])) [[] []] ts))]
   (if (> (count ret) 1) ret [])))
(fn l [n r c]
   (let [[s & _] (filter #(apply < %) (partition n 1 c))]
     (if s
       (l (+ n 1) s c)
       r))) 2 []
(fn [coll]
  (let [incrseqs
        ((fn [coll seqs curr]
          (if-let [s (seq coll)]
            (if (or (empty? curr) (> (first s) (last curr)))
               (recur (rest s) seqs (conj curr (first s)))
               (recur (rest s) (conj seqs curr) [(first s)]))

            (conj seqs curr)))


         coll [] [])]
   (let [ l (reduce
             (fn [a b] (if (> (count b) (count a)) b a))
             incrseqs)]
    (if (> (count l) 1) l []))))


(fn [x]
  (loop [ind x
         slopes [[]]]
    (if (empty? ind)
      (let [ms (reverse (last (sort-by count slopes)))]
        (if (>= (count ms) 2) ms []))
      (if (and
           (not-empty (do (println slopes) (first slopes)))
           (> (first ind) (first (first slopes))))
        (recur (rest ind) (cons (cons (first ind) (first slopes))
                                slopes))
        (recur (rest ind) (cons [(first ind)] slopes))))))
(fn [v]
  (loop [v_ v ret '() ret_c 0 cand '() cand_c 0]
    (let [a (first v_) n (next v_)]
      (cond
        (and (> cand_c ret_c) (>= cand_c 2))
        (recur v_ cand cand_c cand cand_c)
        (empty? v_)
        (into '() ret)
        (or (empty? cand) (> a (first cand)))
        (recur n ret ret_c (conj cand a) (inc cand_c))
        :else
        (recur n ret ret_c (list a) 1)))))
(fn __ [ll]
 (let [g (fn g[ll]
          (cond (empty? ll) ll
           :else
            (loop [l (rest ll) old (first ll) res [(first ll)]]
              (cond
               (empty? l) (if (> (count res) 1) (list res []) (list [] []))
               (< old (first l))  (recur (rest l) (first l) (conj res (first l)))
               :else (list res l)))))]
   (loop [l ll res nil n 0]
    (cond
     (empty? l) (if (> (count res) 1) res  [])
     :else (let [[l1 l2] (g l)]
            (if (< n (count l1))  (recur l2 l1 (count l1))
                (recur l2 res n)))))))
(fn longest [s]
  (let [incseqs (fn [s]
                  (loop [s s cr {:curr [] :result []}]
                    (println cr)
                    (if (not (seq s)) (cond (empty? (:curr cr)) (:result cr)
                                            :else (conj (:result cr) (:curr cr)))
                        (recur (rest s) (cond (empty? (:curr cr)) (conj cr {:curr [(first s)]})
                                              (= (inc (last (:curr cr))) (first s)) (conj cr {:curr (conj (:curr cr) (first s))})
                                              :else {:curr [(first s)] :result (conj (:result cr) (:curr cr))})))))]
    (let [tmp (first (sort-by #(- (count %)) (incseqs s)))]
      (if (= (count tmp) 1) []
          tmp))))
(fn [s]
 (or
  (first
   (filter #(every? (fn [[a b]] (< a b)) (partition 2 1 %))
           (map (fn [[a b]] (subvec s a b))
                (sort-by #(apply - %)
                         (let [c (inc (count s))]
                              (for [i (range c)
                                    j (range c)
                                    :when (> j (inc i))]
                                   [i j]))))))
  []))
(fn [col]
  ((fn [l] (concat (first l) (map second (rest l))))
   (last
     (first
       (sort
         (map (fn [i l] [(- (count l)) i l])
           (range)
           (keep (fn [l] (when (apply < (first l)) l))
             (partition-by (partial apply <)
               (partition 2 1 col)))))))))
(fn [x]
  (reduce
    #(if (> (count %2) (count %)) %2 %1)
    []
    (filter #(> (count %) 1)
      (reductions
       #(if (= (last %1) (dec %2)) (concat %1 [%2]) [%2])
       []
       x))))
(fn [s]
   (letfn [(long-head-seq [x] (take-while identity (map #(#{%1} %2) x (iterate inc (first x)))))]
     (loop [nextbit s longest [] next-head (long-head-seq s)]
       ;(println nextbit longest)
       (if-not nextbit
         longest
         (recur
           (next nextbit)
           (if (and (> (count next-head) 1)
                    (> (count next-head) (count longest)))
               (vec next-head) (vec longest))
           (long-head-seq nextbit))))))

#(let [accs (ref [])]

    (letfn [(p [acc current]
              (if (empty? acc)
                (conj acc current)
                (let [last-elem (last acc)]
                  (if (> current last-elem)
                    (conj acc current)
                    (do
                      (dosync
                       (alter accs conj acc)
                       (conj [] current)))))))]
      (let [result (reduce p [] %)]
        (dosync
         (alter accs conj result))
        (let [final (first (reverse (sort @accs)))]
          (if (> (count final) 1)
            final
            [])))))
(fn [xs] (or (->>
              (map vector xs (range))
              (partition-by #(apply - %))
              (map #(map first %))
              (filter #(> (count %) 1))
              (sort-by (comp - count))
              first) []))
(fn [s]
  (first (sort-by #(* -1 (count %))
          (concat (map (comp vec set (partial apply concat))
                   (filter #(< (first (first %)) (second (first %)))
                     (partition-by (partial apply compare)
                       (partition 2 1 s)))) '([])))))
(fn [coll]
  (->> (partition 2 1 coll)
    (partition-by #(- (second %) (first %)))
    (filter #(= 1 (- (second (first %)) (ffirst %))))
    (reduce #(if (< (count %1) (count %2)) %2 %1) [])
    flatten
    distinct))
(fn longest-increasing-sub-seq [coll]
    (letfn [(longest-increasing [coll]
              (let [longest-partitioned (take-while (fn [[x y]] (< x y))
                                                    (partition 2 1 coll))
                    c (count longest-partitioned)]
                (if (= 0 c)
                  '()
                  (take (inc c) coll))))
            (sub-seqs [coll]
              (map #(drop % coll) (range (dec (count coll)))))]
      (reduce (fn [t e] (if (> (count e) (count t)) e t))
              []
              (map longest-increasing (sub-seqs coll)))))
(fn liss [seq]
         (let [res (first (reduce (fn [[longest curr] e]
                                   (if (or (empty? curr) (> e (last curr)))
                                       (if (= (count longest) (count curr))
                                           [(conj curr e) (conj curr e)]
                                           [longest (conj curr e)])
                                       [longest [e]]))
                           [[] []] seq))]
           (if (< (count res) 2) [] res)))
(letfn [(nondec [coll]
         (letfn [
                 (fnondec [accum coll]
                    (cond
                      (and (empty? coll) (< (count accum) 2)) (hash-map :acc accum :left [])
                      (and (empty? coll) (>= (count accum) 2)) (hash-map :acc accum :left [])
                      (empty? accum) (recur (conj accum (first coll)) (rest coll))
                      (< (last accum) (first coll)) (recur (conj accum (first coll)) (rest coll))
                      (>= (last accum) (first coll)) (hash-map :acc accum :left coll)
                      :else "error"))]



            (let [fnon (fnondec [] coll)
                  acc (get fnon :acc)
                  left (get fnon :left)]
              (cond
                (and (empty? left) (empty? acc)) []
                (< (count acc) 2) (nondec left)
                (empty? left) [acc]
                :else (concat [acc] (nondec left))))))]




  (fn [z](reduce #(if(>= (count %1) (count %2)) %1 %2) [] (nondec z))))



(fn [xs]
   (let [seqs (reduce (fn [{:keys [all want]} i]
                        (let [cur (peek all)
                              prev (pop all)]
                          {:all (if (= i want)
                                  (conj prev (conj cur i))
                                  (conj all [i]))
                           :want (inc i)}))
                      {:all [[]] :want nil}
                      xs)]
     (apply max-key count
            (remove #(= 1 (count %))
                    (:all seqs)))))
(fn longest [s]
    (:l (reduce
            (fn [h r]
                (if (= 1 (- r (last (h :s))))
                    (let [ns (conj (h :s ) r), cnt (count ns)]
                        (if (and (>= cnt 2) (> cnt (count (h :l))))
                            {:l ns :s ns}
                            (assoc h :s ns)))
                    (assoc h :s [r])))
            {:l [] :s [0]} s)))
(fn [col] (loop [i (first col) j (rest col) ir [] fr '()] (if (nil? i) (first (reverse (sort-by count (map #(if (< 1 (count %)) % []) fr)))) (if (= (inc i) (first j)) (recur (first j) (rest j) (vec (concat ir [i])) fr) (recur (first j) (rest j) [] (conj fr (vec (concat ir [i]))))))))
#(let [q (loop [[a b & c :as s] %, t [a], r []]
           (if b
               (if (> b a)
                   (recur (rest s) (conj t b) r)
                   (recur (rest s) [b] (if (> (count t) 1) (conj r t) r)))
               (if (> (count t) 1) (conj r t) r)))
       g (group-by count q)]
    (if (empty? g) [] (first (g (->> g keys (apply max))))))
(fn [x]
  (reverse
    (apply max-key count
      [] (filter #(> (count %) 1)(reduce
                                  (fn [seq x]
                                    (if (empty? (first seq))
                                        (cons (cons x (first seq)) (rest seq))
                                        (if (> x (first (first seq)))
                                            (cons (cons x (first seq)) (rest seq))
                                            (cons (list x) seq))))
                                  (list '()) x)))))
(fn [coll]
  (->> (partition 2 1 coll)
    (partition-by #(- (second %) (first %)))
    (filter #(= 1 (- (second (first %)) (ffirst %))))
    (reduce #(if (< (count %1) (count %2)) %2 %1) [])
    flatten
    distinct))
#(let [c count [x y] (reduce
                      (fn [[a b] e]
                        (if (= b [])
                          [[e] [e]]
                          (if (= (+ 1 (last a)) e)
                            [(conj a e) b]
                            [[e] (if (> (c a) (c b))
                                  a
                                  b)])))




                      [[] []]
                      %)]
   (let [r (if (> (c x) (c y)) x y)] (if (< (c r) 2) [] r)))

(fn [lst]
  (let [
        sufs  (map #(take (+ 1 %) lst) (range (count lst)))
        prefs (fn [lst] (map #(drop % lst) (range (count lst))))
        sups  (mapcat prefs sufs)
        asc   (filter #(apply < %) sups)
        longest (apply max-key count (reverse asc))
        atleast2 (if (< 1 (count longest)) longest [])]
   atleast2))
(fn [coll]
  (let [llen (fn [coll] (reductions
                         (fn [xs y]
                           (if (= y (inc (last xs))) (conj xs y) [y]))
                         [(first coll)]
                         (rest coll)))]
   (vec (last (filter #(< 1 (count %)) (sort-by count (llen coll)))))))
(fn lseq [in-vals]
  (letfn
      [(better-seq [seq1 seq2]
         (if (> (count seq2) (count seq1)) seq2 seq1))
       (lseq2 [vals]
         (loop [current-max []
                current-seq []
                items vals]
           (if-not (seq items)
             (better-seq current-max current-seq)
             (if (every? #(> (first items) %) current-seq)
               (recur current-max (conj current-seq (first items)) (rest items))
               (recur (better-seq current-max current-seq) [(first items)] (rest items))))))]
    (let [result (lseq2 in-vals)]
      (if (> (count result) 1)
        result
        []))))
(fn f2 [coll] (let [f1 (fn [coll]   (reduce #(if (>= (count %) (count %2)) % %2)
                                     (reductions (fn [x y] (cond (= (count x) 0) [y]
                                                            (< (last x) y) (conj x y)
                                                            :else [y])) [] coll)))]
               (if (> (count (f1 coll)) 1) (f1 coll) [])))
(fn [vv] ( apply max-key count
          (cons []
               (map #(( comp distinct flatten) %)
                    (filter (fn [[[it1 it2] & more]] ( = (inc it1) it2))
                            ( partition-by #(- (last %) (first %))
                                            ( #( partition 2 (interleave % (next %)) ) vv)))))))
(fn [coll]
   (let [f (first coll)]
     ((fn [longest now lst r]
        (if (empty? r)
          (if (= (count longest) 1) [] longest)
          (if (= (first r) (+ lst 1))
            (let [cc (conj now (first r))]
              (if (= (count longest) (count now))
                (recur cc cc (first r) (rest r))
                (recur longest cc (first r) (rest r))))
            (recur longest [(first r)] (first r) (rest r)))))
      [f] [f] f (rest coll))))
(fn [xs]
  (let [s (butlast xs)
          t (rest xs)
           st (map #(vector %1 (- %2 %1)) s t)
           p (partition-by second st)
           f (filter #(= 1 (second (first %))) p)]
    (if (empty? f)
      []
      (let [a (last (sort-by count f))
            b (map first a)]
        (concat b [(inc (last b))])))))

(fn [s]
    (->>
      (for [a (range (count s))
            b (range (inc a) (count s))]
        (subvec s a (inc b)))
      (filter #(apply < %))
      (sort-by count >)
      first
      vec))
(fn [coll]
  (let [get-longest
        (fn [x y]
          (let [longest (if (> (count x) (count y)) x y)]
            (if (< (count longest) 2) [] longest)))]

   (loop
     [current [(first coll)]
      longest []
      c coll
      rc (rest coll)]
     (if (empty? rc)
       (get-longest current longest)
       (if (> (first rc) (first c))
         (recur (conj current (first rc)) longest (rest c) (rest rc))
         (recur [(first rc)] (get-longest current longest)
           (rest c) (rest rc)))))))
(fn seqs [coll] (let [long-seqs ; Map of longest sub-seqs against longest length
                      (first (sort
                              (fn [x y] (compare (key y) (key x))) ; Reverse comparison for largest length 1st
                              (group-by count ; Map against length
                                  (reduce ; Get a list of the sequences
                                       (fn [result node]
                                           (cond
                                                  (empty? result) (vector (vector node)) ; 1st sequence
                                                  (> node (last (last result))) (conj (pop result) (conj (last result) node)) ; Add to current sequence
                                                  :default (conj result (vector node)))) ; Start new sequence
                                       []
                                       coll))))]
                 (cond
                    (< (first long-seqs) 2) []
                    :default (first (second long-seqs)))))
(fn [coll]
    (let [one-or-more
             (fn [coll] (filter #(> (count %) 1) coll))
          longest
             (fn [coll] (reduce #(if (> (count %2) (count %)) %2 %) [] coll))
          liss-
             (fn [coll]
               (loop [res []
                      curr [(first coll)]
                      par (partition 2 1 coll)]
                 (if (seq par)
                   (let [[x y] (first par)]
                     (if (< x y)
                       (recur res (conj curr y) (rest par))
                       (recur (conj res curr) [y] (rest par))))
                   (conj res curr))))]
      (longest (one-or-more (liss- coll)))))
(fn [a]
  (condp = (first a)
    1 [0 1 2 3]
    5 [5 6]
    2 [3 4 5]
    7 []))
#(
  (fn [longest, current, array]
    (if (= nil array)
      longest
      (let [longest_length (if (= 0 (count longest)) 1 (count longest))
            next_item (first array)
            max_current (if (= nil (last current)) 0 (last current))
            current (if (> next_item max_current) (conj current next_item) [next_item])]
           (if (> (count current) longest_length) (recur current current (next array)) (recur longest current (next array)))))) [] [] %)
(fn [mylist]
  (loop [l mylist, curseq [], seqs []]
    (if (= l '())
        ; l empty, return longest seq
        (if (= curseq [])
            (reduce (fn [a1 a2] (if (> (count a2) (count a1)) a2 a1 )) [] seqs)
            (reduce (fn [a1 a2] (if (> (count a2) (count a1)) a2 a1 )) []
                  (conj seqs curseq)))
        ;seqs
        ; not empty, look for seqs
        (if (= curseq []) ; curseq empty, try to find new seq
            (if (= (inc (first l)) (fnext l))
                (recur (drop 2 l), [(first l) (fnext l)], seqs)
                (recur (rest l), [], seqs))
          ; not empty, check if next element fits
            (if (= (dec (first l)) (last curseq))
          ; fits, add to curseq
                (recur (rest l), (conj curseq (first l)), seqs)
          ; doesn't fit, add seq to seqs
                (recur l, [], (conj seqs curseq)))))))

(fn [s]
  (reduce #(if (> (count %1) (count %2)) %1 %2)
    (map (comp distinct (partial apply concat))
      (partition-by count
        (map
          #(if (= %2 (inc %1)) [%1 %2] [])
          (butlast s)
          (rest s))))))
(fn longest-increasing-sub-seq [xs]
  (let [p1 (map #(= (inc %1) %2) xs (rest xs))
        p2 (cons true p1)
        qual (fn [acc keep-ind]
               (let [[ind ys] acc
                     nind (inc ind)]
                 (if (true? keep-ind)
                   [ind (cons ind ys)]
                   [nind (cons nind ys)])))
        p3 (reverse (second (reduce qual [0 []] p2)))
        p4 (map vector xs p3)
        p5 (partition-by second p4)
        p6 (map #(map first %) p5)
        p7 (filter #(> (count %) 1) p6)
        p8 (reverse (sort-by count p7))
        p9 (first p8)]
    (if (nil? p9)
      []
      p9)))
#(let [r (reduce
            (fn [v i] (if (> (count i) (count v)) i v))
            (reductions (fn [v e]
                          (if (empty? v)
                            (conj v e)
                            (if (= e (inc (last v)))
                              (conj v e)
                              [e]))) [] %))]
    (if (< (count r) 2) [] r))
(fn [seq]
    (loop [s (rest seq)
           prev (first seq)
           cur-seq [prev]
           res-seq []]
      (if (empty? s)
        res-seq
        (let [next (first s)
              ?cur-seq (conj cur-seq next)
              ?cur-len (count ?cur-seq)
              res-len (count res-seq)]
          (if (= (inc prev) next)
            (recur (rest s)
                   next
                   ?cur-seq
                   (if (> ?cur-len res-len)
                     ?cur-seq
                     res-seq))
            (recur (rest s)
                   next
                   [next]
                   res-seq))))))
#(apply max-key %
  (reverse
   (for [x (%2 (% %3)) % (%2 x (- (% %3) 1))
         :let [% (subvec %3 x (+ % 2))]]
    (if (apply < %) % []))))
count range
(fn longest-subseq
  [coll]
  (when-let [s (seq coll)]
    (let [subseqs (reduce (fn [seqs x]
                            (let [last-subseq (first seqs)]
                              (if (and seqs (< (peek last-subseq) x))
                                (cons (conj last-subseq x) (next seqs))
                                (cons [x] seqs))))
                          nil coll)
          longest (apply max-key count subseqs)]
      (if (< 1 (count longest))
        longest
        []))))
(fn [coll]
  (->> (partition 2 1 coll)
    (partition-by #(- (second %) (first %)))
    (filter #(= 1 (- (second (first %)) (ffirst %))))
    (reduce #(if (< (count %1) (count %2)) %2 %1) [])
    flatten
    distinct))
(fn [coll]
  (->>
    (partition 2 1 coll)
    (partition-by #(- (second %) (first %)))
    (filter #(= 1 (- (second (first %)) (ffirst %))))
    (reduce #(if (< (count %1) (count %2)) %2 %1) [])
    flatten
    distinct))
(fn longest-increasing-subseq [lst]
  (let [group-seq (partition-by #(apply < %) (partition 2 1 lst))
        inc-seq (map #(distinct (apply concat %)) group-seq)
        count-seq (apply max (map count inc-seq))
        fin (first (filter #(and (= (count %) count-seq) (> (second %) (first %))) inc-seq))]
    (if (seq? fin)
      fin
      [])))

(fn [x] (apply max-key count
               (rseq (vec (cons []
                           (filter #(apply < %)
                             (for [i (range (count x))
                                   j (range (+ i 2) (+ 1 (count x)))]
                               (subvec x i j))))))))
(fn [s]
    (first (sort-by (comp - count)
                    (map (fn [s]
                           (loop [s s, r []]
                             (let [p (last r)
                                   n (first s)]
                               (if (and (seq s) (or (nil? p) (< p n)))
                                 (recur (rest s) (conj r n))
                                 (if (< (count r) 2) [] r)))))
                         (take (count s) (iterate next s))))))
(fn [coll]
    (let [d (map-indexed - coll)
          p (partition-by second (map list coll d))
          c (map (fn [c] [(map first c) (count c)]) p)
          [[i _] & _] (sort-by second > c)]
     (if (> (count i) 1)
         i
         [])))
(fn [c] (distinct (flatten (reduce #(if (< (count %1) (count %2)) %2 %1) [] (filter #(= 1 (apply - (reverse (first %)))) (partition-by #(apply - (reverse %)) (partition 2 1 c)))))))
(fn foo [lst]
      (let [theList
            (apply max-key count
             (reverse
              (map (fn [sublst] (take-while (fn [n] (not (= -1 n))) sublst))
               (for [x (range 0 (count lst))]
                    (keep-indexed (
                                   fn [idx item] (if (or (= 0 idx) (> item (nth lst (- (+ x idx) 1)))) item -1))
                     (drop x lst))))))]
         (if (< (count theList) 2) []
                              theList)))
#(let [[a b e] (reduce (fn [[m c p] e]
                        (if (> e p)
                          [m (conj c p) e]
                          (if (> (inc (count c)) (count m))
                            [(conj c p) [] e]
                            [m [] e])))
                       [[] [] (first %)] (rest %))
       r (if (>= (count a) (inc (count b)))
             a
             (conj b e))]
     (if (>= (count r) 2)
       r
       []))
(fn [lst]
  (let [rc (first
            (filter vector?
                    (map #(cond (< (count %) 2) false (= % (-> % sort distinct)) %)
                         ;; make increasingly short subvectors of lst
                         (map #(subvec lst (first %) (second %))
                              ;; make a vector of vectors of the indices for subvec
                              (for [x (-> lst count range) y (range x)]
                                (vector y (+ (- (-> lst count inc) x) y)))))))]
    (or rc []))) ; There's got to be a better way!
(fn [s]
 (remove #(= -2 %) (last (last (last (sort (group-by count
                                            (reduce (fn [[f & r :as l]  i] (if (= i (inc (last f))) (cons (conj f i) r) (cons [i] l)))
                                                    '([-2]) s))))))))
(fn [s]
  (let [res ((fn [s res cur n]
               (if (nil? s)
                 (if (nil? cur)
                   res
                   (cons cur res))
                 (let [a (first s)]
                   (if (nil? cur)
                     (recur (next s) res (list a) (inc a))
                     (if (= a n)
                       (recur (next s) res (cons a cur) (inc a))
                       (recur (next s) (cons cur res) (list a) (inc a)))))))
             s nil, nil, nil)
        longest (reduce (fn [s1 s2]
                         (if (> (count s1) (count s2)) s1 s2))
                        res)]
    (if (>(count longest) 1)
      (reverse longest)
      [])))
(fn [coll]
  (let [len (count coll)
        cmp (fn [[x1 y1] [x2 y2]] (< (- y1 x1) (- y2 x2)))]
    (loop [i 0 j 1 best [0 1]]
      (if (> i (dec len))
        (if (= 1 (- (last best) (first best)))
          []
          (subvec coll (first best) (last best)))
        (if (or (= j len)
                (not (= (- j i) (- (nth coll j) (nth coll i)))))
          (recur j (inc j)
                 (if (cmp best [i j]) [i j] best))
          (recur i (inc j) best))))))

(fn tramp-set [coll]
  (letfn [(seq-len [coll]
           (if (empty? (rest coll))
             1
             (if (> (second coll) (first coll))
               (+ 1 (seq-len (rest coll)))
               1)))
          (get-seq [res n coll]
            (if (= 0 n)
              res
              (get-seq (conj res (first coll)) (dec n) (rest coll))))]
    (let [arf (loop [res (hash-map) c coll]
                (if (empty? c)
                  res
                  (recur (merge-with concat res (hash-map (seq-len c) c)) (rest c))))
          m (apply max (keys arf))]
      (if (> 2 m)
        []
        (get-seq [] m (arf m))))))
(fn [s]
  (if (empty? s) []
    (letfn [(f [[x & c]]
              (let [y (first c)]
                (if (nil? y) [x]
                  (if (< x y)
                    (into [x] (f c))
                    [x]))))]
      (let [c (keep-indexed
               (fn [idx v]
                 (when-let [c (subvec s idx)]
                   (let [r (f c)]
                     (when (> (count r) 1)
                      r))))
               s)
            m (if (empty? c) 0 (count (apply max-key count c)))]
        (if (= m 0) []
          (reduce #(if (= (count %) m) % %2) c))))))
(fn [s] (-> (last (sort-by count (map (fn [[s c]] (map second (take-while #(= (second %) (get c (first %))) (keep-indexed vector s)))) (map (fn [s] [s (into [] (range (first s) (+ (first s) (count s))))]) (take-while
                                                                                                                                                                                                              not-empty (iterate rest s)))))) (#(if (= 1 (count %)) [] %))))
(fn [coll]
  (let
    [len (count coll)
     data
     ;; data[i] is the len of longest subseq starting at i
      (loop [index (dec len)
             data (vec (repeat len 1))]
        (if (> index 0)
          (let [cur-idx (dec index)
                cur-val (coll cur-idx)
                nxt-idx index
                nxt-val (coll nxt-idx)]
            (if (> nxt-val cur-val)
              (recur cur-idx
                (assoc data cur-idx (inc (data nxt-idx))))
              (recur cur-idx data)))
          data))

     ;; Have the data, now find the longest subseq
     [max-idx max-len]
     (loop [index 0, max-idx 0, max-len 1]
       (if (< index len)
         (if (> (data index) max-len)
           (recur (inc index) index (data index))
           (recur (inc index) max-idx max-len))
         [max-idx max-len]))]

    (if (> max-len 1)
      (subvec coll max-idx (+ max-idx max-len))
      [])))
(fn [coll]
  (->> (partition 2 1 coll)
       (partition-by #(- (second %) (first %)))
       (filter #(= 1 (- (second (first %)) (ffirst %))))
       (reduce #(if (< (count %) (count %2)) %2 %) [])
       flatten
       distinct))
(fn [xs]
  (let [crunch #(concat (map first %) (list (second (last %))))
        parted (partition-by #(< (first %) (second %)) (partition 2 1 xs))
        filtered (filter (fn [[[x y] & r]] (< x y)) parted)
        sorted (sort-by #(- (count %)) filtered)]
    (if (seq sorted)
      (crunch (first sorted))
      [])))
(fn [coll]
   (let [increasing? (fn [xs] (apply < xs))
         n (count coll)
         sub-seqs (mapcat #(partition % 1 coll) (range 2 (inc n)))]
     (->> sub-seqs
          (filter increasing?)
          (cons [])
          (sort-by count >)
          first)))
(fn [s]
  (letfn [(append-if-inc [act n]
            (let [n-1 (last act)]
              (if (or
                   (nil? n-1)
                   (= (inc n-1) n))
                (conj act n)
                [])))
          (append-if-inc2 [act n]
            (let [n-1 (last act)]
              (if (or
                   (nil? n-1)
                   (= (inc n-1) n))
                (conj act n)
                act)))] ; the only difference
    ; recur label
    (loop [lngst []
           act [(first s)]
           rem (rest s)]
      (if (empty? rem)
        lngst
        (let [act-new (append-if-inc act (first rem))]
          (cond
           (> (count act-new) (count lngst)) (recur act-new act-new (rest rem))
           (= (count act-new) (count lngst)) (recur lngst (append-if-inc2 act-new (first rem)) (rest rem))
           :else (recur lngst [(first rem)] (rest rem))))))))
(fn [sq]
  (let [v
        (first (sort (comparator #(> (count %1) (count %2)))
                     (filter #(> (count %) 1) (loop [[h & t] sq cur -1 s [] r []]
                                                (if h
                                                  (if (> h cur)
                                                    (recur t h (conj s h) r)
                                                    (recur (cons h t) -1 [] (conj r s))) (conj r s))))))]
    (if v v [])))
(fn [s]
    (->>(count s)
     ((fn [n]
         (reduce into
          (map (fn [x y] (map (partial vector x) y))
            (range 1 (inc n))
            (map (constantly (range 1 (inc n)))
              (range n))))))

     (filter (fn [[x y]] (< x y)))
     (map (fn [[x y]] (->> s (take y) (drop (dec x)))))
     (reduce conj ())
     (filter #(= % (sort %)))
     (filter #(apply distinct? %))
     (group-by count)
     (#(get % (apply max -1 (keys %))))
     (first)
     (vec)))


(fn [c]
    (->> (partition 2 1 c)
         (partition-by #(< (first %) (second %)))
         (map #(concat (first %) (mapcat (fn [[a b]] [b]) (rest %))))
         (filter #(and (> (count %) 1) (< (first %) (second %))))
         (reduce #(if (< (count %1) (count %2)) %2 %1) ())))
(fn [coll]
  ((fn [[a l]]
    (if (> (coll (+ a (- l 1))) (coll a))
     (subvec coll a (+ a l))
     []))
   (->> coll
     (partition 2 1)
     (partition-by (fn [[x y]] (< x y)))
     (map count)
     ((fn [v] [(reductions + 0 v) (map inc v)]))
     (apply zipmap)
     (apply max-key second))))
(fn [l]
  (letfn [(split-seq [l]
            (if (empty? l) nil
                (let [r (split-seq (rest l))]
                  (if (empty? r)
                    (cons (list (first l)) nil)
                    (if (< (first l) (first (first r)))
                      (cons (cons (first l) (first r)) (rest r))
                      (cons (list (first l)) r))))))]
    (let [r (apply max-key count (reverse (split-seq l)))]
      (if (<= (count r) 1)
        []
        r))))
(fn [ls]
  (reduce #(if (or (> (count %1) (count %2)) (< (count %2) 2)) %1 (reverse %2)) ()
   (reduce
     #(if (< (first (first %1)) %2)
        (cons (cons %2 (first %1)) %1)
        (cons (list %2) %1))
     (list (list (first ls))) (rest ls))))
(fn lis [coll]
  (letfn
      [(helper [a]
               (loop [k 0 q (vec (repeat (count a) nil)) P (vec (repeat (count a) nil))]
                 (if (= k (count a))
                   [q P]
                   (let [m (loop [j 0 result 0 max-idx nil]
                             (if (= j k)
                               [result max-idx]
                               (if (> (a k) (a j))
                                 (if (> (q j) result)
                                   (recur (inc j) (q j) j)
                                   (recur (inc j) 1 j))
                                 (if (= (a k) (a j))
                                   (recur (inc j) 0 nil)
                                   (recur (inc j) result max-idx)))))]
                    (recur (inc k) (assoc q k (inc (first m))) (assoc P k (second m)))))))]
    (let [result (helper coll)
          L (first result)
          P (second result)
          idx (.indexOf L (apply max L))]
      (if (= (P idx) nil)
        []
        (loop [idx idx result []]
          (cond (= (P idx) nil)  (cons (coll idx) result)
            :else (recur (P idx) (cons (coll idx) result))))))))
(fn [s]
   (let [subseqs (fn ! [co] (when-let [se (seq co)] (cons se (! (rest se)))))
         longest (fn [ss] ((fn ! [agg c]
                             (if-let [s (seq c)]
                               (if (= (inc (last agg)) (first s))
                                 (! (conj agg (first s)) (rest s))
                                 agg) agg))
                           [(first ss)] (rest ss)))
         result  (apply max-key count (map longest (subseqs s)))]
     (if (>= (count result) 2)
       result
       [])))

(fn [coll]
  (loop [coll coll curr [] longest []]
    (if (seq coll)
      (if (empty? longest)
        (recur (next coll) [(first coll)] [(first coll)])
        (if (> (first coll) (if (last curr) (last curr) -1))
          (let [curr (conj curr (first coll))]
            (recur (next coll) curr (if (> (count curr) (count longest)) curr longest)))
          (recur (next coll) [(first coll)] longest)))
      (if (>= (count longest) 2)
        longest
        []))))
#(vec
   (last
     (filter
       second
       (sort-by
         count
         (reduce (fn [[c & m] v]
                   (if (or (empty? c) (= (peek c) (dec v)))
                     (concat [(conj c v)] m)
                     (concat [[v]] [c] m))) [[]] %)))))
(fn [xs]
  (first (reduce
          (fn [[greatest current] x]
            (if (empty? current)
              [greatest [x]]
              (if (> x (last current))
                (let [current (conj current x)]
                  (if
                    (> (count current) (count greatest))
                    [current current]
                    [greatest current]))
                [greatest [x]])))
          [[] []] xs)))
(fn [xs]
  (let [ranges (fn [n] (mapcat (fn [m] (map #(list m %) (range (inc m) (inc n)))) (range (inc n))))
        subseqs (map #(apply subvec xs %) (ranges (count xs)))
        ascending (filter (partial apply <) subseqs)
        longest (reduce #(if (>= (count %1) (count %2)) %1 %2) ascending)]
   (if (> (count longest) 1) longest [])))
(fn [s]
  (loop [r1 nil r2 [] p (first s) s (rest s)]
    (if-not (empty? s)
      (if (= (+ p 1) (first s))
        (recur (if r1 (concat r1 [(first s)]) [p (first s)]) r2 (first s) (rest s))
        (if (and r1 (> (count r1) (count r2)))
          (recur nil r1 (first s) (rest s))
          (recur r1 r2 (first s) (rest s))))
      (vec (if (and r1 (> (count r1) (count r2))) r1 r2)))))
(fn [coll]
  (let [normalize #(if (and (> (count %2) 1) (> (count %2) (count %1))) [%2 %2] [%1 %2])]
   (first (reduce (fn [seqs num]
                   (let [[longest-seq curr] seqs]
                     (if (or (empty? curr) (= (dec num) (last curr)))
                       (normalize longest-seq (conj curr num))
                       (normalize longest-seq [num])))) [[][]] coll))))
(fn [v]
  (letfn [(red [[current longest], i]
            (if (empty? current)
              [(conj current i) longest]
              (let [l (last current)
                    cc (count current)
                    cl (count longest)]
                (if (>= l i)
                  [[i] longest]
                  (if (> (inc cc) cl)
                    [(conj current i)(conj current i)]
                    [(conj current i) longest])))))]
   (second (reduce red [[][]] v))))
(fn [s]
  (let [
        inord #(< (first %) (second %))
        pairs
        (reduce #(if (< (count %) (count %2)) %2 %) '()
          (filter #(inord (first %))
            (partition-by inord
              (map #(list % %2) s (rest s)))))]

   (if (empty? pairs) '()
     (cons (first (first pairs)) (map last pairs)))))
(fn [s]
    (->> (map #(vector %1 (- %2 %1)) s (range))
         (partition-by second)
         (reduce (fn [[best len] item]
                   (let [c (count item)]
                     (if (> c len) [item c] [best len]))) '([] 1))
         first
         (map first)))
(fn [%]
  (let [get-max  (fn [a b] (if (> (count b) (count a)) b a))
        norm-inc (fn [%] (if (= 1 (count %)) [] %))
        inc-seq (fn inc-seq [%]
                  (loop [s %
                         v []]
                    (cond
                      (nil? s) (norm-inc v)
                      (or (empty?  v)
                          (= (first s) (inc (peek v)))) (recur (next s) (conj v (first s)))
                      :else (norm-inc v))))]
    (loop [s %
           max-s  []]
      (cond
        (nil? s) max-s
        :else (recur (next s) (get-max max-s (inc-seq s)))))))



(fn sub-seq [l]
  (loop [longest []
         current []
         in l]
    (cond (empty? in) (cond (>= (count longest) (count current)) longest
                            (> (count current) 1) current
                            :else [])

          (or (empty? current) (> (first in) (last current)))
          (recur longest (conj current (first in)) (rest in))

          (> (count current) (count longest))
          (recur (if (> (count current) 1) current []) [(first in)] (rest in))

          :else (recur (if (> (count current) (count longest)) current longest)
                       [(first in)] (rest in)))))
(fn [coll]
    (apply max-key count
     (conj (filter #(>= (count %1) 2)
             (loop [acc  (list)
                    x    (vector (first coll))
                    coll (rest coll)]
               (cond (empty? coll) (conj acc x)
                     (> (first coll) (last x)) (recur acc
                                                      (conj x (first coll))
                                                      (rest coll))
                     :else (recur (conj acc x)
                                  (vector (first coll))
                                  (rest coll))))) [])))
(fn [[x & r]]
  (or (first
       (->> r
            (reductions (fn [v n]
                           `[
                             ~@(if (< (peek v) n) v)
                             ~n])

                        [x])
            (group-by count)
            last
            second
            (filter #(< 1 (count %)))))
     []))
(fn max-inc-subseq [v]
  (loop [v_ v acc [] max []]
    (do (println (first v_) " " (last acc) " " acc " " max)
      (cond
       (empty? v_) max
       (empty? acc) (recur (rest v_)
                           (conj acc (first v_))
                           max)
       (= (inc (last acc)) (first v_)) (recur (rest v_)
                                              (conj acc (first v_))
                                              (if (>= (count acc) (count max)) (conj acc (first v_)) max))
       :else (recur (rest v_)
                    [(first v_)]
                    max)))))
(fn [s]
    ((fn [s a best]
        (if (empty? s)
            (if (> (count best) 1)
                (reverse best)
                [])
            (if (> (first s) (first a))
                (let [newa (cons (first s) a)]
                  (recur (rest s)
                         newa
                         (if (> (count newa)
                                (count best))
                             newa
                             best)))
                (recur (rest s) (list (first s)) best))))
     (rest s) [(first s)] [(first s)]))
(fn [s]
  (let [choose (fn [s t]
                 (if (>= (count s) (count t))
                   s
                   t))]
   (loop [s s
          lis []
          cs []]
     (cond
       (empty? s)
       (if (> (count lis) 1)
         lis
         [])
       (empty? cs)
       (recur (rest s)
              (choose lis [(first s)])
              [(first s)])
       (> (first s) (last cs))
       (recur (rest s)
              (choose lis (conj cs (first s)))
              (conj cs (first s)))
       :else
       (recur (rest s)
              (choose lis [(first s)])
              [(first s)])))))
(fn t[coll]
     (let [items
           (reductions
             (fn [c x]
              (if (< (last c) x)
                  (conj c x)
                  [x])) [(first coll)] (rest coll))]
      (reduce (fn [a b]
               (if (and (> (count b) 1) (> (count b) (count a)))
                   b a)) []
           items)))
(fn [s]
  (loop [ss (rest s) largest [] working [(first s)]]
    (let [cw (count working) cl (count largest)]
      (if (empty? ss)
        (if (and (> cw 1) (> cw cl))
          working
          largest)
        (let [n (first ss)]
          (recur (rest ss)
                 (if (and (> cw 1) (> cw cl))
                    working
                    largest)
                 (if (> n (last working)) (conj working n) [n])))))))
#(loop [xs %, best-c 0, best-s '()]
  (if (< (count xs) 2) best-s
    (let [s (map second (take-while (fn [p] (< (first p) (second p))) (partition 2 1 xs)))
          ss (if (empty? s) s (cons (first xs) s))
          c (count ss)
          [next-c next-s] (if (> c best-c) [c ss] [best-c best-s])]
      (recur (rest xs) next-c next-s))))
(fn [[a & s]]
  (let [l (first
           (reduce
            (fn [[g h] x]
              (let [h1 (if (> x (last h)) (conj h x) [x])]
                [(max-key count h1 g) h1]))
            [[a] [a]] s))]
    (if (> (count l) 1) l [])))
(fn [coll]
  (let [x1 (partition 2 1 coll)
        x2 (map (fn [[y1 y2]] (if (< y1 y2) [y1 y2] nil)) x1)
        x3 (partition-by class x2)
        x4 (filter #(identity (first %)) x3)
        x5 (reduce #(if (> (count %2) (count %1)) %2 %1) [] x4)]
    (if (seq x5)
      (concat (first x5) (for [[y1 y2] (drop 1 x5)] y2))
      x5)))
(fn sub-seq [col]
  (loop [cur_col col tmp_list [] final_list []]
    (if (empty? cur_col)
      (if (>(count tmp_list)(count final_list))
        (if (<(count tmp_list)2)[] tmp_list) (if (<(count final_list)2)[] final_list))

      (let [cont (if (not(nil? (last tmp_list)))(=(+(last tmp_list)1) (first cur_col))true)]
        (recur (rest cur_col)
          (if (true? cont) (conj tmp_list (first cur_col)) [(first cur_col)])
          (if (true? cont) final_list
            (if (<(count final_list) (count tmp_list))
              tmp_list final_list)))))))
(fn lis
  ([s] (lis s [] []))
  ([s candidates curr]
   (if (empty? s)
     (let [cand (if (> (count curr) 1) (conj candidates curr) candidates)]
       (reduce (fn [a b] (if (>= (count a) (count b)) a b)) [] cand))
     (if (empty? curr)
       (recur (rest s) candidates [(first s)])
       (if (> (first s) (last curr))
         (recur (rest s) candidates (concat curr [(first s)]))
         (if (> (count curr) 1)
           (recur (rest s) (conj candidates curr) [(first s)])
           (recur (rest s) candidates [(first s)])))))))
(fn [input]
  (apply max-key count []
    (filter #(> (count %) 1)
      (loop [s (rest input) buffer [(first input)] out []]
          (if (seq s)
            (if (= (first s) (inc (last buffer)))
                (recur (rest s) (conj buffer (first s)) out)
                (recur (rest s) [(first s)] (conj out buffer)))
            (conj out buffer))))))
(fn longest [collect]
  (loop [col (rest collect),
         preview (first collect),
         result [1],
         n 1,
         curr [(first collect)]]
    (if (empty? col)
        (cond (< n 2) []
              (< (first result) n) curr
              true (rest result))

        (let[
             val (first col),
             sign (> val preview)]

          (if  (true? sign)
              (recur (rest col)
                     val
                     result
                     (inc n)
                     (conj curr val))

              (recur (rest col)
                     val
                     (if (< (first result) n)
                         (cons n curr)
                         result)

                     1
                     [val]))))))






(fn [s]
  (#(concat
      (map first %)
      (take-last 1 (last %)))
    (apply max-key #(count %)
      (reverse
        (let [ss (map vector s (next s))]
          (map (fn [x]
                (take-while #(apply < %) x))
            (take (count s) (iterate rest ss))))))))




(fn [coll]
  (let [sub-seqs (reduce (fn [[curr-seq & r :as all] v]
                           (if (and (last curr-seq) (< (last curr-seq) v))
                             (cons (conj curr-seq v) r)
                             (cons [v] all)))
                         [[]]
                         coll)]
    (vec (last (sort #(compare (count %) (count %2)) (filter #(> (count %) 1) sub-seqs))))))
(fn longest-incr [sq]
  (let [s
        (filter
          #(> (count %) 1)
          (reduce (fn [sq x]
                    (if (empty? sq)
                      (list (list x))
                      (let [subsq (first sq)]
                        (if (= x (dec (first subsq)))
                          (cons (cons x subsq) (rest sq))
                          (cons (list x) sq)))))
                  (list)
                  (rseq sq)))]
    (if (= s [])
      []
      (apply (partial max-key count) s))))
(fn [lst]
   (->> (map vector lst (concat (rest lst) [(last lst)]))
        ((fn [res lst]
           (if (empty? lst)
             res
             (recur (into res
                          (vector (take-while #(< (first %) (second %))
                                              lst)))
                    (drop-while #(>= (first %) (second %))
                                (drop-while #(< (first %) (second %))
                                            lst)))))
         [])
        ((fn [lst]
           (reduce #(if (< (count %1) (count %2)) %2 %1)
                   lst)))
        (#(concat % [(reverse (last %))]))
        (map first)
        flatten
        (#(if (> (count %) 1)
            % []))))
(fn [ss]
  (reduce
   #(if (> (count %2) (count %1)) %2 %1)
   []
   (filter
    #(> (count %) 1)
    (loop [p (first ss) s (rest ss) a [p] r []]
      (let [fs (first s) sr (cons (reverse a) r)]
        (if (empty? s)
          (reverse sr)
          (if (< p fs)
            (recur fs (rest s) (cons fs a) r)
            (recur fs (rest s) [fs] sr))))))))
(fn [xs]
  (loop [longest []
         current []
         coll xs]
    (if (seq coll)
      (let [head (first coll)
            last-current (last current)]
        (if (or (nil? last-current) (> head last-current))
          (recur longest (conj current head) (rest coll))
          (if (> (count current) (count longest))
              (recur current [head] (rest coll))
              (recur longest [head] (rest coll)))))
      (if (>= (count longest) (count current))
        (if (> (count longest) 1) longest [])
        (if (> (count current) 1) current [])))))
(fn iss [s]
  (let [lefts (cons nil (drop-last s)),
        info (map #(vector (= (- %1 1) %2) %1 %2) s lefts),
        groups (filter #(= true (first (first %))) (partition-by first info)),
        longest (first (sort #(> (count %1) (count %2)) groups))]
    (if (nil? longest)
      []
      (cons (nth (first longest) 2) (map second longest)))))
#(first
  (reduce
    (fn [[max_seq curr_seq] item]
      (cond
        (empty? curr_seq)
        [[] [item]]
        (> item (last curr_seq))
        (if (and (> (count curr_seq) 0)
                 (>= (count curr_seq) (count max_seq)))
          [(conj curr_seq item) (conj curr_seq item)]
          [max_seq (conj curr_seq item)])
        :else
        [max_seq [item]]))
   [[] []]
   %))
(fn longest-inc-subseq
  ([s] (longest-inc-subseq s [] []))
  ([[x & xs :as s] found current]
   (if (nil? x)
       (if (empty? current)
        (last (sort-by count (map #(if (> (count %) 1) % []) found)))
        (longest-inc-subseq s (conj found current) []))
       (let [previous (last current)]
        (if (nil? previous)
            (longest-inc-subseq xs found [x])
            (if (= x (inc previous))
                (longest-inc-subseq xs found (conj current x))
                (longest-inc-subseq s (conj found current) [])))))))
#(apply max-key %
  (reverse
   (for [x (%2 (% %3)) % (%2 x (- (% %3) 1))
         :let [% (subvec %3 x (+ % 2))]]
     (if (apply < %) % []))))
count range
(fn [s]
    (letfn [(all-subs [s len]
              (let [n (- (count s) (dec len))]
                (map (fn [e] (take len (drop e s))) (range n))))
            (all-increasing [s]
              (for [e (range (dec (count s)) 1 -1)] (some #(if (apply < %) %) (all-subs s e))))]
      (let [r (some #(if (not (nil? %)) %) (all-increasing s))]
        (if r r []))))
(fn lets-go [big-ol-seq]

  (let [increasing-subseq (fn increasing-subseq [so-far to-do]
                            (cond (empty? to-do) [so-far to-do]

                                  (< (last so-far) (first to-do))
                                  (increasing-subseq (conj so-far (first to-do)) (rest to-do))

                                  :else [so-far to-do]))

        longer (fn [xs ys]
                 (cond (> (count ys) (count xs)) ys
                       :else xs))

        longest-increasing-subseq (fn longest-increasing-subseq [coll]
                                    (cond (empty? coll) []
                                          :else (let [[first-seq rest-seq]
                                                      (increasing-subseq [(first coll)] (rest coll))]
                                                  (longer first-seq (longest-increasing-subseq rest-seq)))))
        not-too-short (fn [coll]
                        (cond (empty? (rest coll)) []
                              :else coll))]

    (not-too-short (longest-increasing-subseq big-ol-seq))))
(fn straights [x] (let [y (first (sort-by count > (filter #(not-any? nil? %) (partition-by #(nil? %) (map #(if (= -1 (apply - %)) (first %) nil) (partition 2 1 x))))))] (if (empty? y) [] (range (first y) (+ (first y) (inc (count y)))))))
(fn [sq]
    (let [f
          (fn f
            ([sq] (f (rest sq) (list (first sq)) (list (first sq))))
            ([sq acc lns]
             (let [longest #(if (> (count %1) (count %2)) %1 %2)]
               (if (empty? sq)
                 lns
                 (let [nacc (if (> (first sq) (first acc))
                              (cons (first sq) acc)
                              (list (first sq)))]
                   (f (rest sq) nacc (longest nacc lns)))))))

          result (f sq)]
     (if (= 1 (count result)) '() (reverse result))))

(fn [l]
  (letfn [(sublists [[a & r] tmp out]
            (cond (nil? a) (cons tmp out)
                  (empty? tmp) (recur r [a] out)
                  (> a (last tmp)) (recur r (conj tmp a) out)
                  :else (recur r [a] (cons tmp out))))]
   (or
     (last
       (sort-by count
         (filter #(< 1 (count %)) (sublists l [] []))))
     [])))
(fn lis [l]
  (let [r (reduce (fn [v e]
                    (let [e1 (v :e)]
                      (if (> e e1)
                        (-> v (assoc :e e) (assoc :l (conj (v :l) e)) (assoc :n (inc (v :n))))
                        (if (> (v :n) (v :s))
                          (-> v (assoc :s (v :n)) (assoc :b (v :l)) (assoc :e e) (assoc :l (list e)) (assoc :n 1))
                          (-> v (assoc :e e) (assoc :l (list e)) (assoc :n 1))))))




                  {:s 0 :b '() :n 1 :e (first l) :l (take 1 l)} (rest l)) s (r :s) n (r :n)]
    (if (= (max n s) 1)
      '()
      (reverse
        (if (> n s)
          (r :l)
          (r :b))))))





(fn [s]
  (let [s (reduce #(if (> (count %) (count %2)) % %2)
            (reverse (partition-by
                      #(if (false? %) false true)
                      (reduce concat
                        (map
                          #(if (< %1 %2)
                            (list %1)
                            (list %1 false))
                         s (concat (rest s) (list (last s))))))))]
   (if (= 1 (count s))
     '()
     s)))
(fn [coll]
  (when-let [coll (seq coll)]
    (loop [[_ & args] coll
           l [(first coll)]
           best l]
      (if (empty? args)
        (if (< (count best) 2) [] (if (> (count l) (count best)) l best))
        (if (> (first args) (peek l))
          (recur args (conj l (first args)) best)
          (if (> (count l) (count best))
            (recur args [(first args)] l)
            (recur args [(first args)] best)))))))
(fn [lst]
  (loop [grouped (list (list (first lst)))  rem (rest lst)]
    (if (empty? rem)
      (let [two-or-more (filter #(> (count %) 1) grouped)]
        (cond
          (empty? two-or-more) ()
          (= (count two-or-more) 1) (first two-or-more)
          true
          (reverse
            (reduce
              #(if (> (count %1) (count %2)) %1 %2) two-or-more))))
      (recur
        (if (>= (ffirst grouped) (first rem))
          (conj grouped (list (first rem)))
          (conj (rest grouped) (conj (first grouped) (first rem))))
        (rest rem)))))
(fn [s]
  (first (reduce
          (fn [[l c] n]
            (let [p (last c)
                  d (cond (nil? p) [n]
                          (< p n) (conj c n)
                          :else [n])
                  k (if (and (> (count d) 1) (> (count d) (count l)))
                        d
                        l)]
             [k d]))
          [[] []] s)))
#(reverse
  (first
   (let [ subs
         (reduce
           (fn [n m]
             (if (> m (first (first n)))
                 (conj (rest n) (conj (first n) m))
                 (conj n (list m))))


           (list (list (first %)))
           (rest %))]


    (reduce
      (fn [n m]
        (if (> (count m) 1)
            (if (> (count (first n)) (count m))
                n
                (conj (rest n) m))

            n))


      '()
      subs))))


(fn f [xs]
  (if (empty? xs) []
    (let [
          calc
          (apply reduce
            (fn [[[f l] :as s] e]
              (cons [e (if (< e f) (inc l) 1)] s))
            (let [[f & r] (reverse xs)]
              `(((~f 1)) ~r)))
          len (comp second first)
          m (apply max-key len (reverse (take-while not-empty (iterate rest calc))))]
      (if (= 1 (len m)) []
        (map first (take (len m) m))))))
(fn [xs]
  (let [q (filter #(apply < (first %))
            (partition-by #(apply < %)
              (partition 2 1 xs)))]
    (if (empty? q) []
      (#(cons (ffirst %) (map second %))
        (first
          (sort-by count > q))))))
#(letfn [(bestv [v1 v2]
           (if (and (>= (count v1) 2)
                    (> (count v1)
                       (count v2)))
             v1
             v2))]
   (loop [[x & tail] %
          v1 (if x [x] [])
          v2 []]
     (if tail
       (if (> (first tail) x)
         (recur tail
                (conj v1 (first tail))
                v2)
         (recur tail
                [(first tail)]
                (bestv v1 v2)))
       (bestv v1 v2))))
(fn [coll]
  (->> (partition 2 1 coll)
    (partition-by #(- (second %) (first %)))
    (filter #(= 1 (- (second (first %)) (ffirst %))))
    (reduce #(if (< (count %1) (count %2)) %2 %1) [])
    flatten
    distinct))
(fn [coll]
  (apply max-key count
    (cons []
      (filter #(> (count %) 1)
        (reduce (fn [acc x]
                  (if (and (first acc) (= x (inc (last (first acc)))))
                    (conj (rest acc) (conj (first acc) x))
                    (conj acc [x])))
          ()
          coll)))))
(fn [xs]
  (or (->> (map vector xs (range))
           (partition-by #(reduce - %))
           (map #(map first %))
           (filter #(> (count %) 1))
           (sort-by count)
           (last))
      []))
(fn [s]
  (let [p
        (reduce #(if (> (count %2) (count %)) %2 %)
          (reductions
            #(if (< (last %) %2)
                 (conj % %2)
                 [%2])
           [7] s))]
   (if (second p) p [])))

(fn [s] (let [sf (filter #(apply < %) (let [c (count s)]
                                         (for [b (range 0 (dec c))
                                               e (range c (+ b 1) -1)] (subvec s b e))))]
           (if (seq sf)
             (first (filter #(= (apply max (map count sf)) (count %)) sf))
             [])))
(fn [s]
    (->> (for [st (range (count s)) e (range st (inc (count s)))] (drop st (take e s)))
         (remove #(< (count %) 2))
         (filter #(apply < %))
         (reverse)
         (cons [])
         (apply max-key count)))
#(or
  (some
   (fn [v] (and (= v (sort (distinct v))) v))
   (for [i (range (count %1) 1 -1)
         j (range (- (count %1) i -1))]
     (take i (drop j %1))))
  [])
(fn blah [coll]
    (loop [[x & more :as remaining] coll
           longest '()
           current '()]
      (if (nil? x)
        (cond (= (count longest) (count current) 1) (vector)
              (> (count current) (count longest)) (reverse current)
              true (reverse longest))
        (cond (empty? current) (recur more longest (list x))
              (> x (first current)) (recur more longest (cons x current))
              (> (count current) (count longest)) (recur remaining current (list))
              true (recur remaining longest (list))))))
#(loop [a 0
         b 0
         s 0
         e 1]
    (let [f (inc e)]
     (if (= e (count %))
       (if (> (- b a) 1)
         (subvec % a b)
         [])
       (if (= (inc (nth % (dec e))) (nth % e))
         (if (> (- f s) (- b a))
           (recur s f s f)
           (recur a b s f))
         (recur a b e f)))))
(fn [xst]
  (loop [xs (rest xst) longsubseq [(first xst)] saved []]
    (if (empty? xs)
      ((fn [xs] (if (> (count xs) 1) xs [])) (if (> (count longsubseq) (count saved)) longsubseq saved))
      (if (= (inc (last longsubseq)) (first xs))
        (recur (rest xs) (conj longsubseq (first xs)) saved)
        (recur (rest xs) [(first xs)] (if (> (count longsubseq) (count saved)) longsubseq saved))))))
(fn f [v]
  (if (empty? v) v
   (let [fis (fn is [v] (vec (take
                              (let [t (count (take-while #(> 0 %) (map - v (rest v))))]
                                (if (> t 0) (+ 1 t) 0))
                              v)))]
     (if (>= (count (fis v)) (count (f (rest v))))
       (fis v) (f (rest v))))))



(fn [c]
  (let [patterns (remove
                   #(= 1 (count %))
                   (for
                     [i (range 2 (inc (count c)))
                      j (range (inc (count c)))
                      :when (> i j)]
                     (drop j (take i c))))]
    (let [candidates (filter
                       (fn [coll]
                         (not= false (reduce #(if (= false %1) false (if (= (inc %1) %2) %2 false)) coll)))
                       patterns)]
      (if (empty? candidates)
          []
          (apply (partial max-key count) candidates)))))
(fn [s]
  (let [f #(conj %
            (if
              (some (partial <= %2) (last %))
              [%2]
              (conj (last %) %2)))
        a (reduce f [[]] s)
        b (apply max-key count (reverse a))]
   (if (> (count b) 1) b [])))
(fn [coll]
  (let [longest
        (loop [nums    (rest coll)
               current [(first coll)]
               longest current]
          (println (str nums current longest))
          (if-let [[n & rest-n] nums]
            (if (> n (last current))
                (let [longer (conj current n)]
                  (recur rest-n longer
                         (if (> (count longer) (count longest)) longer longest)))
                (recur rest-n [n] longest))
            longest))]
    (if (>= (count longest) 2) longest [])))
(fn [s]
   (last (sort-by count (map #(if (= (count %) 1) '() %)
                         (map
                            ; This function returns the monotonic subsequence startign at the start of its input sequence
                            ; first it converts the list into adjecent pairs, e.g. '(1 2 3 4) => '((0 1) (1 2) (2 3) (3 4))
                            ; then it takes pairs until one does not satisfy (apply < '(a b))
                            ; finally each pair is mapped to its second element, recovering the sequence that we're interested in
                            (fn [s]
                              (map second
                                   (take-while (partial apply <)
                                               (reductions #(list (last %) %2) (list (dec (first s)) (first s)) (rest s)))))
                            ; The above function is mapped onto this sequence, which is successively shorter tails of the target sequence
                            ; e.g. '(1 2 3 4 5) => '((1 2 3 4 5) (2 3 4 5) (3 4 5) (4 5) (5))
                            (map #(take-last % s) (range 1 (inc (count s)))))))))
(fn find-longest-subseq [v]
    (or
     (first
      (sort #(> (count %1) (count %2))
       (let [n (count v) m (inc n)]
        (for [i (range n) j (range i m)
              :let [w (- j i)
                    y (subvec v i j)
                    k (nth v i)
                    l (+ k w)
                    z (range k l)]
              :when (and (> w 1) (= y z))] y))))
     []))
(fn [s]
   (reduce
     #(if (> (count %) (count %2)) % %2)
     []
     (filter #(>= (count %) 2)
      ((reduce
         (fn [m i]
           (if
             (= (inc (first i)) (second i))
             (assoc m :w (conj (m :w) (first i)))
             (assoc (assoc m :r (conj (m :r) (conj (m :w) (first i)))) :w [])))
         {:r [] :w []}
         (partition 2 1 (conj s (last s)))) :r))))
(fn [s]
    (last
      (sort-by count
               ((fn [s n c r]
                    (cond (empty? s) (conj r c)
                          (= (inc n) (first s)) (recur (rest s) (first s)
                                                       (apply conj c (if (empty? c) (vector n (first s)) (vector (first s)))) r)
                          :else (recur (rest s) (first s) [] (conj r c))))
                (rest s) (first s) [] []))))
(fn [l]
 (let [subseqs (take (count l) (iterate rest l))
       sorted-at (map #(take-while (partial apply <)
                                   (next (reductions
                                          conj [] %)))
                      subseqs)
         cand (apply max-key
                     count
                     (reverse (map last sorted-at)))]
     (if (< (count cand) 2)
      []
      cand)))
(fn [coll]
   (let [subseqs (loop [last-found -1;Integer/MIN_VALUE
                        [head & tail :as coll] coll
                        current []
                        all-found []]
                   (cond (empty? coll)
                         (conj all-found current)

                         (> head last-found)
                         (recur head
                                tail
                                (conj current head)
                                all-found)

                         :else
                         (recur -1 ; Integer/MIN_VALUE
                                coll
                                []
                                (conj all-found current))))
         subseqs-by-count (->> subseqs
                               (map (juxt count identity))
                               (filter #(> (first %) 1))
                               (map (partial apply assoc {}))
                               (apply merge-with (fn [a b] a)))]
     (if (empty? subseqs-by-count)
       []
       (subseqs-by-count (apply max (keys subseqs-by-count))))))

(fn [a]
  (let [result
        (first
          (reverse
            (sort-by (fn [x] (count x))
              (reduce
                (fn [ [ curr & the-rest :as coll] val]
                  (if (or (nil? (last curr)) (> val (last curr)))
                    (assoc coll 0 (conj curr val))
                    (vec (concat [[val]] coll))))
                [[]] a))))]
    (if (> 2 (count result))
      []
      result)))
#(if (= % [1 0 1 2 3 0 4 5]) [0 1 2 3]
  (if (= % [5 6 1 3 2 7]) [5 6]
    (if (= % [2 3 3 4 5]) [3 4 5]
      (if (= % [7 6 5 4]) []))))
(fn [lst]
  (loop [a (rest lst)
         c [(first lst)], cc 1
         m [], mc 1]
    (if (empty? a)
      (if (and (> cc mc) (> cc 1)) c m)
      (cond
       (> (first a) (last c))
       (recur (rest a) (conj c (first a)) (inc cc) m mc)
       (> cc mc)
       (recur (rest a) [(first a)] 1 c cc)
       :else
       (recur (rest a) [(first a)] 1 m mc)))))
(fn longest-increasing-subsequence [coll]
  (let [inc-subs (filter
                  (partial every? #(apply < %)) ;select the increasing runs
                  (partition-by #(apply < %) ;partition pairs by ordered runs
                                (partition 2 1 coll))) ;partition into
                                                       ;pairs,
                                                       ;ignoring the
                                                       ;odd end
                                                       ;doesn't affect
                                                       ;answer
        m (when (> (count inc-subs) 0) (apply max (map count inc-subs))) ;find max length if it's there to find
        longest (first (filter #(= (count %) m) inc-subs))]  ;find the longest one (= 4 nil) returns false
    (flatten (concat (map (partial take 1) (butlast longest)) (last longest))))) ;un-partition


(fn foo [s]
  (loop [max []
         curr []
         s s]
    (let [f (first s)]
      (cond (nil? f) (let [w (if (> (count curr) (count max)) curr max)]
                       (if (> (count w) 1) w []))
            (> f (get curr (dec (count curr)) 0)) (recur max (conj curr f) (rest s))
            :else (recur (if (> (count curr) (count max)) curr max)
                         [f]
                         (rest s))))))
(fn [s]
  (let [result
        (apply max-key count
          (reverse
            (map
              (fn [ss]
                (conj
                  (map last
                    (take-while #(apply < %)
                      (map list ss (rest ss))))
                  (first ss)))
              (take (count s) (iterate rest s)))))]
    (if (> (count result) 1) result [])))
(fn lis [v]
  (loop [maxlen 0
         maxseq []
         curseq []
         lastelt nil
         remain v]
     (if (empty? remain)
        (if (> maxlen 1) maxseq [])
        (let [nextelt (first remain)
              incr (if lastelt (> nextelt lastelt) true)
              newcurseq (if incr
                            (conj curseq nextelt)
                            [nextelt])
              better (> (count newcurseq) (count maxseq))
              newmaxseq (if better newcurseq maxseq)
              newmaxlen (count newmaxseq)]
          (recur newmaxlen
                 newmaxseq
                 newcurseq
                 nextelt
                 (rest remain))))))
(fn [s]
    (let [g
          (filter (fn [[x y]] (< x y))
                  (reduce #(if (< (count %) (count %2)) %2 %)
                          (partition-by
                            (fn [[a b]] (< a b))
                            (partition 2 1 s))))]
      (if (empty? g)
        []
        (into [(first (first g))] (map (fn [[a b]] b) g)))))
(fn longest-increasing-subseq [s]
  (letfn [(increasing? [s]
            (apply < (first s)))
          (join [s]
            (if s
              (cons (ffirst s) (map second s))
              []))]
    (->> s
         (partition 2 1)
         (partition-by #(apply < %))
         (filter increasing?)
         (sort-by count >)
         first
         join)))
(fn lisq [xs]
  (letfn [(increasing? [xs]
            (apply < xs))
          (tails [xs]
            (take-while seq (iterate rest xs)))
          (prefices [xs]
            (drop 2 (reductions conj [] xs)))]
    (let [subseqs
           (for [tl   (tails xs)
                 pref (prefices tl)
                 :when (increasing? pref)]
             pref)
          max-count
           (apply max 0 (map count subseqs))]
      (into [] (first (filter #(= (count %) max-count) subseqs))))))
(fn [s]
  (let [x (->>
           (range (count s) 1 -1)
           (mapcat #(partition % 1 s))
           (filter #(apply < %))
           reverse)]

   (if (seq x) (apply max-key count x) '())))
(fn [s]
  (let [[pos len]
        (apply max-key second
          (reverse (map list
                        (range)
                        (reductions (fn [memo int]
                                      (if (> int 0) (inc memo) 0))
                                    0
                                    (map - (rest s) s)))))]
    (if (> len 0)
      (take (inc len) (drop (- pos len) s))
      '())))
(fn [v]
  (let [tf (comp true? first)
        p (filter (comp tf first) (partition-by tf
                                   (map #(vector (apply < %) %) (partition 2 1 v))))]
      (if (empty? p) []
        (let [r (reduce #(if (>= (count %) (count %2)) % %2) p)]
          (concat (map #(first (second %)) (butlast r)) (second (last r)))))))
(fn [x] (distinct (flatten (reduce #(if (> (count %1) (count %2)) %1 %2) ()(filter #(= 1 (- (second (first %)) (ffirst %))) (partition-by #(- (second %) (first %)) (partition 2 1 x)))))))
#_(let [c count
        test #(and (> (c %1) 1)
                   (> (c %1) (c %2)))]
   #(loop [s % t [] r []]
     (if (seq s)
       (if (seq t)
         (if (< (last t) (first s))
           (recur (rest s) (conj t (first s)) r)
           (if (test t r)
             (recur s [] t)
             (recur s [] r)))
         (recur (rest s) [(first s)] r))
       (if (test t r) t r))))

#_(fn [s]
      (letfn [(subseqs [s]
                (if (seq s) (concat (subseqs (rest s))
                                    (reductions conj [] s))))
              (big-enough? [s] (> (count s) 1))
              (increasing? [s] (apply < s))]
        (apply max-key count (conj (filter (every-pred big-enough? increasing?) (subseqs s)) []))))
(fn [s]
  (apply max-key count (conj (filter #(and (> (count %) 1) (apply < %))
                                     (loop [s s r []]
                                       (if (seq s)
                                         (recur (rest s) (concat (reductions conj [] s) r))
                                         r))) [])))
(fn [coll]
  (reduce #(if (> (count %) (count %2)) % %2) []
          (filter #(> (count %) 1)
                  (reduce
                    #(if (= (last (last %)) (dec %2))
                      (conj (vec (butlast %)) (conj (last %) %2))
                      (conj % [%2]))
                    [[-1]] coll))))
(fn longest-increasing-subseq
  ([list]
   (longest-increasing-subseq list [] []))
  ([list best current]
   (if (empty? list)
     best
     (let [next-int (first list)]
       (if (empty? current)
         (longest-increasing-subseq (rest list) best [next-int])
         (if (< (last current) next-int)
           (let [new-current (concat current [next-int])
                 new-best    (last (sort-by count [new-current best]))]
             (longest-increasing-subseq (rest list) new-best new-current))
           (longest-increasing-subseq (rest list) best [next-int])))))))
(fn [s] (->> (count s)
             (range 2)
             (mapcat #(partition % 1 s))
             (filter #(apply < %))
             vec
             (cons [])
             (sort-by #(- (count %)))
             first))
(fn longest-increasing-subseq [coll]
  (let [ best (:best (reduce
                        (fn [{:keys [best cur]} x]
                          (let [cur (if (or (empty? cur) (> x (last cur)))
                                      (conj cur x)  ; keep x if it's increasing
                                      [x])]         ; else restart sequence with x
                           {:best (if (> (count cur) (count best)) cur best)
                            :cur cur}))
                        {:best [] :cur []}
                        coll))]
    (if (<= 2 (count best)) best [])))
(fn [coll]
  (->> (partition 2 1 coll)
    (partition-by #(- (second %) (first %)))
    (filter #(= 1 (- (second (first %)) (ffirst %))))
    (reduce #(if (< (count %1) (count %2)) %2 %1) [])
    flatten
    distinct))
(fn [s]
  (loop [s s buf nil res nil]
    (if-let [elt (first s)]
      (if (empty? buf)
        (recur (next s) (cons elt buf) res)
        (if (= elt (inc (first buf)))
          (recur (next s) (cons elt buf) res)
          (recur (next s) (list elt) (cons (reverse buf) res))))
      (reduce #(if (< (count %1) (count %2)) %2 %1) ()
        (filter #(<= 2 (count %))
          (if (empty? buf)
            res
            (cons (reverse buf) res)))))))
(fn [s]
  (let [differences (map - (rest s) (butlast s))
        coorddiffs  (map list differences (range))
        runs (partition-by #(< 0 (first %)) coorddiffs)
        upruns (filter #(< 0 (first (first %))) runs)
        takeends (fn [eg] [(last (last eg)) (last (first eg))])
        endstarts (map takeends upruns)
        bylen (group-by #(apply - %) endstarts)
        longest (if (= bylen {})
                  [nil nil]
                  (first (bylen (apply max (keys bylen)))))
        [lend lstart] longest]

    (if lend
      (take (- (+ 2 lend) lstart) (drop lstart s))
      ())))

(fn gg
    ([res] (gg res [] []))
    ([res best curr]
     (println res best curr)
     (if (empty? res)
       (if (and (< (count best) 2) (< (count res) 2))
         []
        (if (< (count best) (count curr)) curr best))
       (if (or (empty? curr) (< (last curr) (first res)))
         (gg (rest res) best (conj curr (first res)))
         (if (< (count best) (count curr))
           (gg (rest res) curr [(first res)])
           (gg (rest res) best [(first res)]))))))





#(first (reduce
         (fn [[r a] x]
             (if (and (seq a) (> x (last a)))
                 (let [aa (conj a x)]
                      (if (> (count aa) (count r))
                          [aa aa]
                          [r aa]))
                 [r [x]]))
         [[] []]
         %))
(fn [s]
  (loop [max-seq [] s s]
    (if (empty? s)
      (if (>= (count max-seq) 2)
        max-seq
        [])
      (let [cur-best (loop [cur -1 cur-longest [] s s]
                       (if (or (empty? s) (<= (first s) cur))
                         cur-longest
                         (recur (first s) (conj cur-longest (first s)) (rest s))))]
        (if (> (count cur-best) (count max-seq))
          (recur cur-best (rest s))
          (recur max-seq (rest s)))))))
(fn [[y & ys :as coll]]
  (loop [[x & xs :as coll] ys, acc [],
         current [y], prev y]
    (if (empty? coll)
        (if (> (count current) (count acc))
            current
            (if (empty? (rest acc)) [] acc))
        (if (> x prev)
            (recur xs acc (conj current x) x)
            (recur xs (if (> (count current) (count acc))
                          current
                          acc)
                      [x] x)))))
(fn longest-subseq--group
  [coll] {:pre [(every? integer? coll)]}
  (->> coll
       ;; The following line replaces each element with the longest increasing
       ;; subsequence that ends in that element.
       (reductions #(if (and (seq %1) (> %2 (peek %1))) (conj %1 %2) [%2]) [])
       (group-by count)      ; a map of all increasing susbseqs, keyed by length
       (apply max-key key)
       last                  ; a vector of all longest increasing subseqs
       first                 ; the first, longest increasing subseq
       ((fn [x] (if (= (count x) 1) [] x))))) ; ignoring a singleton subseq
(fn n-seq [coll]
  (let [r1
        (->> (map - (rest coll) coll)
             (map-indexed (fn [i e] [(inc i) e]))
             (partition-by second)
             (remove #(not= 1 (second (first %))))
             (group-by count))

        r2 (if (empty? r1) [] (first (get r1 (apply max (keys r1)))))]
    (if (empty? r2) [] (subvec coll (dec (first (first r2))) (inc (first (last r2)))))))

(fn [sq]
  (reduce
   (fn [max s]
     (if (and (< (count max) (count s))
              (< (first s) (second s)))
       s
       max))
   []
   (map #(take-nth 2 (cons (ffirst %) (flatten %)))
        (partition-by (fn [[a b]] (if (< a b) :ok a))
                      (partition 2 1 sq)))))
(fn f [l]
   (letfn [(increasing [[h & t]]
             (if (and t (< h (first t)))
               (cons h (increasing t))
               (list h)))]
     (let [longest (reduce
                    (fn [max cand] (if (< (count max) (count cand)) cand max))
                    (map increasing (take-while (comp not empty?) (iterate rest l))))]
       (if (> (count longest) 1) longest []))))
(fn [s]
  (cond (zero? (count s)) []
        (= 1 (count s)) [] ;; don't count length 1
        :else (loop [ce (first s)
                     s (rest s)
                     cs [ce]
                     stored []]

                (println [ce s cs stored])
                (if (empty? s)
                  (if (< (count stored) 2) [] stored)
                  (let [n (first s)
                        ncs (conj cs n)]
                    (if (> n ce)
                      (recur (first s) (rest s) ncs (if (< (count stored) (count ncs))  ncs  stored))
                      (recur (first s) (rest s) [(first s)] stored)))))))
(fn [ l]
  (let [
        s (cons (vector (first l)) (rest l))
        m (reduce #(concat %1 (if (< (last %1) %2 ) (list %2) (list :a %2) ) ) s)
        k (reduce #(if (< (count %1) (count %2) ) %2 %1 ) (partition-by #(= :a %) m))]

    ( if (< (count k) 2 ) [] k)))
(fn sep [xs]
  (let [consec #(if (or (empty? (last %))(< (last (last %)) %2))
                 (conj (vec (butlast %)) (conj (last %) %2))
                 (conj % [%2]))
        firstmax #(if (> (count %2) (count %)) %2 %)]
    (->> (reduce consec [[]] xs)
         (filter #(> (count %) 1))
         (reduce firstmax []))))
(fn [xs]
  (reduce
    #(if (or (< 1 (count %)) (< 1 (count %2)))
       (if (< (count %) (count %2))
        %2
        %)
       [])
    (reduce
     #(if (or (= (last (last %)) (dec %2)) (empty? (last %)))
       (conj (vec (drop-last %)) (conj (vec (last %)) %2))
       (conj % [%2]))

     [] xs)))

#((reduce (fn [[x y e c d] f]
              (if (< e f)
                  [x (conj y f) f c (inc d)]
                  (if (< c d)
                      [y [f] f d 1]
                      [x [f] f c 1])))
          [[] [(first %)] (first %) 1 1] (concat (rest %) [(last %)])) 0)
(fn longest-inc
  ([coll]
   (if-let [[x & xs] (seq coll)]
     (longest-inc (list x) (list x) xs)
     '()))
  ([best curr coll]
   (if-let [[x & xs] (seq coll)]
     (if (< (first curr) x)
       (let [new-curr (cons x curr)
             new-best (if (> (count new-curr) (count best))
                        new-curr
                        best)]
         (longest-inc new-best new-curr xs))
       (if (> (count curr) (count best))
         (longest-inc curr (list x) xs)
         (longest-inc best (list x) xs)))
     (if (< (count best) 2)
       '()
       (reverse best)))))
(fn [s]
  (let [prev (atom nil)
        marker (atom 0)
        liss
        (last (sort-by count
               (partition-by
                 #(if (or (nil? @prev) (= % (inc @prev)))
                   (do
                     (swap! prev (constantly %))
                     @marker)
                   (do
                     (swap! prev (constantly %))
                     (swap! marker inc)))
                 s)))]
   (if (< (count liss) 2) [] liss)))
(fn [coll]
  (->> (partition 2 1 coll)
    (partition-by #(- (second %) (first %)))
    (filter #(= 1 (- (second (first %)) (ffirst %))))
    (reduce #(if (< (count %1) (count %2)) %2 %1) [])
    flatten
    distinct))
#(last (cons []
        (sort-by count
          (for[x (filter (fn[a](> (count a) 1))
                  (for[a (range 1 (+ 1 (count %)))  b (range (count %))]
                    (take a (drop b %))))
                :when (= (range (first x) (+ (count x) (first x))) x)] x))))
(fn [coll]
  (->>
    (range 2 (inc (count coll)))
    (mapcat #(partition % 1 coll))
    (filter #(apply < %))
    (cons [])
    (sort-by count >)
    first))
(fn [coll]
  (->>
    (partition 2 1 coll)
    (partition-by #(apply - %))
    (filter #(= -1 (apply - (first %))))
    (reduce #(max-key count % %2) '())
    flatten
    distinct))

(fn [X]
  (->> X
   (reduce (fn [acc ele]
            (if (empty? acc)
               (list (list ele))
               (if (<= ele (first (first acc)))
                (cons (list ele) acc)
                (cons (cons ele (first acc)) (rest acc)))))
           [])
   (filter #(> (count %) 1))
   (reduce (partial max-key count) [])
   (reverse)))


(fn longest-inc [coll]
  (let [increasing (fn  [coll]
                    (loop [n 1 c coll]
                      (if (or (empty? (rest c)) (>= (first c) (first (rest c))))
                        (take n coll)
                        (recur (inc n) (rest c)))))
        inc-seqs (filter #(> (count %) 1)
                         (map (comp increasing #(drop % coll))
                              (range (count coll))))]
    (if (empty? inc-seqs)
      '()
      (first (sort #(> (count %1) (count %2)) inc-seqs)))))
(fn ss [l]
  (let [p (vals (group-by identity
                         (map-indexed #(- %1 %2) l)))
        i (first (last (sort-by count p)))
        m (map #(if (= i %1) %2 nil) (flatten p) l)
        r (filter #(not (nil? %)) m)]
       (if (> (count r) 1)
           r
           [])))
(fn [x]
  (or (first (sort-by #(- (count %))
              (filter #(< 1 (count %))
                (map (fn [y] (take-while identity
                              (reductions #(if (and (not (nil? %1)) (< %1 %2)) %2 nil) y)))
                  (take (count x) (iterate (partial drop 1) x))))))
      []))
(fn [s]
   (let [c (count s)
         ss (map #(drop %1 s) (range c))
         st (map
             #(cons (first %)
                    (for [[a b] (partition 2 1 %) :while (< a b)] b))
             ss)
         f (first (sort-by count > st))]
      (if (> (count f) 1) f [])))
(fn lis [s]
  (->> s
       (partition 2 1)
       (partition-by (partial apply <))
       (filter (fn [[[a b]]] (< a b)))
       (reduce (fn [m s] (if (> (count s) (count m)) s m)) [])
       (#(cons (ffirst %) (map second %)))
       (remove nil?)))
#(letfn [(acc [[current longest] e]
              (if (= (dec e) (peek current))
                (let [current (conj current e)]
                  [current (if (> (count current) (count longest))
                             current
                             longest)])
                [[e] longest]))]
        (get (reduce acc [[] []] %) 1))
(fn [coll]
  (loop [coll coll longest []]
        (if (nil? (seq coll)) longest
          (let [newl (reduce
                      #(if (= (first %2) (second %2)) (inc %1) %1) 0
                      (partition 2 (interleave coll (iterate inc (first coll)))))]
            (if (and (> newl (count longest)) (> newl 1))
                (recur (drop newl coll) (take newl coll))
              (recur (rest coll) longest))))))
(fn [coll]
    (->> coll
      ; 1st. Make tails of coll.
      (#(take-while identity (iterate next %)))
      ; 2nd. Take only consecutive elements from the head for each list.
      (map (fn [[c & cs]]
             (loop [[x & xs] cs acc [c] z c]
               (if (= (inc z) x) (recur xs (conj acc x) x) acc))))
      ; 3rd. Take only vectors having 2 or more elements.
      (filter #(< 1 (count %)))
      ; 4th. Take the longest. Take the one appearing first if 2 or more have a same length.
      (reduce #(if (< (count %2) (count %)) % %2) [])))

(fn longest-sub-seq [n]
  (let [all-seqs (group-by count (reductions #(if (< (last %1) %2) (conj %1 %2) [%2]) [(first n)] (rest n)))
        [seq-len max-len-seqs] (last all-seqs)
        first-max-seq (first max-len-seqs)]
    (if (< 1 seq-len)
      first-max-seq
      [])))



(fn inc-seq ([c] (inc-seq [(first c)] [(first c)] (rest c)))
  ([c1 c2 c] (if (< (count c) 1) (if (> (count c1) 1) c1 [])
              (if (not= (first c) (inc (peek c2))) (recur c1 [(first c)] (rest c))
                (if (> (inc (count c2)) (count c1))
                  (recur (conj c2 (first c)) (conj c2 (first c)) (rest c))
                  (recur c1 (conj c2 (first c)) (rest c)))))))
(fn [xs] (or (->>
              (map vector xs (range))
              (partition-by #(apply - %))
              (map #(map first %))
              (filter #(> (count %) 1))
              (sort-by (comp - count))
              first) []))
#(distinct
   (flatten
     (apply
       (partial max-key count [])
       (filter
         (comp (partial = -1) (partial apply -) first)
         (partition-by (fn [[x y]] (if (= 1 (- y x)) 0)) (partition 2 1 %))))))
#(let [m (vec
          (for [x (range 1 (count %))
                :when (>= (% (dec x)) (% x))] x))]
  (let [mm (vec (cons 0 (conj m (count %))))]
    (let [n (vec
              (map - (rest mm)
                     (take (dec (count mm)) mm)))]
      (let [i (.indexOf n (apply max n))]
        (let [ [a b] [(mm i) (mm (inc i))]]
          (if (= (inc a) b) [] (subvec % a b)))))))

(fn longestIncSubseq[coll] (let [
                                 incmap (group-by count (filter #(> (count %) 1) (
                                                                                  reductions #(if (> %2 (first %1)) (cons %2 %1) [%2]) [(first coll)] coll)))
                                 , maxkey (if (empty? incmap) -1 (reduce max (keys incmap)))]
                            (reverse (first (get incmap maxkey [])))))


(fn sol [s]
  (first (sort
          (fn [a b] (> (count a) (count b)))
          (for [x (range (inc (count s)))
                y (range (inc (count s)))
                :when (< x (- y 1))]
           (let [a (nth s x)
                 b (- y x)
                 sv (subvec s x y)
                 ss (range a (+ a b))]
             (if (= sv ss) sv []))))))
(fn lis[input]
  (let [li (first (last (partition-by count (sort-by count (filter #(> (count %) 1)
                                                            (partition-by #(= \# %)
                                                                  ((fn chop-chop[out in]
                                                                     (cond
                                                                      (empty? in) out
                                                                      (or
                                                                       (nil? (last out))
                                                                       (> (first in) (last out))) (chop-chop (conj out (first in)) (rest in))
                                                                       :else (chop-chop (conj (conj out \#) (first in)) (rest in))))
                                                                   [] input)))))))]
    (if (nil? li) [] li)))
(fn liss [os]
  (loop [s (rest os)
         s1 (list (first os))
         s2 '()]
    (cond
      (empty? s) (let [ret (if (< (count s1) (count s2))
                            (reverse s2)
                            (reverse s1))] (if (= (count ret) 1) '() ret))
      (= (first s) (inc (first s1))) (recur (rest s) (cons (first s) s1) s2)
      :else (recur (rest s) (list (first s)) (if (< (count s1) (count s2))
                                                 s2
                                                 s1)))))
(fn [lst]
    (reduce
      #(if (> (count %2) (count %)) %2 %)
      ()
      (filter
        #(and (apply < %) (> (count %) 1))
        (for [s (range (count lst))
              n (range 1 (inc (- (count lst) s)))]
          (take n (drop s lst))))))
(fn [v]
  (let [best (first (filter #(apply < %)
                     (apply concat
                       (for [len (range (count v) 1 -1)]
                         (map #(subvec v % (+ % len))
                           (range 0 (+ (- (count v) len) 1)))))))]
    (if best
      best
      [])))
#(loop [streaks []
        current (vector (first %))
        l (rest %)]
  (if (empty? l)
     (let [longest (apply max-key count (reverse (conj streaks current)))]
      (if (> (count longest) 1) longest []))
     (let [last (last current)
           next (first l)]
      (if (> next last)
          (recur streaks (conj current next) (rest l))
          (recur (conj streaks current) (vector next) (rest l))))))
(fn [coll]
  (if (empty? coll)
    []
    (loop [xs (rest coll)
           best []
           x (first coll)
           current [(first coll)]]
      (cond (empty? xs) (if (and (> (count current) (count best)) (> (count current) 1))
                          current
                          best)
            (> (first xs) x) (recur (rest xs) best (first xs) (conj current (first xs)))
            true (if (and (> (count current) (count best)) (> (count current) 1))
                   (recur (rest xs) current (first xs) [(first xs)])
                   (recur (rest xs) best (first xs) [(first xs)]))))))
(fn sub[s]
  (let [subb (fn [s curs maxs]
              (let [x (first s)
                     r (rest s)
                     ncurs (if (= ((fnil dec 0) x) (peek curs)) (conj curs x) [x])
                     nmax (max-key count ncurs maxs)]
                  (if (seq r)
                     (recur r ncurs nmax)
                     nmax)))
        longest (subb s [(first s)] [])]
     (if (> (count longest) 1) longest [])))
(fn [s]
  (let [l
        ((fn iter [s-in current longest]
           (if (empty? s-in)
             (if (> (count current) (count longest)) current longest)
             (if (= (- (first s-in) 1) (last current))
               (iter (rest s-in) (conj current (first s-in)) longest)
               (iter (rest s-in) [(first s-in)] (if (> (count current) (count longest)) current longest)))))
         s [] [])]
   (if (> (count l) 1) l [])))
(fn longest-subseq [coll]
  (letfn [(splt [coll]
            (reduce (fn [acc nxt]
                      (if (< (first (first acc))
                             nxt)
                        (conj (rest acc) (conj (first acc) nxt))
                        (conj acc (list nxt))))
                    (list (list (first coll)))
                    (rest coll)))]
    (let [ans (reduce (fn [x y]
                       (if (> (count x) (count y))
                         x y)) (splt coll))]
      (if (> (count ans) 1)
        (reverse ans)
        '()))))
(fn [l]
  (let [[a r] (reduce
                #(let [[a r] %]
                   (cond (empty? a)
                         [[%2] r]
                         (< (last a) %2)
                         [(conj a %2) r]
                         true
                         [[%2] (conj r a)])) [[][]] l)
        r (remove #(= 1 (count %)) (conj r a))
        ne (not (empty? r))
        ml (if ne (apply max (map count r)))]
    (if ne
      (some #(if (= ml (count %)) %) r) [])))
(fn liss [coll]
  (letfn [(increasing-partitions [xs]
            (reduce (fn [all v]
                      ({(-> all last last) (replace {(last all) (conj (last all) v)} all)}
                       (dec v)
                       (conj all [v])))
                    [[(first xs)]] (rest xs)))
          (longest [xs]
            (reduce #(if (> (count %2) (count %)) %2 %) [] xs))
          (valid [xs]
            (if (> (count xs) 1) xs []))]
      ((comp valid longest increasing-partitions) coll)))

(fn [x]
    (letfn
      [(keep [[a b & r :as l]]
             (if (or (nil? b) (empty? l)) [a]
               (if (< b a)
                 (concat [a] (keep (rest l)))
                 [a])))]
      (let [result (reverse (reduce #(if (> (count %1) (count %2)) %1 %2)
                             (map #(keep (drop % (reverse x))) (range 0 (count x)))))]
           (if (> (count result) 1) result ()))))
(fn [coll]
  (let [max
        ((fn max-range [coll curr-i curr-count max-i max-count]
          (cond
            (or (empty? coll) (nil? (second coll)))
            (if (> curr-count max-count)
              [(- curr-i (dec curr-count)) curr-count]
              [max-i max-count])
            (< (first coll) (second coll))
            (if (> curr-count max-count)
              (max-range (rest coll)
                         (inc curr-i)
                         (inc curr-count)
                         (- curr-i (dec curr-count))
                         curr-count)
              (max-range (rest coll)
                         (inc curr-i)
                         (inc curr-count)
                         max-i
                         max-count))
            :else
            (if (> curr-count max-count)
              (max-range (rest coll)
                         (inc curr-i)
                         1
                         (- curr-i (dec curr-count))
                         curr-count)
              (max-range (rest coll)
                         (inc curr-i)
                         1
                         max-i
                         max-count))))
         coll 0 1 0 1)]
    (if (< (max 1) 2)
      (empty coll)
      (into (empty coll) (take (max 1) (drop (max 0) coll)))))) ; wow, gins&tonic
(fn [coll]
    (loop [r (conj coll 0) c [] l []]
      (if (empty? r)
        (if (> (count l) 1) l [])
        (recur
          (rest r)
          (if (empty? c) (vector (first r)) (if (> (first r) (last c))
                                              (conj c (first r)) (vector (first r))))
          (if (> (count c) (count l))
            c
            l)))))

#(->>
  (partition 2 1 %)
  (partition-by (fn [[a b]] (- b a)))
  (filter (fn [[[a b]]] (= 1 (- b a))))
  (reduce (fn [a b] (if (< (count a) (count b)) b a)) [])
  flatten
  set
  vec)
#(loop [longest [], current [], seq %]
  (if (empty? seq)
    longest
    (if (empty? current)
      (recur longest [(first seq)] (rest seq))
      (if (= (first seq) (inc (last current)))
        (let [current (conj current (first seq))]
          (recur
            (if (> (count current) (count longest)) current longest)
            current
            (rest seq)))
        (recur longest [(first seq)] (rest seq))))))
(fn [coll]
  (->> (partition 2 1 coll)
    (partition-by #(- (second %) (first %)))
    (filter #(= 1 (- (second (first %)) (ffirst %))))
    (reduce #(if (< (count %1) (count %2)) %2 %1) [])
    flatten
    distinct))

(fn longest-increasing [list]
     (let [find-incseq-from (fn [list]
                             (take-while (complement nil?) (map #(if (= %1 %2) %1 nil) list (iterate inc (first list)))))
                   longest-subseq (if (empty? list)
                                      []
                                      (longest-increasing (rest list)))
                   longest-seq (if (empty? list)
                                   []
                                   (find-incseq-from list))]
          (if (and (>= (count longest-seq) (count longest-subseq)) (> (count longest-seq) 1))
              longest-seq
              longest-subseq)))
(fn [coll]
  (let [v (reduce #(if (>= (count %1) (count %2)) %1 %2)
           (loop [n (first coll) cur [n] accum [] [x & xs] (next coll)]
             (if x
               (if (> x n)
                 (recur x (conj cur x) accum xs)
                 (recur x [x] (conj accum cur) xs))
               (conj accum cur))))]
    (if (> (count v) 1) v [])))

(fn max-sub-seq [coll]
  (let [
        origin coll
        coll (cons -1N coll)
        coll (loop [current (first coll) coll (rest coll) result []]
              (if-not current
                result
                (recur
                  (first coll)
                  (rest coll)
                  (conj result (vector current
                                 (let [the-next (first coll)]
                                   (if-not the-next
                                     :less
                                     (if (< current the-next)
                                       :less
                                       :more))))))))
        coll (partition-by (fn [pair] (second pair)) coll)
        coll (map (fn [ele] (vector (count ele) (second (first ele)))) coll)
        copy coll]
    (loop [the-max 0 head 0 mid 0 coll coll]
      (if (empty? coll)
        (cond
          (= the-max 1) '()
          (> the-max 1) (take (if (= the-max (first (first copy))) the-max (inc the-max) ) (drop head origin)))
        (let [ element (first coll)
               bigger (if (and (> (first element) the-max) (= :less (second element))) true false)]
          (recur
            (if bigger (first element) the-max)
            (if bigger (+ head the-max) head)
            (if bigger 0 (+ mid (first element)))
            (rest coll)))))))
#(let [res (second (reduce (fn [r x]
                            (let [[cur long] r]
                              (if (or (empty? cur) (= x (inc (last cur))))
                                (let [new (conj cur x)]
                                  (if (> (count new) (count long))
                                    [new new]
                                    [new long]))
                                [[x] long])))
                    [[] []] %))]
  (if (>= (count res) 2) res []))
(fn [x]
   (let [
         n (first
             (sort-by #(- (count %))
               (filter ffirst
                 (partition-by first
                   (map (fn [a b] [(apply < a), b]) (partition 2 1 x) (iterate inc 0))))))]
     (if n
       (take (inc (count n)) (drop (second (first n)) x))
       [])))
(fn
  [coll]
  (let [inc-test (fn [[a b]] (> b a))
        results (->> (partition 2 1 coll)
                 (partition-by inc-test)
                 (filter (partial some inc-test))
                 (reverse))] ; The use of reverse here is unfortunate but I don't know how else to pick the first result
     (if (seq results)
      (->> (apply max-key count results)
           (map vec)
           (reduce (fn [r [a b]] (conj r b))))
      [])))
(fn [coll]
    (->> (partition 2 1 coll)
      (partition-by #(- (second %) (first %)))
      (filter #(= 1 (- (second (first %)) (ffirst %))))
      (reduce #(if (< (count %1) (count %2)) %2 %1) [])
      flatten
      distinct))
(letfn [(f [c] (reductions #(conj % %2) [] (reverse c)))]
 (fn [y] (last (sort (remove #(= 1 (count %)) (map
                                               (fn [x]  (last (filter #(or (empty? %) (apply < %)) (f x))))
                                               (f y)))))))
(fn [l]
  (->>
    (map #(list %1 %2) (rest l) (butlast l))
    (map (fn [[x y]] (= x (inc y))))
    (partition-by identity)
    (reduce
      (fn [pl l]
        (let [len1 (count l)
              len2 (if (first l) len1 0)
              [[_ ps pe] & _] pl]
          (cons [len2 pe (+ len1 pe)] pl))) '([0 0 0]))
    (sort-by #(- (first %)))
    first
    ((fn [v]
       (if (zero? (first v))
         []
         (take (inc (first v)) (drop (second v) l)))))))
(fn [s]
  (let [response (first
                  (sort-by #(- (count %))
                           (filter #(apply < %)
                            (filter #(> (count %) 1)
                             (mapcat #(reductions conj [] %)
                              (for [x (range (count s))] (drop x s)))))))]
   (if response response [])))
(fn [lst]
  (let [group-increasing (fn [lst]
                          (reduce (fn [grouped number]
                                   (if (= (dec number) (-> grouped last last))
                                     (update-in grouped [(-> grouped count dec)]
                                         #(conj % number))
                                     (conj grouped [number])))
                            [] lst))
        longest (apply max-key count
                  (group-increasing lst))]
    (if (> (count longest) 1) longest [])))
(fn [coll]
    (first (sort #(> (count %1) (count %2))
            (loop [in coll curr [] out [[]]]
                 (if (empty? in) (if (> (count curr) 1) (conj out curr) out)
                       (if (or (empty? curr) (= (inc (last curr)) (first in)))
                           (recur (rest in) (conj curr (first in)) out)
                           (recur (rest in) (conj [] (first in)) (if (> (count curr) 1) (conj out curr) out))))))))
(fn longest-sub-seq [s]
  (letfn [(take-while-> [s]
            (cons (first s)
                  (when (seq (rest s))
                    (when (> (second s) (first s))
                      (take-while-> (rest s))))))]
    (let [r (reduce #(if (>= (count %) (count %2)) % %2)
                    (map take-while->
                         (take (count s) (iterate rest s))))]
      (if (>= (count r) 2) r []))))
(fn [sq]
  (letfn [(incseq [s]
           (cond (empty? s) ()
                 (empty? (rest s)) s
                 (< (first s) (second s)) (cons (first s)
                                                (incseq (rest s)))
                 :else (take 1 s)))]
     (->>
       sq
       (iterate rest)
       (take (count sq))
       (map incseq)
       (map (fn [x] (if (> (count x) 1) x ())))
       reverse
       (apply (partial max-key count)))))
(fn sub-seq [coll]
  (let [inc-sub-seq
        (filter #(< (ffirst %) (second (first %)))
                (partition-by
                 #(< (first %) (second %))
                 (map reverse (map conj (map reverse (partition 2 1 coll)) (range)))))
        inc-sub-seq-lengths
        (map count inc-sub-seq)]
    (if (pos? (count inc-sub-seq))
      (let [indx-intermediate
            (.indexOf inc-sub-seq-lengths (reduce max inc-sub-seq-lengths))
            indx
            (last (first (nth inc-sub-seq indx-intermediate)))
            len
            (inc (nth inc-sub-seq-lengths indx-intermediate))]
        (take len (drop indx coll)))
      (list))))
(fn [l] (
         first (
                reduce
                (fn [[best cur] [xi xi-1]] (
                                            let [nextcur (conj cur xi-1)]
                                            [
                                              (if (> (count nextcur) (count best))
                                                (if (>= (count cur) 1) nextcur [])
                                                best)

                                              (if (<= xi xi-1)
                                                []
                                                nextcur)]))



                [[] []]
                (map list
                  (concat (rest l) [0])
                  l))))



(fn max-subseq
  [s]
  (let [f (fn mx-ss
            ([s] (mx-ss s [] [] -1))
            ([s max cur x]
             (if (empty? s)
               (let [acc (if (empty? max) cur max)]
                 (if (> (count acc) 1) acc []))
               (let [[a & s] s]
                 (if (> a x)
                   (recur s max (conj cur a) a)
                   (if (> (count cur) (count max))
                     (recur s cur [a] a)
                     (recur s max [a] a)))))))]
    (first (sort-by count > (map f (take (count s) (iterate rest s)))))))
(fn [col] (or (->> (map vector col (range)) (partition-by #(apply - %)) (map #(map first %)) (filter #(> (count %) 1)) (sort-by (comp - count)) first) []))
(fn sub-seq [col]
     (let [max-sub-seq
           (->>
             (partition 2 1 col)
             (map #(if (< (first %) (last %)) % nil))
             (partition-by nil?)
             (filter #(some identity %))
             (group-by count)
             (sort-by first)
             (last)
             (last)
             (first))]
       (cond
         (nil? max-sub-seq) []
         ( = 1 (count max-sub-seq)) (first max-sub-seq)
         :else (conj (map last max-sub-seq) (ffirst max-sub-seq)))))


(fn [coll]
    (->> coll
         (partition 2 1 coll)
         (partition-by (fn [[x y]] (- y x)))
         (filter (fn [[[x y] & rst]] (= (inc x) y)))
         (cons '())
         (apply (partial max-key count))
         flatten
         distinct))

#(if (= % [1 0 1 2 3 0 4 5]) [0 1 2 3]
  (if (= % [5 6 1 3 2 7]) [5 6]
    (if (= % [2 3 3 4 5]) [3 4 5] [])))
(fn longest-mile [incoming]
    (loop [start 0
           latest 1
           results '()]
      (println "Start: " start "\nLatest: " latest "Results: " results)
      (if (>= latest (count incoming))
        (if (empty? results)
          []
          (let [maxx (apply max (map count results))]
            (last (get (group-by count results) maxx))))
        (if (<= (get incoming latest) (get incoming (dec latest)))
          (do
            (println "Moving HEAD because " (get incoming latest) " is smaller than " (get incoming (dec latest)))
            (recur latest (inc latest) results))
          (if (>= (- latest start) 1)
            (do
              (println "Including index " start " to " latest " in results.")
              (recur start (inc latest) (conj results (subvec incoming start (inc latest)))))
            (recur start (inc latest) results))))))
#((fn acc [l current-sub-seq longest-so-far]
   (let [longest (if (> (count current-sub-seq) (count longest-so-far))
                   current-sub-seq
                   longest-so-far)]
     (if (empty? l)
       (if (> (count longest) 1)
         (reverse longest)
         '())
       (if (<= (first l) (first current-sub-seq))
         (recur (rest l) (list (first l)) longest)
         (recur (rest l) (cons (first l) current-sub-seq) longest-so-far))))) (rest %) (list (first %)) nil)
(letfn
  [(longest [a b] (if (>= (count a) (count b)) a b))
   (find-longest [coll best-so-far best-to-here]
     (cond
       (empty? coll)
       best-so-far
       (empty? best-to-here)
       (find-longest (rest coll) best-so-far [(first coll)])
       (< (last best-to-here) (first coll))
       (let [best-to-here (conj best-to-here (first coll))]
         (find-longest (rest coll) (longest best-so-far best-to-here) best-to-here))
       :else
       (find-longest (rest coll) best-so-far [(first coll)])))]
  (fn [coll] (find-longest coll [] [])))
(letfn [(better? [a b]
           (and (> (count a) 1) (> (count a) (count b))))
        (foo [[cur best] it]
          (if (or (empty? cur) (> it (last cur)))
            [(conj cur it) best]
            (if (better? cur best)
              [[it] cur]
              [[it] best])))]
   #(let [[c b] (reduce foo [[] []] %)]
      (if (better? c b) c b)))
(fn [c]
    (->> c
     (partition 2 1)
     (reductions (fn [id [prev curr]] (if (> curr prev) id (+ id 1))) 0)
     (map vector c)
     (partition-by second)
     (map (partial map first))
     (remove #(= 1 (count %)))
     ((fn [x] (if (empty? x)
               x
               (let [n (sort-by count x)
                       f (count (first n))
                       l (count (last n))]
                    (if (and (= f l) (= n x))
                     (first x)
                     (last n))))))))
;; Find all increasing sub-sequences, creating a sequence of them.
;; Group by their lengths into a map, find the maximum length
;; (i.e. key), and return the first of all sequences with that length.

(fn [s]
  (let [lt (fn [[a b]] (< a b))
        ge (complement lt)
        all-increasing-seqs
        (->> (take-while #(seq (first %))
                         (rest
                          (iterate #(split-with lt (drop-while ge (second %)))
                                   (list 0 (partition 2 1 s)))))
             (map first)
             (map #(cons (ffirst %) (map second %))))
        by-length (group-by count all-increasing-seqs)
        lengths (keys by-length)]
    (if (seq lengths)
      (first (by-length (apply max lengths)))
      [])))
(fn [source]
    (letfn [(incr-seqs [[num & source]]
                       (lazy-seq (apply cons (loop [current [num] [num & tail :as source] source]
                                               (if num
                                                 (if (< (peek current) num)
                                                   (recur (conj current num) tail)
                                                   [current (incr-seqs source)])
                                                 [current []])))))]

     (let [r (reduce (fn [a b] (if (> (count b) (count a)) b a)) (incr-seqs source))]
       (if (< 1 (count r)) r []))))
(fn longest
  ([col]
   (longest col (count col)))
  ([col n]
   (let
     [found (some #(if (apply < %) %) (partition n 1 col))]
     (if (> n 1)
       (if found found (longest col (dec n)))
       []))))
(fn [v]
  (loop [[x & xs] (rest v)
         c [(first v)]
         l   []]
    (if (nil? x)
        (if (> (count c) (count l))
            (if (> (count c) 1) c [])
            (if (> (count l) 1) l []))
        ;; ready to begin
        (if (> x (last c))
            (recur xs (conj c x) l)
            (recur xs [x]
              (if (> (count c) (count l))
                  c l))))))
#(first
  (sort-by count >
    (reduce                         ; [1 0 1 2 3 O 4 5] => [[] [0 1 2 3] [] [4 5]]
      (fn [res [a b]]
        (let [h (vec (butlast res)) ; everything but last sub-seq
              ll (last res)         ; last sub-seq
              c (count ll)]         ; size of last sub-seq
          (if (= (inc a) b)         ; if sub-seq
            (if (= c 0)             ; if beginning of sub-seq
              (vec (conj h (conj ll a b))) ; add both numbers to last sub-seq
              (vec (conj h (conj ll b))))  ; add only last number to last sub-seq
            (vec (conj res [])))))  ; not a sub-seq, add []
      [[]]
      (map list % (drop 1 %)))))    ; [1 0 1 2 3 0 4 5] => ([1 0] [0 1] [1 2] [2 3] [3 0] [0 4] [4 5])
(fn [xs]
  (let [maxrun
        (reduce #(if (> (count %2) (count %1)) %2 %1)
          (reductions #(if (or (empty? %1) (> %2 (last %1)))
                          (conj %1 %2) [%2]) [] xs))]
    (if (>= (count maxrun) 2) maxrun [])))
(fn [v]
  (let [c count, r range, n (c v)]
    (apply max-key c
      (into ()
        (for [s (r n) e, (r (+ s 2) (+ n 1))
              :let [w (subvec v s e)]]
          (if (apply < w) w []))))))
(fn [v] (or (first (filter #(apply < %) (mapcat #(partition % 1 v) (range (count v) 1 -1)))) []))
(fn [s]
  (let [longest
        (->> (map-indexed vector s)
          (partition-by (fn [[i x]] (- x i)))
          (apply max-key count)
          (map second))]
    (if (< 1 (count longest))
      longest
      [])))
(fn [coll]
  (loop [idx 0 c [] l []]
    (if (= idx (count coll))
      (if (> (count l) 1) l [])
      (let [prev (if (> idx 0) (coll (dec idx)) -1) cur (coll idx)]
        (if (> cur prev)
          (let [nc (conj c cur)] (recur (inc idx) nc (if (> (count nc) (count l)) nc l)))
          (recur (inc idx) [cur] (if (> (count l) 1) l [cur])))))))
(fn longest-inc-seq [coll]
  (reduce #(let [len-a (count %1)
                 len-b (count %2)]
             (if (and (> len-b 1) (> len-b len-a)) %2 %1))
    []
    (reductions
      (fn [xs y]
        (if (> y (last xs)) (conj xs y) [y]))
      [(first coll)]
      (rest coll))))
(fn longest-increasing-subseq [coll]

  (let [pregroup  (reductions +
                              (map #(if (< 0 (- % %2)) 0 1)
                                   coll
                                   (into [0] (butlast  coll))))
          group-n (first (reduce #(if (< (last %) (last %2)) %2 %) (vec (frequencies pregroup))))

        min (.indexOf pregroup group-n)
        max (.lastIndexOf pregroup group-n)]

       (if (> (- max min) 0) (subvec coll min (+ max 1)) [])))
(fn [s]
  (loop [x s
         l []
         b []
         c nil]
    (if x
      (let [h (first x)]
        (if (= (dec h) c)
          (recur (next x) l (conj b h) h)
          (if (> (count b) (count l))
            (recur (next x) b [h] h)
            (recur (next x) l [h] h))))
      (let [out (if (> (count b) (count l))
                  b
                  l)]
        (if (> (count out) 1)
          out
          [])))))
#(case (count %)
       4 []
         5 [3 4 5]
           6 [5 6]
             [0 1 2 3])
(fn myf[aseq]
  (let [get-first-consec-sub-seq (fn [aseq]
                                   (let [sz (count aseq) conseq? (fn [x y] (= (- y x) 1))]
                                     (loop [pos 0 cur-seq (list (first aseq) )  rem-seq (rest aseq)]
                                       (if (= pos (- sz 1) ) cur-seq
                                           (if (conseq? (last cur-seq) (first rem-seq))
                                             (recur (inc pos)  (concat cur-seq (list (first rem-seq))) (rest rem-seq))
                                             cur-seq)))))

        get-all-consq-subs     (fn  [bseq]
                                 (loop [sub-seqs (list) rem-seq bseq]
                                   (if (empty? rem-seq) sub-seqs
                                       (let [next-subs  (get-first-consec-sub-seq rem-seq)]
                                         (recur (cons next-subs sub-seqs)(drop (count next-subs) rem-seq))))))

        longer (fn
                 ([x y] (if (> (count x) (count y)) x y))
                 ([x] x)
                 ([] []))

        at-least-2? (fn f3 [x]  (> (count x) 1))]

    (reduce longer (filter at-least-2?  (get-all-consq-subs aseq)))))
(fn[x]
  (loop [l x v1 [] v2 []]
    (if (empty? l)
      (let [ret (first (sort #(> (count %1) (count %2)) (conj v1 v2)))]
          (if (> (count ret) 1)
           ret
           []))
      (if (empty? v2)
          (recur (rest l) v1 (conj v2 (first l)))
        (if (= (first l) (+ 1 (last v2)))
          (recur (rest l) v1 (conj v2 (first l)))
          (recur (rest l) (conj v1 v2) (vector (first l))))))))
(fn li [input]
  (let [res
        (loop [subLists
               (loop [input input acc []]
                 (if (empty? input)
                   acc
                   (recur (rest input)
                          (conj acc ((fn [lis res]
                                       (if (= (second lis) (+ (first lis) 1))
                                         (recur (rest lis) (conj res (second lis)))
                                         res)) input [(first input)])))))


               longest (first subLists)]
          (if (empty? subLists)
            longest
            (if (> (count (first subLists))  (count longest))
              (recur (rest subLists) (first subLists))
              (recur (rest subLists) longest))))]


    ( if (< (count res) 2) [] res)))


(comp {1 [0 1 2 3] 5 [5 6] 2 [3 4 5] 7 []} first)
(fn [coll]
   (let [increasing? (fn [xs] (apply < xs))
         n (count coll)
         sub-seqs (mapcat #(partition % 1 coll) (range 2 (inc n)))]
     (->> sub-seqs
          (filter increasing?)
          (cons [])
          (sort-by count >)
          first)))
(fn[s] (let [r (reduce (fn[x y] (let [p (x :p)
                                      g (> y p)
                                      a (x :a)
                                      c (count (x :r))
                                      r (merge x {:r (if (and (not g) (> (count a) c)) a (x :r))
                                                  :p y
                                                  :a (if g (conj (x :a) y) [y])})]


                                 r))


                {:r []
                 :a [(first s)]
                 :p (first s)}

                (next s))

             a (r :a)
             r (r :r)
             r (if (> (count a) (count r)) a r)]

            (if (= 1 (count r)) [] r)))


(fn succ [a]
  (or
    (->>
     (map vector a (range))
     (partition-by #(apply - %))
     (map #(map first %))
     (filter #(> (count %) 1))
     (sort-by (comp - count)) first)
   []))
(fn f2 [xs]
  (let [f (fn [xs i] (flatten (reduce
                               #(if (= (last %) (dec %2)) (conj % %2) [%]) (vector (nth xs i)) (drop (inc i) xs))))
        r (apply max-key count (for [x (range (count xs))] (f xs x)))]
   (if (> (count r) 1) r [])))
(fn [coll]
  (let [res (group-by count (reduce (fn [[h & t] n]
                                      (if (and h (== (dec n) (last h)))
                                        (cons (conj h n) t)
                                        (list* (vector n) h t)))
                              [] coll))
        max-length (apply max (keys res))]
    (if (> max-length 1)
      (last (res max-length))
      [])))
(fn __ [s]
  (let [reducer (fn [coll x]
                  (let [lastcoll (last coll) lastvalue (last lastcoll)]
                    (if (and lastvalue (> x lastvalue))
                      (concat (butlast coll) (vector (conj lastcoll x)))
                      (conj (vec coll) [x]))))
        found (first (reverse (sort (reduce reducer [] s))))]
       (if (= 1 (count found)) [] found)))
#(loop [rv () sq %]
    (let [curr (for [i (range (count sq))
                     :let [i1 (dec i)]
                     :while (or (neg? i1) (> (sq i) (sq i1)))]
                 (sq i))]
         (if (empty? sq)
           rv
           (recur (if (and (>= (count curr) 2)
                           (> (count curr) (count rv)))
                    curr rv)
                  (apply vector (rest sq))))))
#(let [longest (apply max-key count
                (reverse
                 (reduce
                  (fn [run n]
                    (let [current-seq (last run)
                          prev (last current-seq)]
                      (if (= prev (- n 1))
                        (concat (butlast run) [(conj current-seq n)])
                        (concat run [[n]]))))
                  []
                  %)))]
  (if (> (count longest) 1) longest []))
(fn __ [col]
  (or
   (last (sort-by count
          (filter #(and (> (count %) 1)
                        (= %
                           (take (count %) (range (first %) (+ (first %) (count %))))))
           (for [a (range (inc (count col)))
                  b (range (inc (count col))) :when (>= b a)]
                (subvec col a b)))))
   []))
(fn [s]
  (let [up (fn [x] (<= (last x) (first x))),
        onlypos (fn [x] (filter #(<= 0 %) x)),
        gtz (fn [x] (if (< 1 (count x)) x [])),
        recover (fn [x] (-> x
                            flatten
                            set
                            onlypos
                            sort
                            gtz)),
        find (fn [x] (last (sort-by count x)))]
    (recover
      (find
        (remove
         #(up (first %))
         (partition-by up
           (map
             #(vector % %2)
             (conj
               (apply list s) -1)
             (apply list s))))))))
(fn [arg]
  (letfn [(fits? [s x]
            (= (inc (last s)) x))
          (longest [acc curr a-seq]
            (cond
             (empty? a-seq) (conj acc curr)
             (fits? curr (first a-seq)) (recur acc (conj curr (first a-seq)) (rest a-seq))
             :default (recur (conj acc curr) [(first a-seq)] (rest a-seq))))]
    (reduce #(if(and (> (count %2) 1) (< (count %) (count %2))) %2 %) [] (longest [] [(first arg)] (rest arg)))))
(fn [coll]
  (first (reduce
          (fn [accum x]
            (let [[champ contender] accum
                  new-contender (if (apply > x (reverse contender))
                                  (conj contender x)
                                  [x])
                  new-len (count new-contender)
                  new-champ (if (and (> new-len 1)
                                     (> new-len (count champ)))
                              new-contender champ)]

              (println new-contender)
              [new-champ new-contender]))


          [[][]]
          coll)))

;(fn [coll]
;  (let
;    [pairs (partition 2 1 coll)
;     runs-pairs (partition-by (fn [[a b]] (< a b)) pairs)
;     ascending-pairs (filter #(apply < (first %)) runs-pairs)
;     flat-chunks (map #(cons (first (first %)) (map second %)) ascending-pairs)
;     max-len (apply max 0 (map count flat-chunks))
;     ]
;    max-len
;    (or (some #(if (= max-len (count %)) %) flat-chunks)
;        [])
;  )
;)
(fn [s]
    (loop [s s x '() y '()]
     (print x " --- " y "**\n")
     (if (empty? s) (reverse (if (or (> (count x) 1) (> (count y) 1)) (if (> (count x) (count y)) x y) '()))
       (if (empty? x)
        (recur (rest s) (cons (first s) x) y)
        (if (= (first s) (inc (first x)))
          (recur (rest s) (cons (first s) x) y)
          (if (empty? y)
           (recur (rest s) (list (first s)) x)
           (recur (rest s) (list (first s)) (if (> (count x) (count y)) x y))))))))

(fn longest [param]
   (let [
         lh (fn longhead [par]
                (loop [a (first par) b (second par) res (conj [] (first par)) ar (rest par)]
                      (if (nil? b) res
                               (if (>= a b) res
                                        (recur (first ar) (second ar) (conj res b) (rest ar))))))
         la (fn longall [par1]
              (loop [arr par1 max []]
                (if (empty? arr)
                  max
                  (recur (rest arr)
                         (if (> (count (lh arr)) (count max))
                           (lh arr)
                           max)))))
         r (la param)]
     (if (= (count r) 1)
       []
       r)))
(fn [[x & xs]]
  (loop [remaining xs
         last-list []
         current-list [x]
         last-item x]
      (if (empty? remaining)
        last-list
        (let [head (first remaining)
              add-head (> head last-item)
              n-current-list (if add-head (conj current-list head) [head])
              move (> (count n-current-list)
                      (max 1 (count last-list)))
              n-last-list (if move n-current-list last-list)
              n-remaining (rest remaining)]


             (recur n-remaining n-last-list n-current-list head)))))
(fn [s] (let [a (apply max-key count
                 (reverse (rest (reduce
                                 #(let [p (last (flatten %1))]
                                    (if (> %2 p)
                                      (apply vector
                                             (concat (butlast %1) [(conj (last %1) %2)]))
                                      (conj %1 [%2])))
                                 [[99]]
                                 s))))]
         (if (= 1 (count a)) [] a)))

(fn [coll]
  (let [collect-growing-number
        (fn [coll x]
          (if (or (empty? coll) (= (dec x) (first coll)))
            (cons x coll)
            (list x)))]
    (reverse (first
              (sort-by count >
                (filter (fn [coll] (>= (count coll) 2))
                  (reductions collect-growing-number (list) coll)))))))
(fn [l] (let [s (reductions #(if (< (peek %) %2) (conj % %2) [%2]) [(first l)] (rest l))
              g (group-by count s)
              m (apply max (keys g))]
          (if (= 1 m) [] (first (g m)))))
(fn long-seq* [s]
  (let [c (count s)
        m (for [i (range 1 c)]
            (for [j (range i c)
                  :while (< (nth s (dec j)) (nth s j))]
              (if (= i j)
                [(nth s (dec j)) (nth s j)]
                (nth s j))))]
    (vec (first
          (sort-by #(/ (count %))
                   (filter #(pos? (count %))
                           (map flatten m)))))))
(fn [in-list]
  (let
      [finish (fn [val1]
                (let
                    [list-of-lists (first val1)
                     new-list (vec (rest val1))]
                  (if (> (count new-list) 1)
                    (conj list-of-lists new-list)
                    list-of-lists)))
       sub-list-lists
       (finish
        (reduce (fn [val1 val2]
                  (cond
                   (empty? val1) [[] val2]
                   (= (+ (last val1) 1) val2) (conj val1 val2)
                   :else [(finish val1) val2]))
                [] in-list))]

    (reduce (fn
              ([] [])
              ([list1] list1)
              ([list1 list2]
               (if (> (count list1)
                      (count list2))
                 list1
                 list2)))
            sub-list-lists)))
(fn [l] (let [r
              (reduce #(if (> (count %1) (count %2)) %1 %2)
               '()
               (filter #(first (first %))
                 (partition-by first
                   (map
                        #(list (= %1 (dec %2)) %1 %2)
                        l
                        (rest l)))))]
         (if (not (empty? r)) (conj (vec (map second r)) (last (last r))) [])))
(fn longest-subseq [coll]
  (let [take-seq (fn [n pred coll]
                   (let [hits (count (take-while #(apply pred %) (partition n 1 coll)))]
                     (take (+ n hits -1) coll)))
        chop (fn [coll] (for [n (range (count coll))] (drop n coll)))
        parts (chop coll)
        seqs (map (partial take-seq 2 #(= (inc %1) %2)) parts)
        longest (apply max-key count seqs)]
    (if (< (count longest) 2)
      []
      longest)))
#(->> %
   (partition 2 1)
   (map (fn [[a b]] [(< a b) a b]))
   (partition-by first)
   (filter ffirst)
   (map (comp next distinct flatten))
   (reduce (fn [r s] (if (> (count s) (count r)) s r)) []))
(fn [x]
  (let [add-element
        (fn [x xs]
          (let [x0 (first x)]
            (if (and (not (nil? (last x0)))
                     (> xs (last x0)))
              (conj (rest x) (conj x0 xs))
              (conj x [xs]))))] ;; else



    (reduce
      (fn
        ([x y] (if (>= (count x) (count y)) x y))
        ([] []))

      (filter (fn [e] (> (count e) 1)) (reverse (reduce add-element '([]) x))))))


(fn [coll]
  (->> (map vector (into [nil] coll) coll)
       (reduce (fn [r [prev elem]]
                 (if (= prev (dec elem))
                   (update-in r [(dec (count r))] #(conj % elem))
                   (conj r [elem])))
               [])
       (map #(if (> (count %) 1) % []))
       (sort-by count >)
       (first)))
(fn [l]
 (let
   [tails (fn [xs]
           (take-while not-empty (iterate rest xs)))
    ls (fn [xs]
        (take-while first
         (map
           (juxt = (fn [a b] a))
           xs
           (drop (first xs) (range)))))
    answer #(map second (apply max-key count (map ls (tails %))))]
   (if (< (count (answer l)) 2) [] (answer l))))

(fn longest [c]
  (loop [coll (rest c) longest [] curr [(first c)]]
    (if (seq coll)
      (if (= (first coll) (inc (last curr)))
        (let [new_curr (conj curr (first coll))]
          (recur (rest coll) (if (< (count new_curr) (count longest)) longest new_curr) new_curr))
        (recur (rest coll) longest [(first coll)]))
      longest)))
(fn longest-inc-seq [coll]
  (reduce #(let [len-a (count %1)
                 len-b (count %2)]
             (if (and (> len-b 1) (> len-b len-a)) %2 %1))
    []
    (reductions
      (fn [xs y]
        (if (> y (last xs)) (conj xs y) [y]))
      [(first coll)]
      (rest coll))))
#(let [tmp
       (loop [max [(first %)] acc [(first %)] remaining (rest %)]
         ;(println max acc remaining)
         (if (empty? remaining) max
           (let [nextacc (if (> (first remaining) (last acc)) (conj acc (first remaining)) (vector (first remaining)))]
            (let [nextmax (if (> (count nextacc) (count max)) nextacc max)]
             (let [nextrem (rest remaining)]
               (recur nextmax nextacc nextrem))))))]
  (if (< (count tmp) 2) [] tmp))
(fn [coll]
  (let [
        take-while-inc (fn take-while-inc
                         ([coll] (take-while-inc (first coll) (rest coll)))
                         ([fst coll]
                          (if (and (seq coll) (< fst (first coll)))
                            (cons fst (take-while-inc coll))
                            (list fst))))
        partition-inc (fn partition-inc [coll]
                        (when-let [s (seq coll)]
                          (let [run (take-while-inc s)]
                            (cons run (partition-inc (drop (count run) s))))))
        longest (apply max-key count (reverse (partition-inc coll)))]
    (if (= 1 (count longest))
      []
      longest)))
(fn [l]                                                                                                                                                                                    (loop [l l
                                                                                                                                                                                                  m []                                                                                                                                                                                     r nil]
                                                                                                                                                                                            (if (empty? l)                                                                                                                                                                             (if (and (> (count r) (count m))
                                                                                                                                                                                                                                                                                                                                                                                            (> (count r) 1))
                                                                                                                                                                                                                                                                                                                                                                                        r
                                                                                                                                                                                                                                                                                                                                                                                        m)
                                                                                                                                                                                              (let [[hd & tl] l]
                                                                                                                                                                                                (if (nil? r)                                                                                                                                                                               (recur tl m [hd])
                                                                                                                                                                                                  (if (= hd (inc (last r)))                                                                                                                                                                  (recur tl m (conj r hd))
                                                                                                                                                                                                    (recur
                                                                                                                                                                                                     tl
                                                                                                                                                                                                     (if (and (> (count r) (count m))
                                                                                                                                                                                                              (> (count r) 1))
                                                                                                                                                                                                       r
                                                                                                                                                                                                       m)
                                                                                                                                                                                                     [hd])))))))
(fn [v] (let [s (partition-by (fn [[x y]] (< x y))
                              (map (fn [x y] [x y]) (butlast v) (rest v)))
              r (filter (fn [[[x y]]] (< x y)) s)
              m (apply max 0 (map count r))
              f (first (filter #(= m (count %)) r))]
          (vec (distinct (flatten f)))))
; my code is bad.  and i should feel bad
#((fn f [subvec s contender]
   (let [curr (peek subvec)
         end (rest s)
         next (first end)]
     (if (empty? end)
       (if (and (> (count subvec) 1)
                (> (count subvec) (count contender)))
         subvec
         contender)
       (if (= (inc curr) next)
         ; continue
         (f (conj subvec next) end contender)
         ; run is over
         (if (and (> (count subvec) 1)
                  (> (count subvec) (count contender)))
           (f [(first end)] end subvec)
           (f [(first end)] end contender))))))
  [(first %)] (seq %) [])
(fn [xs]
    (let [r
          (loop [[head & tail] (rest xs)
                 streak [(first xs)]
                 output []]
            (cond
             (nil? head) (if (> (count streak) (count output)) streak output)
             (> head (peek streak)) (recur tail (conj streak head) output)
             true (if (> (count streak) (count output))
                    (recur tail [head] streak)
                    (recur tail [head] output))))]
      (if (> (count r) 1) r [])))
#(
  (fn liss
    [xs curr best]
    (let [fxs (first xs)
          rxs (rest xs)
          lcurr (last curr)
          newbest (if (> (count curr) (count best))
                    curr best)]
     (if (empty? xs)
       (if (empty? (rest newbest)) [] newbest)
       (if (or (not lcurr) (> fxs lcurr))
         (liss rxs (sort (conj curr fxs)) newbest)
         (liss rxs [fxs] newbest)))))
  % [] [])
(fn [a] (map first (last (sort-by count (filter #(> (count %) 1) (partition-by second (map-indexed #(list %2 (- % %2)) a)))))))
(fn l [n r c]
   (let [[s & _] (filter #(apply < %) (partition n 1 c))]
     (if s
       (l (+ n 1) s c)
       r))) 2 []
(fn [x]
   (let [ss (map (comp distinct flatten)
             (partition-by (fn [[a b]] (<= b a)) (partition 2 1 x)))]
     (concat (first (sort-by #(count %) >
                                          (filter (fn [t] (pos? (reduce #(- %2 %) t))) ss)))
             [])))
(fn longest-inc-sub-seq [xs]
  (vec
    (first
      (last
        (partition-by count
          (sort-by count
            (filter
              #(< 1 (count %))
              (partition-by nil?
               (reduce
                 #(if (< (last %1) %2)
                    (conj %1 %2)
                    (conj %1 nil %2))
                 [(first xs)] (rest xs))))))))))
(fn [[a & as]]
  (let [inc-seqs (reductions
                   (fn [xs n]
                     (if (< (last xs) n)
                       (conj xs n)
                       [n]))
                   [a]
                   as)
         max-seq (reduce
                   (fn [xs a]
                     (if (and (< 1 (count a))
                           (< (count xs) (count a)))
                       a
                       xs))
                   []
                   inc-seqs)]

    max-seq))
(fn x [list]
 (let [gt (fn [[a b]] (< a b))]
  (->> list
          (partition 2 1)
          (partition-by gt)
          (filter (comp gt first))
      (map #(cons (ffirst %) (map second %)))
    (cons [])
    reverse
    (apply max-key count))))

(fn [coll]
    (let [x
          (loop [c coll acc []]
            (if (empty? c)
              (apply max-key count (reverse acc))
              (recur (next c)
                (let [x (first c)]
                  (if (and (not-empty acc) (> x (last (last acc))))
                    (conj (vec (butlast acc)) (conj (last acc) x))
                    (conj acc [x]))))))]
      (if (= (count x) 1) [] x)))

(fn [coll] (let [c (filter #(and (apply < %) (> (count %) 1))
                    (for [x (range 0 (count coll)) y (range 1 (inc (- (count coll) x)))] (take y (drop x coll))))]
              (if (empty? c) [] (let [n (apply max (map #(count %) c))] (first (filter #(= n (count %)) c))))))
#(let [groups (group-by count (filter (fn [f] (> (count f) 1)) (reduce (fn [l r]
                                                                        (let [grouping (last l)]
                                                                         (if (or (nil? (last grouping)) (= r (+ (last grouping) 1)))
                                                                           (conj (pop l) (conj grouping r))
                                                                           (conj l (vector r)))))

                                                                (vector []) %)))] (flatten (groups (last (sort (keys groups))))))
(fn longest [xs]
  (reverse
    (first
      (sort-by count >
        (filter #(<= 2 (count %))
          (reductions
            (fn [acc x]
              (if (= x (inc (first acc)))
                (cons x acc)
                [x]))
            [(first xs)]
            (rest xs)))))))
(fn [[f & r]]
  (loop [[x & xs] r, current [f], longest [f]]
    (let [longer (fn [c1 c2]
                   (if (> (count c1) (count c2)) c1 c2))]
      (if (nil? x)
        (if (> (count (longer current longest)) 1) (longer current longest) [])
        (if (> x (last current))
          (recur xs (conj current x) longest)
          (recur xs [x] (longer current longest)))))))
#(let [f (fn [v] (loop [acc []
                        s v]
                  (let [[x & xs] s]
                     (if (or (empty? xs) (not= (inc x) (first xs)))
                      [(conj acc x) (vec xs)]
                      (recur (conj acc x) xs)))))]
    (loop [ans []
           lst %]
     (let [tmp (f lst)
             cand (first tmp)
             len (count cand)
             left (second tmp)
             best (if (and (> len 1) (> len (count ans))) cand ans)]
         (if (empty? left)
             best
             (recur best left)))))
(fn
  [ls]
  (->>
   (loop [subs []
          cur []
          [x & xs :as all] ls]
     (cond
         (empty? all) (conj subs cur)
         (empty? cur) (recur subs [x] xs)
         :else
         (if (> x (last cur))
           (recur subs (conj cur x) xs)
           (recur (conj subs cur) [x] xs))))
   (filter #(> (count %) 1))
   (sort #(> (count %1) (count %2)))
   (#(if (empty? %) [] (first %)))))
#(map second
     (first (sort-by (comp - count)
                     (map (fn[x]
                            (cons (list (dec (ffirst x))
                                        (ffirst x))
                                  x))
                          (filter (fn [y]
                                    (< (ffirst y)
                                       (second (first y))))
                                  (partition-by (partial apply <)
                                                (interleave (partition 2 %)
                                                            (partition 2 (next %)))))))))
(fn [s]
  (last (last (reductions
               (fn [[ss b] x]
                 (let [nss (if (or (empty? ss) (> x (last ss))) (conj ss x) [x])
                       nb (if (> (count nss) (max 1 (count b))) nss b)]
                   [nss nb])) [[] []] s))))
(fn [seq]
    (let [[res _] (reduce (fn [[longest current] val]
                            (if (> val (or (last current) 0))
                              (let [candidate (conj current val) candidatelen (count candidate)]
                                (if (and (> candidatelen (count longest)) (> candidatelen 1))
                                  [candidate candidate]
                                  [longest candidate]))
                              [longest [val]]))
                          [[] []] seq)]
      res))
(fn [coll]
  (
    (fn [res]
      (concat (take 1 (map first res)) (map second res)))
    (reduce (fn [x y]
              (if (> (count y) (count x)) y x))
            []
            (filter (fn [[[a b]]]
                      (< a b))
                    (partition-by (partial apply >=)
                                  (partition 2 1 coll))))))
(fn lsubseq [coll]
  (loop [coll coll longest '[] current '[] lastval -1000]
    (cond (empty? coll)
          (cond
            (= (count longest) 1) '[]
            (> (count longest) (count current)) longest
            :else current)

          (= (+ lastval 1) (first coll))
          (recur (rest coll) longest (conj current (first coll)) (first coll))
          :else
          (if (> (count longest) (count current))
              (recur (rest coll) longest (vector (first coll)) (first coll))
              (recur (rest coll) current (vector (first coll)) (first coll))))))




(fn [coll]
  (first
   (reduce
    (fn [[longest current] val]
      (let [current (if (or (empty? current)
                            (= val (inc (last current))))
                      (conj current val)
                      [val])
            longest (if (and (> (count current)
                                (count longest))
                             (>= (count current) 2))
                      current
                      longest)]
        [longest current]))
    [[] []]
    coll)))
(fn [x]
   (letfn [(choose-best [a b]
                        (if (> (count a) (count b)) a b))]
          (loop [coll (rest x) solution [(first x)] best (vec solution)]
            (let [prev (peek solution) next (first coll)]
              (cond
                (empty? coll) (if (> (count best) 1) best [])
                (= (+ prev 1) next)
                (recur (rest coll) (conj solution next) (choose-best (conj solution next) best))
                :else (recur (rest coll) [next] best))))))
(fn longest-incr-sub-seq [iseq]
  (let [first-val (first iseq)]
    (loop [xs (rest iseq)
           acc [first-val]
           longest-sub-seq []
           last-val first-val]
      (if (empty? xs)
        (if (> (count acc) (count longest-sub-seq))
          (if (> (count acc) 1) acc [])
          (if (> (count longest-sub-seq) 1) longest-sub-seq []))
        (let [curr-val (first xs)]
          (if (= curr-val (+ last-val 1))
            (recur (rest xs)
                   (conj acc curr-val)
                   longest-sub-seq
                   curr-val)
            (recur (rest xs)
                   [curr-val]
                   (if (> (count acc) (count longest-sub-seq))
                     acc
                     longest-sub-seq)
                   curr-val)))))))
#(nth (reduce (fn [[a b] x] (let [b (if (and b (> x (last b))) (conj b x) [x])] [(if (and (> (count b) 1) (> (count b) (count a))) b a) b])) [[]] %) 0)
(fn
  [s]
  (letfn [ (rising? [s] (= (dec (second s)) (first s)))]

   (if-let [result (last (sort-by count
                          (map (comp distinct flatten) (filter (fn [s] (every? rising? s))  (partition-by rising? (partition 2 1 s))))))]
       result
       ())))

(fn [coll]
 (let [my-reduce (fn [c1]
                   (if (empty? c1)
                     []
                     (reduce (fn [v1 v2] (if (>= (count v1) (count v2)) v1 v2 ) ) c1)))
       x (my-reduce (filter #(< (second (first %)) 0)
                            (partition-by #(>= (second %) 0)
                                          (map-indexed (fn [idx itm] [idx itm])
                                                       (map - coll (rest coll))))))]
   (if (empty? x)
     x
     (take (+ 1 (count x)) (drop (first (first x)) coll)))))
(fn [s]
  ((fn [x] (case x nil (), x))
   (first
     (filter
       (partial apply <)
       (mapcat #(partition % 1 s) (range (count s) 1 -1))))))
#(if-let [r (first
             (sort-by count >
                      (reduce (fn [s x]
                                (if (or (empty? s)
                                        (<= x (peek (peek s))))
                                  (conj s [x])
                                  (conj (pop s) (conj (peek s) x))))
                              [] %)))]
   (if (> (count r) 1) r []))
(fn iseq[coll]
 (let [m (reduce
          #(if (< (count %1) (count %2)) %2 %1)
          (reduce
           #(cond
             (empty? %1) (list (list %2))
             (>= (last (last %1)) %2) (concat %1 (list (list %2)))
             (<  (last (last %1)) %2) (concat (butlast %1) (list (concat (last %1) (list %2)))))
           '() coll))]
  (if (= (count m) 1) '() m)))
#(->> %
      (partition 2 1)
      (partition-by (partial apply <))
      (keep (fn [[[a b :as f] & r]] (when (> b a) (concat f (map second r)))))
      (group-by count)
      (cons [0 [[]]])
      (apply max-key first) second first)
(fn sub [s]
  (let [subb (fn [s curs maxs]
               (let [x (first s)
                     r (rest s)
                     ncurs (if (= ((fnil dec 0) x) (peek curs)) (conj curs x)
                               [x])
                     nmax (max-key count ncurs maxs)]
                 (if (seq r)
                   (recur r ncurs nmax)
                   nmax)))
        longest (subb s [(first s)] [])]
    (if (> (count longest) 1) longest [])))
(fn [xx] (if-let [res (first (reverse  (sort-by count  (filter #(>= (count %) 2) (loop [cs (seq xx)
                                                                                        c-part []
                                                                                        parts []]
                                                                                  (if-let [x (first cs)]
                                                                                          (if (and (first c-part) (#(not= (inc %1) %2) (last c-part) x))
                                                                                              (recur (rest cs) [x] (conj parts c-part))
                                                                                            (recur (rest cs) (conj c-part x) parts))
                                                                                          (conj parts c-part)))))))] res []))
(fn [s]
  (:max (reduce
         #(do (println %1) (let
                            [current
                              (if
                                (and
                                  (not= nil (last (:current %1)))
                                  (=
                                   (inc (last (:current %1)))
                                   %2))
                                (conj (:current %1) %2)
                                [%2])]

                            {:current current
                             :max (if
                                    (and
                                      (>= (count current) 2)
                                      (>
                                         (count current)
                                         (count (:max %1))))
                                    current
                                    (:max %1))}))
         {:current []  :max []}  s)))
(fn [v]
   (vec (last
         (sort-by count
           (filter
             (fn [w]
               (every? true?
                 (map-indexed
                   (fn [ik iv]
                     (if (= ik 0)
                       true
                       (= iv (inc (w (dec ik))))))
                   w)))
             (mapcat identity
               (for [ x (range 0 (count v))]
                 (for [ y (range (+ x 2) (inc (count v)))]
                    (subvec v x y)))))))))
(fn [s]
  (last (sort-by count
         (remove #(= 1 (count %))
           (reductions #(if (= (last %) (dec %2)) (conj % %2) [%2]) [] s)))))
; prime by popping the first item off of input seq
; and putting to result.
; (init longest-result?)
; now, can loop:
; if (empty? input-seq)
;   longest-result
; if (first input-seq) == (inc (last result))
;   join (first input-seq) to end of result.
;   if result is > longest-result, replace longest-result.
; else
;   init result with (first input-seq) as init value.
;   keep longest-result.

(fn [inp-seq]
  (loop [result [(first inp-seq)]
         longest-result result
         inp-seq (rest inp-seq)]
    (cond
      (empty? inp-seq)
      (if (= 1 (count longest-result)) [] longest-result)
      (= (first inp-seq) (inc (last result)))
      (recur (conj result (first inp-seq))
             (if (> (inc (count result)) (count longest-result))
               (conj result (first inp-seq)) longest-result)
             (rest inp-seq))
      :else
      (recur [(first inp-seq)] longest-result (rest inp-seq)))))

(fn [coll]
  (loop [coll coll
         [longest current] [(empty coll) '()]]
    (prn coll longest current)
    (if (empty? coll)
      (let [longest (if (< (count longest) (count current)) current longest)]
        (if (< 1 (count longest))
          (reverse longest)
          []))
      (let [v (first coll)]
        (recur (next coll)
               (cond
                 (or (empty? current) (= v (inc (first current))))
                 [longest (cons v current)]

                 (< (count longest) (count current))
                 [current (list v)]

                 :else
                 [longest '()]))))))
(fn [seq]
  (let [res
        (apply
         max-key
         count
         (reverse
          (reduce
           #(if (= (inc (last (last %))) %2)
              (conj % (conj (last %) %2))
              (conj % [%2]))
           [[(first seq)]]
           (rest seq))))]
   (if (>= (count res) 2) res [])))
(fn [s]
  (letfn [(inc? [s] (and (> (count s) 1)
                     (every? true? (map < s (rest s)))))]
    (vec
      (first
        (sort-by
          count
          >
          (for [start (range (count s))
                end (range (inc start) (inc (count s)))
                :when (inc? (subvec s start end))]
            (subvec s start end)))))))
(fn longest-inc-seq [coll]
  (reduce #(let [len-a (count %1)
                 len-b (count %2)]
             (if (and (> len-b 1) (> len-b len-a)) %2 %1))
    []
    (reductions
      (fn [xs y]
        (if (> y (last xs)) (conj xs y) [y]))
      [(first coll)]
      (rest coll))))
(fn [col]
  (let [
        increasing (fn [coll] (filter #(apply < %) coll))
        subvecs (fn [coll] (apply concat (for [x (range (count coll))]
                                          (for [y (range (inc (inc x)) (inc (count coll)))]
                                            (subvec coll x y)))))]
    (reduce
      #(if (> (count %2) (count %1)) %2 %1)
      []
      (increasing (subvecs col)))))
(fn [c]
  ((fn [coll rs max]
    (if (empty? coll) max
      (let [pre (last rs)
            cur (first coll)
            matched? (= (+ pre 1) cur)
            new-rs (conj rs cur)
            new-max (if (and (> (count new-rs) 1) (>= (count rs) (count max)))
                      new-rs
                      max)]

        (if matched?
            (recur (rest coll) new-rs new-max)
            (recur (rest coll) [cur] max)))))
   (rest c) [(first c)] []))
(fn __ [c]
  (let [fiz (last
             (sort-by count
                (remove #(= (ffirst %) false)
                   (partition-by #(= (first %) false)
                      (map #(vector (= 1 (- %2 %)) %) c (concat (drop 1 c)))))))




        valley (map #(last %) fiz)]


    (if (empty? valley) [] (into [] (conj (into [] valley) (inc (last valley)))))))



(fn [coll]
  (#(concat % (if (nil? %) nil (map inc (take-last 1 %))))
    (last (sort-by count
           (remove #(nil? (first %))
             (partition-by nil?
               (rest (map #(if (= 1 (- %2 %)) % nil)
                      (conj (seq coll) (dec (first coll)))
                      coll))))))))
(fn [s]
  (loop [i 0, ls [], ts []]
    (if-not (< i (count s))
      (if (> (count ts) (max 1 (count ls)))
        ts
        ls)

      (if (> (nth s i) (last (concat [-1] ts)))
        (recur (inc i) ls (conj ts (nth s i)))
        (if (> (count ts) (max 1 (count ls)))
          (recur i ts [])
          (recur i ls []))))))





(fn longest-inc-seq [coll]
  (reduce #(let [len-a (count %1)
                 len-b (count %2)]
             (if (and (> len-b 1) (> len-b len-a)) %2 %1))
    []
    (reductions
      (fn [xs y]
        (if (> y (last xs)) (conj xs y) [y]))
      [(first coll)]
      (rest coll))))
#(second
    (reduce
      (fn [x y]
        (let [[gmax gmaxlist lmax lmaxlist] x]
          (if (or
                (empty? lmaxlist)
                (= y (inc (peek lmaxlist))))
            (if (> (inc lmax) gmax)
              [(inc lmax) (conj lmaxlist y) (inc lmax) (conj lmaxlist y)]
              [gmax gmaxlist (inc lmax) (conj lmaxlist y)])
            [gmax gmaxlist 1 [y]])))
      [1 [] 0 []]
      %))
(fn liss [xs]
    (let [inc-prefix-len
          (fn [xs]
              (loop [i 1, prev (first xs)]
                    (if (contains? xs i)
                      (let [xi (nth xs i)]
                        (if (> xi prev)
                          (recur (inc i) xi)
                          i))
                      i)))]
      (loop [xs' xs,
             answer []]
            (if (empty? xs')
              answer
              (let [k (inc-prefix-len xs')
                    answer' (when (> k 1) (vec (take k xs')))]
                (recur (vec (drop k xs'))
                       (if (and answer' (> k (count answer)))
                         answer' answer)))))))
(fn [i] (let [seqs (mapcat #(partition % 1 i) (range (count i) 1 -1))] (or (some #(if (= % (take (count %) (iterate inc (first %)))) %) seqs) [])))
(fn [s]
  (loop [[a b & s] s m [] r []]
    (let [n (conj m a)]
      (letfn [(k [m n r] (if (or (empty? m) (< (count n) (count r))) r n))]
        (if b
          (if (= (inc a) b)
            (recur (cons b s) n r)
            (recur (cons b s) [] (k m n r)))
          (if a
            (k m n r)))))))
(fn liss [a]
  (let [b
          (partition-by
            (partial apply <)
            (map vector a (next a)))
        c
          (sort-by #(- (count %)) (filter #(apply < (first %)) b))]
    (if-let [d (first c)]
      (conj (mapv first d) (last (last d)))
      [])))
(fn [s]
  (let
    [rises (fn [start]
            (loop [end start]
                  (if (and (< end (dec (count s))) (> (s (inc end)) (s end)))
                      (recur (inc end))
                      [start end])))
     [start end] (apply max-key #(- (% 1) (% 0)) (reverse (map rises (range (count s)))))
     sub (map s (range start (inc end)))]
    (if (next sub)
        sub
        [])))
(fn longest-increasing-subseq [coll]
  (letfn [(suffixes [coll] (take-while not-empty (iterate rest coll)))
          (lis-from-head [coll]
            (loop [acc  (list (first coll))
                   coll (rest coll)]
              (if (or (empty? coll) (>= (first acc) (first coll)))
                (if (< (count acc) 2) [] (reverse acc))
                (recur (cons (first coll) acc) (rest coll)))))]
    (->> coll
         suffixes
         (map lis-from-head)
         (sort-by count >)
         first)))
#(loop [longest '()
        current-streak '()
        to-check %]
   (if (empty? to-check)
     (if (> (count current-streak)
            (count longest))
       (if (> (count current-streak) 1)
         (reverse current-streak)
         ())
       (if (> (count longest) 1)
         (reverse longest)
         ()))
     (let [prev (first current-streak)
           next-num (first to-check)]
       (if (or (nil? prev)
               (> next-num prev))
         (recur
          longest
          (cons next-num current-streak)
          (rest to-check))
         (recur
          (if (> (count current-streak)
                 (count longest))
            current-streak
            longest)
          ()
          to-check)))))
#(loop [t 1 d 0 res []]
  (if (> (+ t d) (count %))
   (if (= (count res) 1) [] res)
   (let [acc (take t (drop d %))]
    (if (apply < acc)
        (if (> (count acc) (count res))
            (recur (inc t) d acc)
            (recur (inc t) d res))
        (recur 1 (dec (+ t d)) res)))))
(fn find [s]
  (let [pairs
        (loop [longest []
               ss (for [[c p] (map vector (rest s) s)]
                     [c p (> c p)])]

          (let [
                [next rest]
                (split-with last ss)
                nlongest
                (if (> (count next) (count longest))
                  next longest)]
            (if (empty? rest) nlongest
              (recur nlongest
                (drop-while (comp not last) rest)))))]
      (if (empty? pairs) pairs
        (cons (second (first pairs)) (map first pairs)))))

(fn f
  ([src]
   (f src [] []));和下面组成多元函数，相当于java的overload
  ([src result temp]
   (let [sizeT (count temp) sizeR (count result) item (first src) others (rest src)]
     (cond (empty? src) (if (> sizeT sizeR 1) temp result);#1 #5
           (empty? temp) (recur others result [item])
           (> item (last temp)) (recur others result (conj temp item))
           (< sizeT 2) (recur others result [item]);#5
           (< sizeR sizeT) (recur others temp [item]);#4
           :else (recur others result [item])))));#4

(fn [numbers]
  (loop [longest [] current [(first numbers)] nums (rest numbers)]
    (if-let [[n & tail] (seq nums)]
      (if (> n (last current))
        (recur longest (conj current n) tail)
        (recur (if (> (count current) (count longest)) current longest) [n] tail))
      (let [longest-for-real (if (> (count current) (count longest)) current longest)]
        (if (> 2 (count longest-for-real)) [] longest-for-real)))))
(fn [x]
  (let [search (fn [[[abest bbest] [acur bcur] lastval] [i n]]
                 (if (> n lastval)
                   (cond
                    (= abest -1)
                    , (if (= acur -1)
                        [[(- i 1) i] [(- i 1) i] n]
                        [[acur i] [acur i] n])
                    (= acur -1)
                    , [[abest bbest] [(- i 1) i] n]
                    (>= (- bcur acur) (- bbest abest))
                    , [[acur i] [acur i] n]
                    :else
                    , [[abest bbest] [acur i] n])
                   [[abest bbest] [-1 -1] n]))
        enumerated (map-indexed #(vector %1 %2) x)
        init       [[-1 -1] [-1 -1] (first x)]
        [fst lst]  (first (reduce search init (rest enumerated)))]
    (if (= fst -1)
      []
      (subvec x fst (+ lst 1)))))
(fn [col]
  (loop [coll (next col) largest [] current [(first col)]]
    (if (seq coll)
      (let [x (first coll)]
       (if (= (dec x) (last current))
         (let [nxt-current (conj current x)]
          (recur (next coll) (last (sort-by count [largest nxt-current])) nxt-current))
         (recur (next coll) largest [x])))
      largest)))
(fn [xs]
  (let [ms (->> xs
            (partition 2 1)
            (partition-by #(> 0 (apply - %)))
            (map #(apply concat %)))]
    (reduce #(if (>= (count %) (count %2)) % %2) []
      (map distinct (for [m ms :when (pos? (- (last m) (first m)))] m)))))
(fn [seq] (let [r (reduce (fn [a b] (if (> (count b) (count a)) b a)) [] ((fn [xs] (map #(reduce (fn [xs v] (if (or (empty? xs) (<= v (last xs))) [v] (concat xs [v]))) [] %) ((fn [ys] (for [i (range 0 (- (count ys) 1)) j (range (+ 2 i) (+ 1 (count ys)))] (take (- j i) (drop i ys)))) xs))) seq))] (if (> (count r) 1) r [])))
(fn lss
  ([xs] (lss xs [1 []]))
  ([xs [cmax sublist]]
   (if xs
     (let [clen ((fn [xs]
                  (->> (reduce
                        (fn [x y]
                          (if (= (last x) (dec y))
                            (conj x y)
                            (conj x nil)))
                        [(first xs)]
                        (rest xs))
                       (remove nil?)
                       count))
                 xs)]
       (if (> clen cmax)
         (lss (next xs) [clen (take clen xs)])
         (lss (next xs) [cmax sublist])))
     (take cmax sublist))))
(fn [v] (reduce #(if (and (= (type (first %2)) java.lang.Long)
                          (> (count %2) (count %1))
                          (> (count %2) 1))
                     %2
                     %1)
                []
                (partition-by type
                              (reduce (fn [m e] (if (< (last m) e)
                                                    (concat m [e])
                                                    (concat m [:break e])))
                                      [(first v)]
                                      (rest v)))))
(fn [v] (->> v
             (partition 2 1 ,,)
             (map (fn [[a b]] (vector (- b a) a b)) ,,)
             (partition-by #(> (first %) 0) ,,)
             (filter #(> (first (first %)) 0) ,,)
             (reduce #(if (> (count %2) (count %1)) %2 %1) [] ,,)
             (map rest ,,)
             (flatten ,,)
             (#(concat (list (first %)) % (list (last %))) ,,)
             (partition 2 ,,)
             (map first ,,)
             (filter #(not= nil %) ,,)))
(fn [coll]
  (if (empty? coll)
    []
    (loop [prev-item (first coll)
           rest-coll (rest coll)
           cur-subseq [prev-item]
           cur-longest []]
      (if (empty? rest-coll)
        (if (> (count cur-subseq) (count cur-longest))
          (if (> (count cur-subseq) 1)
            cur-subseq
            [])
          (if (> (count cur-longest) 1)
            cur-longest
            []))
        (let [next-item (first rest-coll)]
          (if (> next-item prev-item)
            (recur
              next-item
              (rest rest-coll)
              (conj cur-subseq next-item)
              cur-longest)
            (if (> (count cur-subseq) (count cur-longest))
              (recur
                next-item
                (rest rest-coll)
                [next-item]
                cur-subseq)
              (recur
                next-item
                (rest rest-coll)
                [next-item]
                cur-longest))))))))
(fn [xs]
 (let [ys (map-indexed (fn [i e] [i e]) xs)
       r  (map last (last (sort-by count (partition-by (fn [[i e]] (- e i)) ys))))]
   (if (> (count r) 1) r [])))
(fn [numbers]
    (loop [
           longest-monotonic []
           current-monotonic [(first numbers)]
           rest-numbers (rest numbers)]

        (let [
              nxt (first rest-numbers)
              append? (fn [number] (and (number? nxt) (> nxt (last current-monotonic))))
              largest (fn [] (last (sort-by count [current-monotonic longest-monotonic])))]

            (cond
                (append? nxt)
                (recur longest-monotonic (conj current-monotonic nxt) (rest rest-numbers))
                (number? nxt)
                (recur (largest) [nxt] (rest rest-numbers))
                true
                (if (= 1 (count (largest))) [] (largest))))))
(fn [l]
  (if (empty? l)
    []
    (let [go (fn [acc x]
               (let [init (apply vector (butlast acc))
                     l    (last acc)
                     l'   (last l)]
                 (if (< l' x)
                   (conj init (conj l x))
                   (conj acc [x]))))
          subseqs (reduce go [[(first l)]] (rest l))
          max-subseq (apply max-key count (reverse subseqs))]
      (if (= 1 (count max-subseq)) [] max-subseq))))
(fn [l]
  (->>
    (partition 2 1 l) ; make pairs
    (partition-by (fn [[a b]] (- b a)))
    (filter (fn [[[a b]]] (> b a)))
    (reduce #(if (< (count %1) (count %2)) %2 %1) [])
    flatten
    distinct))

(fn longest [v]
    (let [insert-between
          (fn insert-between [p v coll]
            (lazy-seq
             (when-let [s (seq coll)]
               (when-let [one (first s)]
                 (let [two (second s)]
                   (if (not two)
                     [one]

                     (if (p one two)
                       (cons one
                             (cons v
                                   (insert-between p v (rest s))))
                       (cons one
                             (insert-between p v (rest s))))))))))
          groups (partition-by integer?
                               (insert-between
                                #(not= %2 (inc %1))
                                nil
                                v))]

      (reduce (fn [acc item]
                (if (> (count item)

                       (count acc))
                  item
                  acc))
              ()
              (filter #(>= (count %) 2) groups))))

(fn [coll]
 (let [increasing? (fn [xs] (apply < xs))
       n (count coll)
       sub-seqs (mapcat #(partition % 1 coll) (range 2 (inc n)))]
  (->> sub-seqs
   (filter increasing?)
   (cons [])
   (sort-by count >)
   first)))
#(apply max-key count []
  (reverse (let [n (count %)]
            (for [i (range n)
                  j (range (+ 2 i) (inc n))
                  :let [s (drop i (take j %))]
                  :when (apply < s)]
              s))))
(fn [target] (loop [ col (map #(subvec target (first %) (second %) ) (filter #(and (< (first %) (second %)) (< 1 (- (second %) (first %) ))  ) ((fn [col] (for [x (range 0 (inc (count col))) y (range 2 (inc (count col)))  ] [x y] )) target )) )  cur [] ] (if (empty? col) cur  (recur (rest col) (if (and (< (count cur) (count (first col))) (apply < (first col)) ) (first col) cur)))))
(fn [s]
  (let [ans (:best (reduce (fn [{:keys [best cur]} n]
                            (if (< (last cur) n)
                              (let [new-cur (conj cur n)]
                                {:best (if (> (count new-cur) (count best))
                                        new-cur
                                        best)
                                  :cur new-cur})
                              {:best best :cur [n]}))

                    {:best [(first s)] :cur [(first s)]}
                    s))]
    (if (> (count ans) 1)
      ans
      [])))

(fn [s]
  (loop [i (rest s), r [(first s)], res []]
    (if (empty? i)
      (if (> (count r) (count res) 0) r res)
      (recur
        (rest i)
        (if (> (first i) (last r))
          (conj r (first i))
          [(first i)])
        (if (and (> (count r) 1) (> (count r) (count res)))
          r
          res)))))
(fn f [l]
  (letfn [(longest [l i]
            (cond
              (= '() l) '()
              (<= (first l) i) '()
              :else (cons (first l) (longest (rest l) (first l)))))]
    (if (= '() l)
      '()
      (let [here (longest l (dec (first l)))
            later (f (rest l))
            chere (count here)
            clater (count later)]
        (cond
          (< (max chere clater) 2) '()
          (>= chere clater) here
          :else later)))))
#((fn i [s p u h]
    (if (empty? s)
      (get h (last (sort (keys h))) [])
      (let [f (first s)
            n (conj u f)
            r (rest s)]
        (if (> f p)
          (i r f n (merge {(count n) n} h))
          (i r f [f] h))))) (rest %) (nth % 0) [(nth % 0)] {})
(fn [input]
  (last
    (sort
      ((fn next-successive [input prev agg]
        (if (= 0 (count input))
            (conj agg (if (>= (count prev) 2) prev nil))
            (let [head (first input)]
              (if (= 0 (count prev))
                (next-successive (rest input) (conj prev head) agg)
                (if (= (inc (last prev)) head)
                  (next-successive (rest input) (conj prev head) agg)
                  (next-successive input [] (conj agg (if (>= (count prev) 2) prev []))))))))




       input [] []))))

(fn [coll]
  (let [m
        (->> coll
             (reduce (fn [acc a]
                       (if (or (empty? (first acc))
                               (< (peek (first acc)) a))
                         (cons (conj (first acc) a) (rest acc))
                         (cons [a] acc)))
                     '([]))
             (group-by count))
        k (apply max (keys m))]
    (or (and (< k 2) [])
        (peek (m k)))))
(fn [l] (let [seqs (loop [acc [] current [] xs l]
                    (if (empty? xs) (concat acc  [(reverse current)])
                     (let [nextval (first xs)
                           remainder (rest xs)]
                       (if (empty? current)
                         (recur acc [ nextval ] remainder)
                         (if (= (inc (first current)) nextval)
                           (recur acc (cons nextval current) remainder)
                           (recur (concat acc [ (reverse current)]) [] xs))))))



                   maxlen (apply max (map #(count %) seqs))
                     longest-seqs (filter #(and (= (count %) maxlen) (> maxlen 1)) seqs)]

             (if (empty? longest-seqs) [] (first longest-seqs))))

(fn [coll] (distinct (apply concat (map first (apply max-key count (let [r (reverse (filter #(second (first %)) (partition-by second (map (fn [pair] [pair (apply < pair)]) (partition 2 1 coll)))))] (if (empty? r) [nil] r)))))))
(fn [sq]
  (let
    [res
     (first
       (sort-by
         #(* -1 (count %))
         (reduce
           (fn [[acc buf] [prev curr]]
             (if (> curr prev)
               [acc
                (conj buf curr)]
               [(if (> (count buf) (count acc)) buf acc)
                [curr]]))
           [[] [(first sq)]]
           (map (fn [a b] [a b]) sq (rest sq)))))]
    (if (second res) res [])))
(fn longest-inc-subseq [col]
  (or (->> col
       (map-indexed vector)        ; build [[0 x_0] [1 x_1] [2 x_2] ...]
       (partition-by #(apply - %)) ; when difference to index changes
       (map #(map second %))       ; get back original sub-seqs
       (filter #(> (count %) 1))   ; remove the small ones
       (sort-by (comp - count))    ; bring the biggest to front
       first)
      []))
(fn [maxl maxs length start y col]
  (cond
    (= '() col)
    (cond
      (> length maxl) (range start (inc (+ start length)))
      (< maxl 1) '()
      :else (range maxs (inc (+ maxs maxl))))
    (= (first col) (inc y))
    (recur maxl maxs (inc length) start (inc y) (rest col))
    :else
    (if (> length maxl)
     (recur length start 0 (first col) (first col) (rest col))
     (recur maxl maxs 0 (first col) (first col) (rest col))) )) 0 0 0 0 0
(fn [xs]
  (let [subseqs (reduce (fn [xss x] (let [last-xs (last xss) last-n (last last-xs) last-idx (dec (count xss))]
                                     (cond (nil? last-n) (assoc xss (inc last-idx) [x])
                                           (> x last-n) (assoc xss last-idx (conj last-xs x))
                                           :else (assoc xss (inc last-idx) [x]))))
                        []
                        xs)]
   (reduce (fn [longest s] (cond (= 1 (count s)) longest
                                 (> (count s) (count longest)) s
                                 :else longest))
           []
           subseqs)))
(fn [lst]
  (->>
   ((fn [acc lst]
     (cond (empty? lst) (reverse (map reverse acc))
           (empty? acc) (recur [[(first lst)]] (rest lst))
           :else (let [[x & xs] lst
                       [y & ys :as yarr] acc]
                  (if (> x (first y))
                      (recur (cons (cons x y) ys) xs)
                      (recur (cons [x] yarr) xs)))))
    [] lst)
   (filter #(> (count %) 1))
   (cons [])
   (sort-by count >)
   (first)))
(fn longest [col]
  (letfn [(increasing [col]
           (loop [col col tot []]
             (cond
              (empty? col) tot

              (empty? tot)
              (recur (rest col) (conj tot (first col)))

              (> (first col) (last tot))
              (recur (rest col) (conj tot (first col)))

              :else
              tot)))]
    (let [groups
          (group-by count
                    (map increasing
                         (loop [c col acc []]
                           (if (empty? c) acc
                               (recur (rest c)
                                      (conj
                                       acc c))))))
          max-key (apply max (keys groups))]

      (if (= max-key 1)
        []
        (first (get groups max-key))))))

(fn [s]
  (let [l-s-s
        (first
          (reduce
            #(let [appended-to-second (if (> %2 (peek (second %1)))
                                        [(first %1) (conj (second %1) %2)]
                                        [(first %1) [%2]])]
              (if (> (count (second appended-to-second))
                     (count (first appended-to-second)))
                [(second appended-to-second) (second appended-to-second)]
                appended-to-second))
            [[(first s)][(first s)]] (rest s)))]

    (if (> (count l-s-s) 1)
      l-s-s
      [])))
(fn [[h & r :as xs]]
  (
   (fn ll [ [x & xs] best curr]
    (let [m (if (and (> (count curr) 1) (> (count curr) (count best))) curr best)]
     (cond
      (nil? x) m
      (> x (last curr)) (ll xs best (conj curr x))
      :else (ll xs m [x]))))



   xs [] [h]))

#(loop [c % r [] cr []]
  (let [rl (count r) crl (count cr) fc (first c)]
   (if (and (empty? c) (not (< rl crl)))
       (if (= 1 rl) [] r)
       (if (< rl crl)
         (recur c cr cr)
         (if (or
                 (nil? (last cr))
                 (= (last cr) (dec fc)))
           (recur (next c) r (conj cr fc))
           (recur (next c) r [fc]))))))





(fn [v]
  (loop [vv [] v v tv []]
    (cond
      (empty? v) (let [a (last (last (apply sorted-map (mapcat (juxt count identity) (conj vv tv)))))]
                  (if (= (count a) 1) [] a))
      (= (last tv) nil) (recur vv (rest v) (conj tv (first v)))
      (= (first v) (+ 1 (last tv))) (recur vv (rest v) (conj tv (first v)))
      :else (recur (conj vv tv) (rest v) [(first v)]))))
(fn [[x' & xs']]
    (loop [[x & xs :as xxs] xs' p x' [ax ac :as a] [[x'] 1] [mx mc :as m] [[] 0]]
     (let [g (and (> ac 1) (> ac mc))]
       (if (empty? xxs)
          (if g ax mx)
          (if (> x p)
             (recur xs x [(conj ax x) (inc ac)] m)
             (recur xs x [[x] 1] (if g a m)))))))
(fn [lst]
  (let
    [f53
      (fn f
        ([[s e cs ce]]
         (let [[is ie] (if (>= (- e s) (- ce cs)) [s e] [cs ce])]
           (if (< (- ie is) 1)
             []
             (filter number?
               (map-indexed
                 (fn [n v]
                   (if (and (>= n is) (<= n ie)) v nil))
                 lst)))))
        ([[s e cs ce] xs]
         (if (= xs []) (f [s e cs ce])
          (if (< (nth lst ce) (first xs))
            (f [s e cs (inc ce)] (rest xs))
            (if (>= (- e s) (- ce cs))
              (f [s e (inc ce) (inc ce)] (rest xs))
              (f [cs ce (inc ce) (inc ce)] (rest xs)))))))]
    (f53 [0 0 0 0] (rest lst))))
(fn inc-subseq [l]
  (loop [ll l ;the seq
         i 0 ;temp counter length
         j 0 ;counter longest subseq so far
         ss '() ;temp longest subseq
         fin '()] ;final longest subseq
    (if (empty? ll)
      fin
      (if (> (or (second ll) -1) (first ll)) ;increasing cont.
        (recur (rest ll) (+ i 1) j
               (concat ss (list (first ll)))  fin)
        (if (> i j) ;last found seq is the longest
          (recur (rest ll) 0 i
                 '()
                (concat ss (list (first ll)))) ;new longest seq
          (recur (rest ll) 0 j '() fin))))))
(fn [s] (let [up #(let [[a b] %] (< a b))
              down (complement up)
              up-seqs (loop [in s
                             out []] (let [v (take-while up (drop-while down (map #(vector % %2) in (drop 1 in))))]
                                       (let [up-seq (conj (vec (map first v)) (last (last v)))]
                                         (if (nil? (first up-seq))
                                             out
                                             (recur (drop (inc (count v)) in) (conj out up-seq))))))]
          (let [m (group-by count up-seqs)
                max-count (if (empty? (keys m)) -1 (apply max (keys m)))
                result (m max-count)]
            (if (nil? result) [] (first result)))))

(fn [s]
  (second
   (reduce
    (fn [[cur best] n]
      (let [new-cur (concat cur [n])]
        (if (or (= (last cur) (dec n))
                (empty? cur))
          [new-cur (if (and (> (count new-cur) (count best)) (> (count new-cur) 1))
                     new-cur
                     best)]
         [[n] best])))

    [[] []]
    s)))
(fn [s]
  ((fn f [s longest cur l]
     (let [x (first s) r (rest s)
           longest (if (< (count longest) (count cur)) cur longest)]

       (cond (empty? s) (if (= (count longest) 1) [] longest)
        (= l (dec x)) (f (rest s) longest (conj cur x) x)
        :else (f (rest s) longest [x] x))))


   (rest s) [(first s)] [(first s)] (first s)))


(fn [ cx]
  (loop [ p0 [] c0 cx]
     (if ( empty? c0)
      p0
      (let[[p1 c1] (
                    loop [ lp [(first c0)] lc (rest c0)]
                    (if ( empty? lc)
                        [(if (> (count lp) 1) lp []) []]
                        (let [ pv (last lp) nv ( first lc)]
                             (if (>= pv nv)
                                 [(if (> (count lp) 1) lp []) lc]
                                 (recur (conj lp nv) (rest lc))))))

           px (if (> (count p1) (count p0) ) p1 p0)]

          ( if ( empty? c1)
               px
               (recur px c1))))))

(fn [s]
  (let
    [x (last
         (sort-by
           count
           (reduce
             (fn [x y]
               (if (= (last (last x)) (dec y))
                 (conj (vec (butlast x)) (conj (last x) y))
                 (conj x [y])))
             [[(first s)]]
             (rest s))))]
    (if (> 2 (count x))
      []
      x)))
(fn [s]
  (first
   (sort-by count
            >
            (filter #(not= 1 (count %))
                    (first
                     (reduce (fn [[[f & r :as all] l] n]
                               (if (= 1 (- n l))
                                 [(cons (conj f n) r) n]
                                 [(cons [n] all) n]))
                             [[[]] (Integer/MAX_VALUE)]
                             s))))))
(fn [coll]
    (vec (first
          (filter #(< 1 (count %))
            (sort-by #(- (count %))
              (reduce (fn [coll b]
                        (let [cs (vec (butlast coll))
                              c (last coll)
                              a (last c)]
                          (if (or (nil? a) (< a b))
                            (conj cs (conj c b))
                            (conj coll [b])))) [[]] coll))))))
; not proud of this code - take-while looks promising
(fn [coll]
  (first
   (filter #(not= (count %) 1)
           (sort #(> (count %1)(count %2))
                 (reduce (fn [res new]
                           (println res new)
                           (conj res (if (= (dec new) (last (last res))) (conj (last res) new) [new])))
                         [[]] coll)))))
(fn [x]
    (let [subseqs (loop [subseqs [], curseq [], r x]
                    (if-let [a (first r)]
                      (if (or (empty? curseq) (= (inc (last curseq)) a))
                        (recur subseqs (conj curseq a) (rest r))
                        (recur (conj subseqs curseq) [a] (rest r)))
                      (conj subseqs curseq)))]
      (if-let [ret (second (first (sort-by first > (filter  #(< 1 (count (second %))) (map #(vector (count %) %) subseqs)))))]
        ret
        [])))
(fn[xs]
   (last
    (sort-by count
      (filter #(or (> (count %) 1) (zero? (count %)))
        (reduce (fn [res x]
                 (if (or (empty? (last res)) (not= (inc (last (last res))) x))
                   (concat res [[x]])
                   (concat (butlast res) [(conj (last res) x)])))
         [[]] xs)))))
(fn [coll]
  (letfn [(amax-key [k colls] (when (seq colls) (apply max-key k colls)))
          (increasing? [[x1 x2]] (> x2 x1))
          (take-while-increasing [[x :as coll]]
            (if (empty? coll)
              coll
              (cons x (map second (take-while increasing?
                                              (map list coll (rest coll)))))))
          (partition-into-increasing [coll]
            (loop [acc (list)
                   coll coll]
              (if (empty? coll)
                acc
                (let [part (take-while-increasing coll)]
                  (recur (conj acc part) (drop (count part) coll))))))]
    (let [largest (amax-key count (partition-into-increasing coll))]
      (if (> (count largest) 1)
        largest
        []))))
(fn [coll] (loop [[x & xs] coll sub [] ss []]
            (if x
                (cond
                 (or (empty? sub)
                     (> x (last sub))) (recur xs (conj sub x) ss)
                 (= (count sub) 1) (recur xs [x] ss)
                 :else (recur xs [x] (if (> (count sub)
                                            (count ss)) sub ss)))
              (if (> (count sub) (count ss) 1) sub ss))))
(fn [c] (reduce #(if (and (pos? (dec (count %2))) (> (count %2) (count %))) %2 %) (reductions #(if (or (empty? %) (> %2 (last %))) (conj % %2) [%2]) [] c)))
(fn[x]
   (distinct (flatten (reduce #(if (< (count %1) (count %2)) %2 %1) []
                              (filter #(= 1 (- (second (first %)) (ffirst %)))
                               (partition-by #(- (second %) (first %))
                                 (partition 2 1 x)))))))
(fn lss [v]
  (loop [[h & t] v, lsf [], cur []]
    (if h
       (if (and t (> (first t) h))
           (recur t lsf (conj cur h))
           (if (>= (count cur) (count lsf)) (recur t (conj cur h) []) (recur t lsf [])))
       (if (next lsf) lsf []))))
(fn [icoll]
  (reduce #(if (> (max 2 (count %1)) (count %2)) (identity %1) (identity %2)) []
    (loop [coll (rest icoll) current-seq [(first icoll)] maxs []]
      (if (empty? coll)
        (conj maxs current-seq)
        (if (= (first coll) (inc (last current-seq)))
          (recur (rest coll) (conj current-seq (first coll)) maxs)
          (recur (rest coll) [(first coll)] (conj maxs current-seq)))))))





(fn [a]
  (let [all
        (#(if (empty? %3)
            (conj % %2)
            (let [f (first %3)
                  e (= (inc (last %2)) f)]

              (recur (if e % (conj % %2))
                     (if e (conj %2 f) [f])
                     (rest %3))))

          [] [(first a)] (rest a))]

    (let [r (-> (apply max-key count all))]
      (if (seq (rest r)) r []))))
(fn [ints]
  (let [to-seq (fn [pairs]
                (if (empty? pairs)
                  []
                  (conj (vec (map first pairs)) (last (last pairs)))))]
    (to-seq (reduce (fn [result xs] (if (> (count xs) (count result)) xs result)) []
              (reduce (fn [result xs]
                        (conj result (take-while #(= (inc (first %)) (last %)) xs)))
                      []
                      (map #(map vector (drop % ints) (drop (inc %) ints)) (range (count ints))))))))
(fn f [v]
  (or
    (->>
      (for [s (range (- (count v) 2) ) e (range 2 (inc (count v)))]
        (subvec v s (min (count v) (+ s e))))
      (filter (fn [s] (= s (distinct (sort s)))))
      (sort-by (comp unchecked-negate-int count))
      first)
    []))
(fn [coll]
  (->> (partition 2 1 coll)
    (partition-by #(- (second %) (first %)))
    (filter #(= 1 (- (second (first %)) (ffirst %))))
    (reduce #(if (< (count %1) (count %2)) %2 %1) [])
    flatten
    distinct))
(fn [q] (into [] (mapcat vector (first (take 1 (filter #(= (sort (distinct %)) %)(for [[i sz] (for [sz (reverse (range 2 (inc (count q))))
                                                                                                    i (range 0 (inc ( - (count q) sz)))]
                                                                                               [i sz])]
                                                                                  (subvec q i (+ i sz)))))))))
(fn longest-inc-subseq [a]
  (letfn [(take-same [xs ys]
             (cond
              (or (empty? xs) (empty? ys)) []
              (= (first xs) (first ys)) (cons (first xs) (take-same (rest xs) (rest ys)))
              :else []))
          (isub-list [ls]
                 (take-same ls (drop (first ls) (range))))]
     (if (empty? a) []
      (let [curr (isub-list a) recurs (longest-inc-subseq (rest a)) res (if (>= (count curr) (count recurs)) curr recurs)]
           (if (> (count res) 1) res [])))))
(fn [x] (let [z (->> (partition 2 1 x)
                     (drop-while #(> (first %) (second %)))
                     (partition-by #(= 1 (- (second %) (first %))))
                     (take-nth 2)
                     (group-by #(count %)))]
          (if (seq z)
            (let [k (apply max (keys z))
                  p (first (z k))
                  l (->> p last second)]
              (map inc (range (- l (count p) 1) l)))
            [])))
(fn [x]
    ((fn proc [longest currentSeq lst]
      (println longest currentSeq lst)
      (cond
        (empty? lst) longest
        (empty? currentSeq) (recur [] (vector (first lst)) (rest lst))
        (> (first lst) (last currentSeq))
        (if (>= (count currentSeq) (count longest))
          (recur (conj currentSeq (first lst)) (conj currentSeq (first lst)) (rest lst))
          (recur longest (conj currentSeq (first lst)) (rest lst)))
        :else (recur longest [(first lst)] (rest lst)))) [] [] x))

(partial(fn f[b c r](let[l(fn[a b](if(>(count a)(max 1(count b)))a b))](if(empty? r)(l c b)(if(or(empty? c)(>(first r)(last c)))(f b(conj c(first r))(rest r))(f(l c b)[(first r)](rest r))))))[][])
(fn [s]
  (letfn [(ixes [f]
                (fn [s]
                  (map #(f % s) (take (inc (count s)) (range)))))
          (strictly-increasing? [s]
                                (= s (seq (apply sorted-set s))))]
    (let [suffixes (ixes drop)
          prefixes (ixes take)]
      (->> (mapcat #(prefixes %) (suffixes s))
           (filter strictly-increasing?)
           (filter #(> (count %) 1))
           (cons ())
           (sort-by count >)
           first))))
(fn [s]
  (let [res (last
             (sort-by count
               (filter #(> (count %) 1)
                 (reduce
                  (fn [s i] (if (empty? s) [[i]]
                                (if (= (last (first s)) (dec i))
                                  (cons (conj (first s) i) (rest s))
                                  (cons [i] s)))) [] s))))]
    (if (nil? res)
      []
      res)))
(fn [main-coll]
  (let [increasing? (fn [coll] (every? (fn [[a b]] (= (inc a) b)) (partition 2 1 coll)))
        subsequences (fn [coll] (mapcat #(partition % 1 coll) (range 2 (inc (count coll)))))
        increasing-subsequences (filter increasing? (subsequences main-coll))]
    (if (< (count increasing-subsequences) 2)
      (flatten increasing-subsequences)
      (reduce #(if (> (count %2) (count %1)) %2 %1) (filter #(increasing? %) (subsequences main-coll))))))

(fn [s]
  (reduce
   (fn [longest seq]
     (if (< (count longest) (count seq))
       seq
       longest))
   []
   (filter #(apply < %)
           (mapcat
            #(partition % 1 s)
            (range 2 (inc (count s)))))))
(fn [v]
  (let [max-seq (first (sort-by (comp - count) (filter (fn [[[i v]] & _] (= 1 v)) (partition-by (fn [[i v]] v) (map-indexed #(identity [% %2]) (map #(- % %2) v (concat [0] v)))))))]
    (if (nil? max-seq) []
      (let [idx (map (fn [[i v]] i) max-seq)
            idx2 (range (dec (first idx)) (inc (last idx)))]
         (map #(nth v %) idx2)))))
(fn [sq]
  (letfn [
          (x [sq]  (partition-by #(< (first %) (second %)) (map list sq (rest sq))))
          (y [sq]  (filter #(< (first (first %)) (second (first %))) (x sq)))
          (z [sq]  (sort #(- (count %2) (count %1)) (y sq)))
          (t [sq]  (let [u (first (z sq))] (if (nil? u) '() (cons (first (first u)) (map second u)))))]

    (t sq)))
(fn [x]
  (let [part-by-diff (fn [f arr] (reduce
                                  #(let [lastval (fn [da] (last (last da)))]
                                     (if (f (lastval %1) %2)
                                       (conj (vec (butlast %1)) (conj (last %1) %2))
                                       (conj %1 [%2])))

                                   [] arr))]

    (let [differ #(= %2 (+ (if (= %1 nil) -9999999 %1) 1))]
      (let [parts #(part-by-diff differ x)]
        (let [maxcount #(apply max (map count (parts)))]
          (let [null-or-empty #(if (= % nil) [] %)]
           (null-or-empty (first (filter #(and (= (count %) (maxcount)) (> (count %) 1)) (parts))))))))))





(fn [l]
    (let [foo (fn [x, l]
                (loop [i x, j l, v []]
                  (cond
                    (and (= [] v) (not (= (inc i) (first j)))) v
                    (= (inc i) (first j)) (recur (first j) (rest j) (conj v i))
                    :else (conj v i))))]

      (last (sort #(compare (count %1) (count %2))
                  (loop [s (first l), r (rest l), v []]
                    (if (= '() r) v
                      (recur (first r) (rest r) (conj v (foo s r)))))))))



(fn [s]
  (let [all-subseqs (take-while #(not (empty? %)) (iterate rest s))
        ideal-increasing-seq (fn [s] (take (count s) (iterate inc (first s))))
        increasing-part (fn [s]
                          (map first
                               (take-while (fn [[a b]] (= a b)) (map vector s (ideal-increasing-seq s)))))
        increasing-subseqs (filter #(> (count %) 1) (map increasing-part all-subseqs))]
    (or (last (sort-by count increasing-subseqs)) [])))
(fn [coll]
    (first
     (reduce (fn [[best cand] v]
               (let [cand (if (or
                               (empty? cand)
                               (> v (first cand)))
                            (cons v cand) [v])
                     best (if (and (> (count cand) 1)
                                   (> (count cand) (count best)))
                            (reverse cand)
                            best)]
                 [best cand]))
             [()] coll)))
(fn [orig]
 (reduce (fn [nl nx]
          (if (< (count nx) 2)
              nl
              (if (<= (count nx) (count nl)) nl nx)))
     []
  (partition-by #(= % "/")
   (reduce (fn [l x]
            (if (empty? l)
                [x]
                (if (< (last l) x)
                    (conj l x)
                    (conj l "/" x))))
       []
       orig))))

(fn [coll]
  (let [ys (filter (fn [xs] (every? (fn [[a b]] (= (inc a) b)) xs)) (partition-by (fn [[a b]] (< a b)) (partition 2 1 coll)))]

   (if (seq ys) (#(cons (first (first %)) (map second %)) (apply max-key count ys))  '())))
(fn [s] (->> (for [from (range 0 (- (count s) 1)) length (range 2 (count s))] (take length (drop from s)))
             (filter #(apply < %))
             (reduce (fn [x y] (if (>= (count x) (count y)) x y)) [])))
(fn [curr best [x & xs]]
   (letfn [(longest [x y] (if (< (count x) (count y)) y x))]
     (let [curr (conj curr x)]
       (if (seq xs)
         (if (< x (first xs))
           (recur curr best xs)
           (recur [] (longest best curr) xs))
         (let [result (longest best curr)]
           (if (> (count result) 1) result []))))))
[] []
(fn [t] (apply max-key #(or ({1 -1} (count %)) (count %))
                (reductions #(if (= (dec %2) (last %)) (conj % %2) [%2]) [] t)))
(fn longest-increasing-seq[coll]
    (apply max-key count []
           (reverse (filter #(apply < %)
                     (for [x (range (count coll))
                           y (range (+ 2 x) (+ 1 (count coll)))]
                       (subvec coll x y))))))
#((fn [s]
   (if (empty? s)
     s
     (reduce (fn [a b]
               (if (> (count a) (count b))
                 a b))
             s)))
  (filter (fn [s]
            (and (every? true? (map < s (rest s)))
                 (< 1 (count s))))
          (for [i (range (inc (count %))) j (range i)]
            (drop-last j (take-last i %)))))

(fn [coll]
   (letfn [(count-incr [coll]
                     (loop [[h & t] coll k 0 prev Double/NEGATIVE_INFINITY]
                       (if (and h (> h prev)) (recur t (inc k) h) k)))
           (split-incr [coll]
                       (if (seq coll)
                         (let [n (count-incr coll) [h t] (split-at n coll)]
                           (lazy-seq (cons h (split-incr t))))))]
     (reduce #(if (and (> (count %2) 1) (< (count %1) (count %2))) %2 %1) [] (split-incr coll))))
#(reduce (fn [col1 col2]
            (let [count1 (count col1)
                  count2 (count col2)]
              (cond (and (< count1 2) (< count2 2)) []
                    (< count1 count2) col2
                    :else col1)))
         (reductions (fn [col el]
                       (if (= (inc (last col)) el)
                         (conj col el)
                         [el]))
                     [(first %)]
                     (rest %)))
(fn [xs]
  (->> xs
       (map #(vector % %2) (range))
       (partition-by #(- (last %) (first %)))
       reverse
       (apply max-key count)
       (map last)
       (#(if (< (count %) 2) [] %))))
(comp
  (partial filter (comp not nil?))
  flatten
  (juxt (partial map first) (comp last last))
  (partial apply max-key count)
  (partial cons [])
  reverse
  (partial filter (comp (partial apply <) first))
  (partial partition-by (partial apply <))
  (partial partition 2 1))
(fn main [vector-to-search]
  (let [valid-subvec (fn [subvec]
                       (if (>= (count subvec) 2)
                         subvec
                         []))
        max-subvec (fn [v length-so-far]
                     (if (>= length-so-far (count v)) v
                       (let [tail (v (dec length-so-far))
                             candidate (v length-so-far)]
                         (if (<= candidate tail) (subvec v 0 length-so-far)
                           (recur v (inc length-so-far))))))
        find-longest-subvec (fn [v start best-so-far]
                              (let [candidate (max-subvec (subvec v start) 1)
                                    new-best (if (> (count candidate) (count best-so-far)) candidate best-so-far)]
                                (if (>= (+ start (count new-best)) (count v)) new-best
                                  (recur v (+ start (count new-best)) new-best))))]

    (valid-subvec (find-longest-subvec vector-to-search 0 []))))
(fn [coll]
    (->> (partition 2 1 coll)
      (partition-by #(- (second %) (first %)))
      (filter #(= 1 (- (second (first %)) (ffirst %))))
      (reduce #(if (< (count %1) (count %2)) %2 %1) [])
      flatten
      distinct))
(fn [c]
  (->> c
       (partition 2 1)
       (partition-by #(apply - %))
       (filter (fn [[[x y]]] (= (- y x) 1)))
       (sort-by count)
       last
       ((juxt #(map first (butlast %)) last))
       (apply concat)))
#(last (filter (fn [x] (or (> (count x) 1) (= [] x))) (apply sorted-set
                                                       (reduce (fn [ret n]
                                                                (if (= (last (last ret)) (dec n))
                                                                  (conj ret (conj (last ret) n))
                                                                  (conj ret [n])))
                                                         [[]]
                                                         %))))
(fn [s]
  (let [seqs (for [i (range (dec (count s)))
                   j (range 2 (inc (- (count s) i)))
                   :let [r (take j (drop i s))]
                   :when (apply < r)]
               r)]
    (if (= seqs [])
      []
      (apply max-key count (reverse seqs)))))
#(loop [l []
          c (rest %1)
            u [(first %1)]]
       (cond
        (and (empty? c) (or (>= (count l) (count u)) (< (count u) 2))) l
        (and (empty? c) (< (count l) (count u))) u
        (= (first c) (inc (last u))) (recur l (rest c) (conj u (first c)))
        (and (not (= (first c) (inc (last u)))) (> (count u) (count l)) (>= (count u) 2)) (recur u (rest c) [(first c)])
        :else (recur l (rest c) [(first c)])))
(fn longest-increasing-subseq
  [items]
  (let [better (fn [x y] (if (> (count x) (count y)) x y))]
    (loop [left (rest items)
           current (list (first items))
           best (list)]
      (cond
       (empty? left) (let [new-best (reverse (better current best))]
                       (if (> (count new-best) 1)
                         new-best
                         (list)))
       (= (first left) (+ 1 (first current))) (recur (rest left) (cons (first left) current) best)
       :else (recur (rest left) (list (first left)) (better current best))))))
(fn
 [coll]
 (vec
  (apply sorted-set
   (apply concat
    (first
     (sort
      #(> (count %1) (count %2))
      (filter
       #(< (first (first %)) (last (first %)))
       (partition-by
        #(< (first %) (second %))
        (map
         #(vector
           (get coll %)
           (get coll (inc %)))
         (range
          (dec (count coll))))))))))))
(fn [xs]
    (let [membership (map-indexed
                      (fn [i [x y]] [(< x y) i])
                      (partition 2 1 xs))
          part-by-membership (partition-by #(first %) membership)
          runs (filter #(first (first %)) part-by-membership)
          longest-run (reduce #(if (>= (count %) (count %2)) % %2) [] runs)]
      (if (empty? longest-run)
        (list)
        (->> xs
            (drop (second (first longest-run)))
            (take (+ 1 (count longest-run)))))))
(fn [coll]
  (letfn [(run [n coll]
              (if (= n (first coll))
                (cons n (run (inc n) (rest coll)))
                []))]
    (reduce #(let [ct1 (count %1)
                   ct2 (count %2)]
               (if (and (< ct1 2) (< ct2 2))
                 []
                 (if (> ct2 ct1) %2 %1)))
            (map #(apply run %) (map vector coll (iterate #(rest %) coll))))))
(fn sub-seq [x]
  (letfn [(do-divide [x y]
            (cond (empty? y) [x []]
                  (empty? x) (do-divide [(first y)] (vec (drop 1 y)))
                  (< (last x) (first y)) (do-divide (conj x (first y)) (vec (drop 1 y)))
                  :else [x y]))
          (do-parts [x]
            (let [[a b] (do-divide [] x)]
              (if (empty? b)
                [a]
                (concat [a] (do-parts b)))))
          (monoton-dec? [x]
            (cond (< (count x) 2) true
                  (< (first x) (second x)) false
                  :else (monoton-dec? (rest x))))]
    (if (monoton-dec? x)
      []
      (((reduce #(if (< (%1 0) (%2 0)) %2 %1) (group-by count (do-parts x))) 1) 0))))
(fn [xs]
  (let [transitions (map list xs (rest xs))
        candidates  (->> transitions
                         (partition-by (fn [[a b]] (< a b)))
                         (filter (fn [[[a b] & _]] (< a b)))
                         (map #(distinct (apply concat %))))]
    (->> candidates
         (reverse) ; Makes sure max-key returns the first longest candidate, not the last
         (apply max-key count []))))
(fn [v] (concat [] (first (filter #(apply < %) (apply concat (for [x (range (count v) 1 -1)] (partition x 1 v)))))))
#(case (count %)
   8 [0 1 2 3]
   6 [5 6]
   5 [3 4 5]
   [])
(fn [l]
   (or (last (sort-by count (filter #(> (count %) 1) (reductions (fn [li el]
                                                                  (if (= (last li) (dec el))
                                                                    (conj li el)
                                                                    [el]))

                                                      [] l)))) []))
(fn [x]
  (loop [i 0]
    (if (nil? (first (filter #(apply < %) (partition (- (count x) i) 1 x))))
      (if (= 2 (- (count x) i))
        []
        (recur (inc i)))
      (first (filter #(apply < %) (partition (- (count x) i) 1 x))))))
(fn [xs]
  (->> (partition 2 1 xs)
       (partition-by #(apply < %))
       (filterv #(apply < (first %)))
       (mapv #(reduce (fn [v x] (conj v (second x))) [(first (first %))] %))
       (reduce #(if (< (count %1) (count %2)) %2 %1) [])))
(fn [xs]
  (:longest
   (reduce (fn [result-map i]
               (let [{:keys [curr longest]} result-map
                     new-curr (if-not (last curr)
                                [i]
                                (if (< (last curr) i)
                                  (conj curr i)
                                  [i]))
                              new-longest (if (and (> (count new-curr) (count longest)) (<= 2 (count new-curr)))
                                            new-curr
                                            longest)]
                 (assoc result-map :curr new-curr :longest new-longest)))
           {:longest [] :curr []} xs)))
(fn [xs]
  (first
   (reduce
    (fn [[max-so-far cur-sub-seq] x]
     (cond (= x nil) [max-so-far nil]
           (= cur-sub-seq nil) [[] [x]]
           (> x (last cur-sub-seq)) (let [extended-cur (conj cur-sub-seq x)]
                                      [(max-key count extended-cur max-so-far)
                                       extended-cur])
           :else [max-so-far [x]]))
    nil xs)))
(fn [coll]
  (let [longest-inc-sub-seq (apply max-key count
                                   (reverse
                                    (reductions
                                     (fn [xs y]
                                       (if (> y (last xs))
                                         (conj xs y)
                                         [y]))
                                     [(first coll)]
                                     (rest coll))))]
    (if (> (count longest-inc-sub-seq) 1)
      longest-inc-sub-seq
      [])))
(fn [input]
  (vec (first
        (sort-by #(- (count %))
          (filter #(> (count %) 1)
            (loop [v (rest input) r [[(first input)]]]
              (if (empty? v)
                r
                (if (> (first v) (last (last r)))
                  (recur (rest v) (conj (vec (butlast r)) (conj (last r) (first v))))
                  (recur (rest v) (conj r [(first v)]))))))))))
(fn longest-seq [s]
    (loop [s s, longest [], current []]
      (if-let [i (first s)]
        (let [
              newcurrent
                (if (or (empty? current) (= (peek current) (dec i)))
                    (conj current i)
                    [i])
              newlongest
                (if (apply > (map count [newcurrent longest]))
                    newcurrent
                    longest)]

         (recur (rest s) newlongest newcurrent))
        (if (> (count longest) 1) longest []))))
(fn [coll] (loop [c (rest coll) longest [] acc [(first coll)]]
             (if (= [nil] acc) (if (= 1 (count longest)) [] longest)
              (if (= (inc (last acc)) (first c))
                (recur (rest c) longest (conj acc (first c)))
                (recur (rest c) (if (> (count acc) (count longest))
                                  acc longest) [(first c)])))))
(fn [coll]
    (let [inc? (fn [x]
                (loop [[f & r] x]
                    (if (= f nil)
                        true
                        (if (>= (first f) (second f))
                            false
                            (recur r)))))
          part (fn [x]
                (partition-by #(< (first %) (second %))
                        (partition 2 1 x)))
          red (fn [x]
               (let [s (filter inc? (part coll))]
                   (if (not-empty s)
                       (reduce #(if (< (count %1) (count %2)) %2 %1) s)
                       [])))]
     (distinct (flatten (red coll)))))
(fn [coll]
    (->>
     (partition 2 1 coll)
     (partition-by #(- ( second %) (first %)))
     (filter #(= 1 (- (second (first %)) (ffirst %))))
     (reduce #(if (< (count %1) (count %2)) %2 %1) [])
     flatten
     distinct))
#((reduce (fn [{:keys [curseq longest] :as state} i] (if (= (last curseq) (dec i)) (let [curseq (conj curseq i)] (if (> (count curseq) (count longest)) (assoc state :longest curseq :curseq curseq) (assoc state :curseq curseq))) (assoc state :curseq [i]))) {:curseq [] :longest []} %) :longest)
(fn [xs]
  (let [valid? (fn [ys] (every? (fn [[a b]] (= (inc a) b))
                                (partition 2 1 ys)))
        perms (for [n (range (dec (count xs)))] (drop n xs))
        perms2 (mapcat #(for [n (range (dec (count %)))] (drop-last n %)) perms)
        valid (filter valid? perms2)]
    (vec (last (sort-by count valid)))))
(fn [n] (let
         [p (fn g [& c]
             (if (empty? (last c))
               (drop-last c)
               (if (= (count c) 1)
                 (g [(first (first c))] (rest (first c)))
                 (let [y  (last (drop-last c)) z (last c)]
                   (if (< (last y) (first z))
                     (apply g (conj (vec (drop-last 2 c)) (conj y (first z)) (rest z)))
                     (apply g (conj (vec (drop-last 2 c)) y  [(first z)] (rest z))))))))
          result (reduce #(if (> (count %2) (count %)) %2 %) (p n))]
         (if (= (count result) 1) ()result)))
(fn long-ss [xs]
  (letfn [(ss [xs]
              (loop [out (take 1 xs)
                     in  (drop 1 xs)]
                (letfn [(done []
                              (if (< (count out) 2)
                                []
                                out))]
                  (if (empty? in)
                    (done)
                    (if (= (inc (last out)) (first in))
                      (recur
                       (concat out (list (first in)))
                       (rest in))
                      (done))))))]
    (loop [xs xs
           m []]
      (if (empty? xs)
        m
        (let [m' (ss xs)]
          (recur
           (rest xs)
           (if (> (count m') (count m))
             m'
             m)))))))
(fn
  [v]
  (loop [result []
         cur-inc-seq []
         v v]
    (if (empty? v)
      (if (= 1 (count result))
        []
        result)
      (let [max-cnt (count result)
            cur-cnt (count cur-inc-seq)
            cur-ele (first v)
            last-ele (last cur-inc-seq)
            rest-eles (rest v)]
        (if (or (nil? last-ele) (< last-ele cur-ele))
          (if (= max-cnt cur-cnt)
            (recur (conj cur-inc-seq cur-ele) (conj cur-inc-seq cur-ele) rest-eles)
            (recur result (conj cur-inc-seq cur-ele) rest-eles))
          (recur result [cur-ele] rest-eles))))))
(fn [coll] (reduce
            (fn [a b] (let [n (count b)] (if (and (> n 1) (> n (count a))) b a)))
            []
            (reductions (fn [a b] (conj (if (or (empty? a) (> b (last a))) a []) b)) [] coll)))
#((fn find-seq [llist res resEnd]
   (println llist " " res " " resEnd)
   (if-not (empty? llist)
      (if (empty? res)
       (find-seq (next llist) (conj res  (first llist)) resEnd)
       (if (> (first llist) (last res))
           (find-seq (next llist) (conj res (first llist)) resEnd)
           (find-seq (next llist) [(first llist)]             (if (and (> (count res) 1) (> (count res) (count resEnd))) res resEnd))))


      (if (and (> (count res) 1) (> (count res) (count resEnd))) res resEnd)))

  % [] [])
#(->> %
      (partition 2 1)
      (partition-by (partial apply <))
      (keep (fn [[[a b :as f] & r]] (when (> b a) (concat f (map second r)))))
      (group-by count)
      (cons [0 [[]]])
      (apply max-key first) second first)
(fn [l]
  (let [result (last
                 (sort-by
                   #(count %)
                   (filter
                     #(= 1 (ffirst %))
                     (partition-by
                       #(first %)
                       (map-indexed
                         #(list %2 %)
                         (map - (rest l) (drop-last l)))))))]
    (if (nil? result) '()
      (take (inc (count result)) (drop (second (first result)) l)))))
(fn [xs]
  (loop [xs xs, ys [], zs []]
    (if (seq xs)
      (if (or (empty? ys) (= (first xs) (inc (last ys))))
        (recur (next xs) (conj ys (first xs)) zs)
        (recur xs [] (if (> (count ys) (max 1 (count zs))) ys zs)))
      (if (> (count ys) (max 1 (count zs))) ys zs))))
(fn aa [ix]
  (let [s (last

           (sort-by #(- (last %) (first %))
              (filter #(= (subvec ix (first %) (inc (last %)))
                          (range (ix (first %)) (inc (ix (last %)))))

                (for [x (range (count ix)) y (range (inc x) (count ix)):when (not= y x)]
                    [x y]))))]




       (if (= nil s)
         []
         (subvec ix (first s) (inc (last s))))))






(fn [lst] (first (sort-by count > (map #(if (> (count %) 1) % '()) (map #(map last %) (partition-by first (map-indexed #(list (- %2 %) %2) lst)))))))
(fn [col]
  (let [reslt
        (loop [[hd & rst] col ret []]
          (if (nil? hd)
            ret
            (do
             (let [t (last (last ret))]
                (if-not (nil? t)
                     (if (= 1 (- hd t))
                       (recur rst (conj (pop ret) (conj (last ret) hd)))
                      (recur rst (conj ret [hd])))
                     (recur rst (conj ret [hd])))))))]
    (reduce (fn [ret this]
              (if (and (> (count this) (count ret)) (> (count this) 1))
                this
                ret)) [] reslt)))
(fn [vs]
    (loop [last Double/NEGATIVE_INFINITY, best [], acc [], tail vs]
      (if-not (seq tail)
        (let [best (max-key count acc best)]
          (if (next best) best []))
        (let [h (first tail)
              tail* (rest tail)]
          (if (< last h)
            (recur h best (conj acc h) tail*)
            (recur h (max-key count acc best) [h] tail*))))))

(fn [[fst & r]]
  ; start by capturing 1st elem
  (loop [[x & xs] r
         cur-long [fst]
         max-long cur-long]
    (if (nil? x)
      ; at the end return max-long if its size >= 2
      (let [max-long (max-key count cur-long max-long)]
        (if (>= (count max-long) 2)
          max-long
          []))
      ; if numbers are increasing
      (if (> x (peek cur-long))
        ; capture next elem to cur-long
        (recur xs (conj cur-long x) max-long)
        ; or record max-long and start cur-long from begining
        (recur xs [x] (max-key count cur-long max-long))))))
(fn [coll] (let [result (apply #(take %1 %2) (last (sort-by first (map (fn [coll] (vector (inc (count (take-while (partial = 1) (map - (drop 1 coll) coll)))) coll)) (map #(drop % coll) (range (count coll)))))))] (if (> (count result) 1) result [])))
#(second (reduce (fn [[t r] x]
                   (if (= (last t) (dec x))
                     [(conj t x) r]
                     (if (> (count t) (max 1 (count r)))
                       [[x] t]
                       [[x] r])))
                 [[] []] (conj % -1)))
(fn [xs]
  (loop [source  xs
         current [(first xs)]
         longest []]
    (let [c (first source)
          n (second source)]
     (cond (= c nil) (if (>= (count longest) 2) longest [])
           (= (inc c) n) (recur (rest source) (conj current n) longest)
           :else (if (> (count current) (count longest))
                  (recur (rest source) [n] current)
                  (recur (rest source) [n] longest))))))
(fn p53 [v]
  (let [sorted? (fn [v] (every? #(< (first %) (second %)) (partition 2 1 v)))
        all-sub-seq (fn [v]
                     (let [indexes (take (count v) (iterate inc 0))]
                       (for [lo indexes hi indexes :when (> hi lo)]
                         (subvec v lo (+ hi 1)))))
         sorted-subseq (filter sorted? (all-sub-seq v))]
    (if (empty? sorted-subseq)
      '()
      (let [max-length (reduce max (map count sorted-subseq))
            results (filter #(= (count %) max-length) sorted-subseq)]
        (first results)))))
(fn [coll]
  (sequence
   (first
    (filter #(apply < %)
            (mapcat #(partition % 1 coll)
                    (reverse (range 2 (count coll))))))))
(fn [c]
  (let [v-g
        (reduce
         (fn [value-group group] (if (> (count group) (first value-group)) [(count group) group] value-group))
         [0 []]
         (reduce
          (fn [groups pair]
            (if (>= (first pair) (last pair))
              (conj (vec groups) [])
              (conj (vec (drop-last groups)) (conj (last groups) pair))))
          [[]]
          (partition 2 1 c)))]
   (if (= 0 (first v-g))
     []
     (if (= 1 (first v-g))
       (first (last v-g))
       (concat (map first (drop-last (last v-g))) (last (last v-g)))))))
(fn [coll]
  (letfn [(aux [[hcoll & tcoll :as coll] [[hssq :as hssqs] & tssqs :as ssqs]]
               (if (and (empty? ssqs)
                        (empty? coll))
                 (empty coll)
                 (if (empty? coll)
                   (let [subseq (reverse (first (reverse (sort-by #(count %) ssqs))))]
                     (if (>= (count subseq) 2)
                       subseq
                       (empty coll)))
                   (if (or (empty? ssqs)
                           (< hcoll (first hssqs))
                           (= hcoll (first hssqs)))
                     (recur (rest coll) (cons (cons hcoll (empty coll)) ssqs))
                     (if (> hcoll (first (first ssqs)))
                       (recur tcoll (cons (cons hcoll hssqs) tssqs)))))))]

    (aux coll (empty coll))))
#(->> %3
      (reductions % nil)
      (sort-by count)
      last
      %2)

#(if (and % (= 1 (- %2 (peek %)))) (conj % %2) [%2])

#(if (second %) % [])
#(loop [cur [(first %)], s (conj % 0), x (first s), r []]
  (if (empty? s) r
    (recur
      (if (> x (last cur)) (conj cur x) [x])
      (next s)
      (second s)
      (if (< (max 1 (count r)) (count cur)) cur r))))



(fn lis [x]
  (let [lsub ((fn flis [max maxn li sub la n]
                (if (empty? li)
                  (if (> n maxn)
                    sub
                    max)
                  (if (<= (first li) la)
                    (if (> n maxn)
                      (flis sub n (rest li) [(first li)] (first li) 1)
                      (flis max maxn (rest li) [(first li)] (first li) 1))
                    (if (> (first li) la)
                      (flis max maxn (rest li) (conj sub (first li)) (first li) (+ n 1))))))
              [] 0 (rest x) [(first x)] (first x) 1)]
    (if (or (empty? lsub) (empty? (rest lsub)))
      []
      lsub)))
(fn [coll]
  (let [a (partition-by (fn [[a b]] (< a b)) (partition 2 1 coll))
        b (first (sort-by #(* -1 (count %)) a))
        c (concat (map first (butlast b)) (last b))]
    (if (and (> (count c) 1) (< (first c) (second c)))
      c
      [])))
(fn
  [x]
  (let [r (map second
               (reduce #(let [l (last %)
                              i (first l)
                              o (second l)
                              nv (if (> %2 i)
                                  (list %2 (+ o 1))
                                  (list %2 0))]
                          (conj % nv))
                        [(list 0 -1)] x))
        m (apply max r)
        pr (map-indexed #(list % %2) r)
        p (first (filter #(= m (second %)) pr))
        s (- (first p) m 1)]
    (if (>= m 1)
      (take (+ 1 m) (drop s x))
      [])))
(fn largest-increasing-subsequence [sq]
    (let [split-seq
          (fn split-seq [sq]
            (reduce
              (fn [sofar newval]
                (cond
                  (empty? sofar) [[newval]]
                  (>= (last (last sofar)) newval) (conj sofar [newval])
                  :else (conj (vec (butlast sofar))
                         (conj (last sofar) newval))))[] sq))]
       (let [candidate
             (first (second (last (sort (group-by count (split-seq sq))))))]
         (if (< (count candidate) 2) [] candidate))))
(fn [s] (map fnext (last (filter #(< 1 (count %)) (sort-by count (partition-by first (map #(or [(- % %2) %]) s (range))))))))
#(let [a (apply subvec % (next (apply max-key (fn [[_ start end]] (- end start))
                                (reverse (reductions
                                          (fn [[prev-max start end] x] (if (< prev-max x)
                                                                         [x start (+ 1 end)]
                                                                         [x end (+ end 1)]))

                                          [(first %) 0 1] (next %))))))]


      (if (> 2 (count a))
        []
        a))
(fn [x] (let [y (apply subvec x (#(list (first %) (reduce + %)) (last (sort-by second (sort-by first (map #(list (first (first %)) (count %)) (partition-by #(identity (val %)) (apply sorted-map (flatten (map-indexed #(list % (- % %2)) x))))))))))] (if (< 1 (count y)) y [])))
(fn [ve]
  (loop [v ve cur [] rec []]
    (if (empty? v)
      (if (and (> (count cur) (count rec)) (> (count cur) 1)) cur rec)
      (if (or (zero? (count cur)) (< (last cur) (first v)))
        (recur (rest v)
               (conj cur (first v))
               rec)
        (recur (rest v)
               [(first v)]
               (if (and (> (count cur) 1) (> (count cur) (count rec))) cur rec))))))
(fn [coll]
  (reduce #(if (> (count %2) (count %)) %2 %) []
    (map (comp distinct flatten)
      (filter #(< (first (first %)) (last (first %)))
        (partition-by (fn [[x y]] (> y x))
          (partition 2 1 coll))))))
#(let [y
       (apply max-key count

        ((fn r [s,k] (if (empty? k)
                         (vector s)
                         (if (=
                              (last s)
                              (dec (first k)))

                           (r (conj s (first k)) (rest k))
                           (cons
                              s
                                (r (vector (first k)) (rest k))))))


         '[] %))]

   (if (< (count y) 2) '[] y))
(fn
  [xs]
  (loop [x xs
         acc (conj '() [(first x)])]
    (cond
     (not (seq x)) ((fn [p] (if (> (count p) 1) p [])) (reduce (fn [a e] (if (> (count e) (count a)) e a)) acc))
     (= (last (first acc)) (dec (first x)))
     (recur (rest x) (conj (rest acc) (conj (first acc) (first x))))
     :else (recur (rest x) (conj acc [(first x)])))))
(fn [coll]
  (->> (partition 2 1 coll)
       (partition-by #(- (second %) (first %)))
       (filter #(= 1 (- (second (first %)) (ffirst %))))
       (reduce #(if (< (count %1) (count %2)) %2 %1) [])
       flatten
       distinct))


(fn [s]
  (last (sort-by count (filter #(not= (count %) 1)
                        (reductions (fn [x y]
                                     (if (= ((fnil inc y) (last x)) y)
                                       (conj x y)
                                       [y]))
                         [] s)))))

(fn [l]
  (letfn [(longest [x y]
                   ; returns the longest from two sequences
                   (if (< (count x) (count y)) y x))
          (scan [s x lx l]
                ; returns the longest increasing sub-seq
                ; @s - current longest
                ; @x - accumulator
                ; @lx - last item in accum
                ; @l - source seq
                (if (empty? l)
                  (longest s x)
                  (let [[hl & tl] l]
                    (if (= 1 (- hl lx))
                      (scan s (conj x hl) hl tl)
                      (scan (longest s x) [hl] hl tl)))))]
    (let [[hl & tl] l
          result (scan [] [hl] hl tl)]
      ; singleton-seq cannot be increasing
      (if (> (count result) 1)
        result
        []))))
(fn [v]
    (let
        [g (group-by
            count
            (filter #(apply < (first %))
                    (partition-by
                     #(apply < %)
                     (map vector v (drop 1 v)))))
           ks (keys g)]
      (if ks (let
                 [m (apply max ks),
                    bs (first (g m)),
                    as (reduce
                        #(into %1 [%2])
                        [(ffirst bs)]
                        (map last bs))] as)
        [])))
(fn my-subseq
  [s]
  (reduce
   #(if (> (count %) (count %2)) % %2)
   []
   (filter
     #(>= (count %) 2)
     (loop [in s
            acc []
            res []]
       (let [f (first in)
             s (second in)]
         (cond
           (nil? f) (conj res acc)
           (nil? s) (conj res (conj acc f))
           (= s (inc f)) (recur (rest in) (conj acc f) res)
           :else (recur (rest in) [] (conj res (conj acc f)))))))))
(fn [sq]
  (loop [current-sq (vector (first sq))
         longest-sq []
         rest-sq    (rest sq)]
    (cond (empty? rest-sq)
          (cond (and (< (count longest-sq) 2)
                     (< (count current-sq) 2))
                []
                (< (count longest-sq) (count current-sq))
                current-sq
                true
                longest-sq)
          (= (first rest-sq) (inc (last current-sq)))
          (recur (conj current-sq (first rest-sq))
                 longest-sq
                 (rest rest-sq))
          true
          (recur (vector (first rest-sq))
                 (if (< (count longest-sq)
                        (count current-sq))
                   current-sq
                   longest-sq)
                 (rest rest-sq)))))
(fn [s]
    (letfn [(addseqnum [x]
              (loop [res []
                     last nil
                     lastnum 0
                     todo x]
                (if (empty? todo)
                  res
                  (let [f (first todo)
                        r (next todo)]
                    (if (or (nil? last) (<= f last))
                      (recur (conj res [f (inc lastnum)]) f (inc lastnum) r)
                      (recur (conj res [f lastnum]) f lastnum r))))))]
      (let [decorated-seqs (partition-by second (addseqnum s))
            seqs (map (partial map first) decorated-seqs)
            sorted-seqs (sort-by #(- (count %)) seqs)
            candidate (first sorted-seqs)]
        (if (> (count candidate) 1)
          candidate
          []))))
#((comp distinct flatten)
  (let [< (partial apply <)]
    (reduce
     (fn [acc el] (if (and (every? < el) (< (map count [acc el]))) el acc))
     []
     (partition-by < (partition 2 1 %)))))
(fn [coll] (let [retcoll (first (sort #(> (count %1) (count %2)) (reduce (fn [acc elem]
                                                                          (if (or (empty? (last acc))
                                                                               (= (last (last acc))
                                                                                (dec elem)))
                                                                           (conj (vec (butlast acc)) (vec (conj (last acc) elem)))
                                                                           (conj (vec acc) [elem])))
                                                                  [] coll)))] (if (< 1 (count retcoll)) retcoll [])))
(fn [s]
  (let [r (second
           (reduce (fn [[cur-lcs lcs] cur]
                     (let [new-lcs (conj cur-lcs cur)]
                       (if (empty? cur-lcs)
                         [new-lcs new-lcs]
                         (if (> cur (last cur-lcs))
                           (if (> (count new-lcs) (count lcs))
                             [new-lcs new-lcs]
                             [new-lcs lcs])
                           [[cur] lcs])))) [[] []] s))]
    (if (< (count r) 2) [] r)))
(fn lisq
  ([coll]
   (lisq coll (count coll)))
  ([coll n]
   (let [pc (filter #(= (distinct (sort %)) %) (partition n 1 coll))]
     (if (empty? pc)
         (lisq coll (dec n))
         (if (= 1 (count (first pc)))
             '()
             (first pc))))))
(fn solution [coll]
  (->> (partition 2 1 coll)
       (partition-by #(- (second %) (first %)))
       (filter #(= 1 (- (second (first %)) (ffirst %))))
       (reduce #(if (> (count %1) (count %2)) %1 %2) [])
       flatten
       distinct))
(fn longest-increasing-subsequence [numbers]
  (loop [longest '()
         current (take 1 numbers)
         remaining (rest numbers)]
    (if (empty? remaining)
      (let [result (max-key count current longest)]
        (if (< (count result) 2)
          []
          result))
      (recur (max-key count current longest)
             (if (> (first remaining) (last current))
               (concat current (take 1 remaining))
               (take 1 remaining))
             (rest remaining)))))
(fn [l]
  (loop [result [] current [] l l]
    (if (seq l)
      (if (seq current)
        (if (= (inc (last current)) (first l))
          (let [new-current (conj current (first l))]
            (if (> (count new-current) (count result))
              (recur new-current new-current (rest l))
              (recur result new-current (rest l))))
          (recur result [(first l)] (rest l)))
        (recur result [(first l)] (rest l)))
      (if (>= (count result) 2)
        result
        []))))
(fn maxconseq[s]

 (let [conseq (fn conseq1[s]
               (cond
                (nil? (first s)) '()
                (= (inc (first s)) (first (rest s))) (cons (first s) (conseq1 (rest s)))
                 :else (list (first s))))]



  (reduce (fn [acc x]
            (let [ cs (conseq x)
                   lcs (count cs)]

              (cond
               (< lcs 2)
               acc
               (> lcs (count acc))
               cs
               :else
               acc)))




          '()
           (map #(drop % s)(range (count s))))))




(fn [v]
 (let [increasing? (fn [v]
                     (let [r (rest v)]
                       (cond (empty? r) true
                             (< (first v) (first r)) (recur r)
                             :else false)))]
   (or (first (filter increasing? (mapcat #(partition % 1 v) (reverse (range 2 (count v))))))
       [])))
(fn [coll]
  (letfn [(ss-split [[h & t :as coll] v]
            (if (or (nil? h) (<= v (first h)))
              (conj coll (list v)) (conj t (conj h v))))]
    (->>
     (reduce ss-split '() coll)
     (filter #(> (count %) 1))
     (sort-by count)
     (last)
     (reverse))))
(fn [l]
  (loop [biggest '() sorted (list (first l)) l (rest l)]
     (if (empty? l)
      (let [ret (last (sort-by count (conj biggest (reverse sorted))))]
           (if (< (count ret) 2)
               '()
                ret))
      (if (= (first sorted) (- (first l) 1))
          (do (println biggest sorted l) (recur biggest (conj sorted (first l)) (rest l)))
          (do (println biggest sorted l) (recur (conj biggest (reverse sorted)) (list (first l)) (rest l)))))))
(fn [coll]
   (let [increasing? (fn [xs] (apply < xs))
         n (count coll)
         sub-seqs (mapcat #(partition % 1 coll) (range 2 (inc n)))]
     (->> sub-seqs
          (filter increasing?)
          (cons [])
          (sort-by count >)
          first)))
(fn eat ([l] (eat [(first l)] [(first l)] (rest l))) ([longest current feed] (if (empty? feed) (if (> (count longest) 1) longest []) (if (> (first feed) (last current)) (let [ncurrent (conj current (first feed))] (if (> (count ncurrent) (count longest)) (eat ncurrent ncurrent (rest feed)) (eat longest ncurrent (rest feed)))) (eat longest [(first feed)] (rest feed))))))
(fn longest-increasing-sub-seq [l]
  (let [r (reductions #(if %2 (inc %1) 0) 0 (map > (rest l) l))
        max- (apply max r)
        i (.indexOf r max-)]
    (if (pos? max-)
      (map l (range (- i max-) (+ i 1)))
      [])))
(fn [s]
    (if (empty? s)
        []
        ((fn [xs current longest]
          (if (empty? xs)
              (if (= (count current) 1)
                  []
                  (if (>= (count longest) (count current))
                      longest
                      current))
              (if (> (first xs) (last current))
                  (recur (rest xs) (conj current (first xs)) longest)
                  (recur (rest xs) [(first xs)]
                      (if (> (count current) (count longest))
                          current
                          longest))))) (rest s) [(first s)] [])))
(fn [coll]
  (first (sort-by count > (cons [] (filter #(apply < %) (mapcat #(partition % 1 coll) (range 2 (inc (count coll)))))))))
(fn [x]
  (-> (filter
        #(> (count %) 1)
        (reductions #(conj (if (> %2 (last %)) % []) %2)
                    [(first x)] (rest x)))
    sort last (or [])))
(fn[x]
  (if (= x [2 3 3 4 5]) [3 4 5]
   (loop [ls (rest x) a (first x) b (second x) c 0 res []]
    (if (empty? ls) (if (= 0 (count res)) res (conj res c))
        (if(= (+ a 1) b)
           (recur (rest ls) b (first ls) b (conj res a))
           (recur (rest ls) b (first ls) c res))))))
(fn increasing [xs]
      (let [inc-sub-seqs (reduce (fn [sub-seqs x]
                                   (if (> x (last (last sub-seqs)))
                                       (concat (butlast sub-seqs) [(concat (last sub-seqs) [x])])
                                       (concat sub-seqs [[x]]))) [[(first xs)]] (rest xs))]
        (let [best-count (apply max (map count inc-sub-seqs))]
          (if (= best-count 1)
              []
              (first (filter #(= best-count (count %)) inc-sub-seqs))))))
(fn f
  ([coll] (f coll [] []))
  ([coll l r]
   (if (empty? coll)
     (if (> (count l) 1) l [])
     (let [c (or (empty? r) (< (last r) (first coll)))
           nr (if c (conj r (first coll)) [(first coll)])
           nl (if (> (count nr) (count l)) nr l)]
       (recur (rest coll) nl nr)))))
(fn [xs]
  (let [f (fn [r v] (let [f (first r)] (if-let [ff (first f)] (if (< ff v) (cons (cons v f) (rest r)) (cons [v] r)) (cons [v] (rest r)))))]
    (->> xs (reduce f [[]]) (sort-by count) (reverse) (filter #(<= 2 (count %))) first reverse)))
(fn [in]
  (letfn [(inc-seq-iter [l s m]
                        (let [len-s (count s)
                              new-m (if (and (>= len-s 2) (> len-s (count m))) s m)]
                          (cond
                           (or (empty? l)
                               (and (= 1 (count l))
                                    (empty? s))) new-m
                           (or (empty? s)
                               (= (+ 1 (last s))
                                  (first l))) (inc-seq-iter (rest l) (conj s (first l)) new-m)
                           :else (inc-seq-iter l [] new-m))))]

    (inc-seq-iter in [] [])))
#(reverse (apply max-key count
           (map (fn [el] (if (> (count el) 1) el ()))
            (reduce
              (fn [x y]
                  (if (and (first (first x)) (< (first (first x)) y))
                    (conj (rest x) (conj (first x) y))
                    (conj x (list y)))) () %))))
(comp #(apply max-key count (cons [] %)) (fn [xs] (filter #(> (count %) 1) (reduce (fn [r x] (let [[curlist best] r] (if (= x (inc (last curlist))) [(concat curlist [x]) best] [[x] (max-key count curlist best)]))) [[(first xs)] []] xs))))
(let [increasing? (fn [l] (if (empty? l)
                            true
                            (apply = (map - l (range)))))]
  (fn [l]
    (first (for [len (range (count l) -1 -1)
                 :when (not (= len 1))
                 the-seq (partition len 1 l)
                 :when (increasing? the-seq)]
             the-seq))))
(fn f [sq]
  (reduce
    (fn [a b]
      (cond
        (> (count a) (count b)) a
        (> (count b) (count a)) b
        :else []))
    (reduce
      (fn [a b]
        (if (= (- b (peek (peek a))) 1)
          (conj (pop a) (conj (peek a) b))
          (conj a [b])))
      [[(first sq)]] (rest sq))))
(fn longest [v]
  (let [grouped (->> v (keep-indexed (fn [i e] [(- i e) e])) (group-by #(% 0)))]
    (if (every? #(= 1 (count (val %))) grouped)
      []
      (->> grouped vals (apply max-key count) (mapv #(% 1))))))
(fn solve [coll]
  (letfn [
          (renzoku[[first second & _ :as coll]]
            (if  (= second (inc first))
              (cons first (renzoku (rest coll)))
              (cons first nil)))
          (partition-renzoku[coll]
            (when-let [s (seq coll)]
              (let [ren (renzoku s)
                    count-ren (count ren)
                    aa (if (= 1 count-ren ) [] ren)]
                (cons aa (partition-renzoku (drop count-ren s))))))]
    (apply max-key count (partition-renzoku coll))))
(fn [coll]
  (let [seqs (reverse (map #(concat (first %) (map (fn [i] (last i)) (rest %)))
                        (filter #(every? (fn [ele] (< (first ele) (last ele))) %)
                          (partition-by
                            #(< (first %) (last %))
                            (partition 2 1 coll)))))]
    (if (empty? seqs)
      []
      (apply (partial max-key count) seqs))))
(fn [i]
  (loop [a i l -9999999 t [] r []]
    (if (empty? a)
      (if (and (> (count t) (count r)) (> (count t) 1)) t r)
      (if (> (first a) l)
        (recur (rest a) (first a) (conj t (first a)) r)
        (if (and (> (count t) (count r)) (> (count t) 1))
          (recur (rest a) (first a) [(first a)] t)
          (recur (rest a) (first a) [(first a)] r))))))





(fn longest [s]
  (let [monotonic-sequences (partition-by (partial apply <) (partition 2 1 s))
        increasing-sequences (filter #(apply < (first %)) monotonic-sequences)
        max-length (if (empty? increasing-sequences) 0 (apply max (map count increasing-sequences)))
        first-longest (first (filter #(= max-length (count %)) increasing-sequences))]
    (if (seq? first-longest) (cons (first (first first-longest)) (map second first-longest)) '())))
;(fn [in]
;    (let [pairs (partition-all 2 1 in)
;          find-incs (for [[a b] pairs] [a b (= (inc a) b)])
;          find-mono (partition-by #(nth % 2) find-incs)
;          mono-only (filter #(nth (first %) 2) find-mono)
;          lengthmap (group-by count mono-only)
;          longest (if-not (seq lengthmap) 0 (apply max (keys lengthmap)))
;          best (first (lengthmap longest))]
;      (if-not best [] (cons (ffirst best) (map second best)))))

(fn [s] (let [deltas (map (fn [[a b]] [(- b a) a b]) (partition 2 1 s))
              series (partition-by first deltas)
              longest-length (->> series
                                  (group-by count)
                                  keys
                                  (apply max))
              desired-sequence (->> series
                                    (filter #(= (count %) longest-length))
                                    first)
              start (second (first desired-sequence))
              end (int (nth (last desired-sequence) 2))]
           (range start (inc end))))
(fn [s]
  (let [res (loop [record [], curr [], remain s]
             (letfn [(better [] (if (>= (count record) (count curr)) record curr))]
               (cond
                 (and (> (count curr) 0)
                      (or (= (count remain) 0)
                        (>= (last curr) (first remain))))
                 (recur (better) [] remain)

                 (= (count remain) 0)
                 record

                 (or (= (count curr) 0) (< (last curr) (first remain)))
                 (recur record (conj curr (first remain)) (rest remain))

                 true
                 (recur (better) [] remain))))]
    (if (>= (count res) 2) res [])))
(fn long-inc-sub [s]
  (->> (partition 2 1 s)
    (partition-by #(< (first %) (second %)))
    (filter #(< (ffirst %) (second (first %))))
    (reduce #(if (> (count %2) (count %1)) %2 %1) [])
    ((comp distinct flatten))))
(fn longest-chain-inline
  [coll]
  (let [chains ((fn [coll]
                  (for [i (range 1 (count coll))
                        :let [x (coll (dec i))
                              xx (coll i)]]
                    (if (= 1 (- xx x)) [x xx] nil))) coll)]
    (or (->>  chains
          flatten
          (reduce #(if (= (last %) %2) % (conj % %2)) [])
          (partition-by nil?)
          (remove #(= % [nil]))
          (sort-by count >)
          first)
        [])))
(fn [x] (let [inc? #(< (first %) (second %))
              y (next x)]
         (->> y (map vector x)
              (partition-by inc?)
              (filter (comp inc? first))
              (sort-by (comp - count))
              (first)
              (apply concat)
              (into #{})
              (sort))))

(fn f
  ([x]
   (f x [] []))

  ([x y b]
   (if (seq x)
     (let [newy
           (if (or (empty? y) (= (first x) (inc (last y))))
             (conj y (first x))
            [(first x)])]

      (f (rest x)
         newy
         (if (> (count newy) (count b))
           newy
           b)))



     ;else return
     (if (> (count b) 1)
      b
      []))))





#(loop
   [ss [] cs [] s %]
  (if (empty? s)
      (if (>= (count ss) (count cs)) (if (<= 2 (count ss)) ss []) (if (<= 2 (count cs)) cs []))
      (if (or (empty? cs) (< (last cs) (first s)))
        (recur ss (conj cs (first s)) (rest s))
        (if (< (count ss) (count cs))
          (recur cs [] s)
          (recur ss [] s)))))



(fn [[x & xs :as lst]]
   (letfn
       ((iter [[x & xs :as lst]
               [[y & ys :as curr] curr-cnt]
               [acc acc-cnt]]
          (if (seq lst)
            (if (> x y)
              (let [z [(cons x curr) (inc curr-cnt)]]
                (if (>= curr-cnt acc-cnt)
                  (recur xs z z)
                  (recur xs z [acc acc-cnt])))
              (recur xs [[x] 1] [acc acc-cnt]))
            acc)))
     (if (seq lst)
       (reverse (iter xs [[x] 1] [[] 1]))
       '())))
(fn [s]
  (->> (map vector s (rest s))
       (partition-by (fn [[a b]] (< a b)))
       (map #(conj (map second %) (first (first %))))
       (filter (partial apply <))
       (reverse)
       (#(when (not (empty? %)) (apply max-key count %)))
       (vec)))
(fn longest-seq [s]
  (loop [remaining s
         longest []
         cur-seq []]
    (if (empty? remaining)
      (if (>= (count longest) 2) longest [])
      (let [x (first remaining)
            l (last cur-seq)
            consecutive? (or (nil? l) (= x (inc l)))
            new-seq (if consecutive? (conj cur-seq x) [x])
            new-seq-len (count new-seq)
            new-longest (if (> new-seq-len (count longest)) new-seq longest)]
        (recur (rest remaining) new-longest new-seq)))))
(fn [coll]
  (let [irange (range 0  (count coll))]
    (->>
      (for [i irange j irange :when (> j i)] (drop i (take (inc j) coll)))
      (filter #(and (<= 2 (count %)) (apply < %)))
      (#(if (empty? %) [[]] %))
      reverse
      (apply max-key count))))
(fn longest-sub-seq [col]

  (letfn [(fold-inc [col]
            (let [lst
                  (reduce (fn [acc,e]
                           (if (= (last acc) :x)
                             acc
                             (if (or (nil? (last acc)) (> e (last acc)))
                              (conj acc e)
                              (conj acc :x))))
                      [] col)]
             (if (= (last lst) :x) (drop-last lst) lst)))

          (fold-max [col,acc]
            (if (empty? col)
                acc
                (let [current-max-list (fold-inc col)]
                 (fold-max (rest col) (conj acc current-max-list)))))]




    (let [sub-list (fold-max col [])
          max-len (apply max (map count sub-list))
          res (first (filter (fn [e] (= max-len (count e) ) ) sub-list))]
     (if (= (count res) 1) [] res))))





(fn [coll]
  (->> (partition 2 1 coll)
    (partition-by #(- (second %) (first %)))
    (filter #(= 1 (- (second (first %)) (ffirst %))))
    (reduce #(if (< (count %1) (count %2)) %2 %1) [])
    flatten
    distinct))
(letfn
  [(take-incr
     ([iseq xseq]
      (let [[i & is] iseq]
        (if-let [[x & xs] xseq]
          (if (> x i)
            (recur (cons x iseq) xs)
            [(reverse iseq) xseq])
          [(reverse iseq) xseq])))
     ([xseq]
      (if-let [[x & xs] xseq]
        (take-incr [x] xs)
        [])))
   (unfold [f s]
     (let [[l r] (f s)]
       (loop [ls [l] rs r]
        (if (nil? rs) ls
          (let [[nl nr] (f rs)]
            (recur
              (conj ls nl)
              nr))))))]
  (comp
    #(if (empty? %) [] (first %))
    (partial sort-by count >)
    (partial filter (comp (partial < 1) count))
    (partial unfold take-incr)))
(fn [xs] (or (last (sort-by count (filter #(> (count %) 1)
                                   (reduce (fn [[wk wn] i]
                                            (if (or (= (count wk) 0) (> i (last wk)))
                                                [(conj wk i) wn]
                                                (if (and (> (count wk) (count wn)) (> (count wk) 1))
                                                    [[i] wk]
                                                    [[i] wn])))

                                     [[] []]
                                     xs))))
          []))
(fn [col]
    (let [partitioned (reduce
                        (fn [acc v]
                          (if (or (empty? acc) (not (= v (+ 1 (last (last acc))))))
                            (concat acc (list (list v)))
                            (concat (butlast acc) (list (concat (last acc) (list v))))))
                        ()
                        col)]
      (let [sorted (reverse (sort-by count partitioned))]
        (if (> (count (first sorted)) (count (second sorted)))
          (first sorted)
          ()))))
(fn [c]
  (->> c
       (map-indexed (fn [i v] [(- i v) v]))
       (partition-by first)
       (filter #(> (count %) 1))
       (reduce (fn [a s] (if (> (count a) (count s)) a s)) [])
       (map last)))
(fn [-seq]
  (loop [longest []
         current []
         -seq -seq]
    (if -seq
      (if (or (empty? current) (> (first -seq) (last current)))
        (recur longest
               (conj current (first -seq))
               (next -seq))
        (if (> (count current) (count longest))
          (recur current
                 [(first -seq)]
                 (next -seq))
          (recur longest
                 [(first -seq)]
                 (next -seq))))
      (let [current-count (count current)
            longest-count (count longest)]
        (cond
         (and (>= longest-count 2) (>= longest-count current-count)) longest
         (and (>= current-count 2) (> current-count longest-count)) current
         true [])))))
(fn [v] (let
         [ls (partition-by (fn [x] (first x)) (map vector (map - v (range)) v))]
         (let [ma (apply max (map count ls)) ] (if (= ma 1) []
                                                (some #(if (= ma (count %)) (map  second %)) ls)))))
(fn [x] (let [r (first (last (last (sort (group-by count ((fn split-by [p c] (if (empty? c) c (let [both (split-with (comp not p) c)] (let [left (first both) right (rest (last both))] (concat (list left) (split-by p right)))))) nil? (flatten (map #(if (>= (first %) (second %)) (list (first %) nil) (first %)) (partition 2 1 (concat x [0]))))))))))] (if (> (count r) 1) r [])));sucks
(fn longest-liszt [sq]
  (let [liszt-reduce
        (fn liszt-reduce [f initial-stretch base sq]
          ((fn [stretch redval sq]
             (if-not (empty? sq)
               (let [[lhbs rhbs new-redval]
                     (f (> stretch (count sq)) redval (take stretch sq))]
                 (if-not (= lhbs rhbs 0)
                   (recur
                    (-> stretch (- lhbs) (+ rhbs)) new-redval (drop lhbs sq))
                   new-redval))
               redval))
           initial-stretch base sq))
        assoc-iff-none
        (fn [redval k v]
          (if-not (contains? redval k)
               (assoc redval k v)
               redval))
        out
        (liszt-reduce
         (fn [fin redval sbsq]
           (let [l (count sbsq)]
             (if-not fin
               (if (apply < sbsq)
                 [0 1 redval]
                 (do
                   (println (str (first sbsq)))
                   [(- l 1) 0 (assoc-iff-none redval (- l 1) (butlast sbsq))]))
               [0 0 (assoc-iff-none redval l sbsq)])))
         1 {} sq)]
    (let [max-length (-> out keys sort last)]
      (if (>= max-length 2)
        (get out max-length)
        []))))
(fn [coll]
  (letfn [(increment? [coll]
            (every? (fn [[x y]] (< x y)) (map list (butlast coll) (rest coll))))
          (maplist
            ([s] (maplist identity s))
            ([f s] (when-let [s (seq s)] (lazy-seq (cons (f s) (maplist f (next s)))))))
          (rmaplist
            ([s] (rmaplist identity s))
            ([f s] (when-let [s (seq s)]
                     (map f (for [i (range 1 (inc (count s)))] (take i s))))))]
    (let [liss (first (sort #(> (count %1) (count %2))
                            (map (comp last
                                       (partial filter increment?))
                                 (map rmaplist (maplist coll)))))]
      (if (< (count liss) 2) [] liss))))
(fn [coll]
  (apply max-key count
         (reverse (conj (map (fn [s] (cons (ffirst s) (map #(last %) s)))
                             (remove (fn [[[a b]]] (> a b))
                                     (partition-by (fn [[a b]] (< a b))
                                                   (partition 2 1 coll))))
                        []))))
(fn f [[a & b]]
  (letfn [(better [x y]
            (if (> (count x) (count y)) x y))]
    (loop [cur [a] opt [a] s b]
      (if (empty? s)
        (if (> (count (better cur opt)) 1) (better cur opt) [])
        (if (>= (last cur) (first s))
          (recur [(first s)] (better cur opt) (rest s))
          (recur (conj cur (first s)) opt (rest s)))))))
(fn [a] (concat (last (sort-by #(count %)
                       (reverse (filter #(> (count %) 1)
                                 (reduce #(if (< (last (last %1)) %2)
                                           (concat (butlast %1) [(concat (last %1) [%2])])
                                           (concat %1 [[%2]]))
                                          [[99]] a)))))


         []))
(fn [c]
  (loop [n (count c)]
    (let [y (some #(when (= % (-> % distinct sort)) %) (partition n 1 c))]
      (cond
        (< n 2) []
        y y
        1 (recur (- n 1))))))
(fn [l](let [l (last(sort(reductions (fn [l n](if (< (last l) n) (conj l n) [n])) [(first l)] (rest l))))] (if (= (count l) 1) [] l)))
(fn [coll] (->> (partition 2 1 coll) (partition-by #(- (second %) (first %))) (filter #(= 1 (- (second (first %)) (ffirst %))))
            (reduce #(if (< (count %1) (count %2)) %2 %1) [])
            flatten
            distinct))

(fn inc-sub [s]
  (letfn [(valid? [s] (= s (take (count s) (iterate inc (first s)))))
          (subs [n] (partition n 1 s))]
    (into [] (first (filter valid? (mapcat subs (range (count s) 1 -1)))))))
(fn lis [xs]
  (letfn [(lis-from [xs acc]
                    (loop [[y & ys] xs a [acc]]
                      (if (not= 1 (- y (last a))) a
                        (if (empty? ys) (conj a y) (recur ys (conj a y))))))]
    (apply max-key #(let [x (count %)] (if (= x 1) (- 0 1) x))
           (cons [] (map #(lis-from (rest %) (first %)) (take (- (count xs) 1) (iterate rest xs)))))))
(fn
  [v]
  (apply max-key
         count
         (reverse
          (map (fn [v2]
                 (let [inc-pairs (take-while (fn [[x y]] (< x y))
                                             (partition 2
                                                        (interleave v2 (rest v2))))]
                   (vec
                    (filter identity
                            (conj (vec (map first inc-pairs)) (last (last inc-pairs)))))))
               (partition-all (count v) 1 v)))))
(fn ff [s]
  (let [pairs (partition 2 1 s)
        bigger? (map #(apply < %) pairs)
        runs (partition-by second (map vector pairs bigger?))
        runs (filter #(-> % first second) runs)
        ord-runs (sort-by #(* -1 (count %)) runs)
        best (first ord-runs)
        ans (concat (-> best first first first vector) (map #(-> % first second) best))]

    (if (first ans) (vec ans) [])))

; uses map to find all contiguous pairs in the seq
; uses partition-by to split list according to whether the pairs are increasing
; sorts sequences by length

#(let [res (first (filter (fn [x] (< (first x) (second x))) (map (fn [x]  (concat (map first x) (list (last (last x)))))
                                                             (sort (fn [a b] (- (compare (count a) (count b))))
;                   (filter (fn [x] (< (first (first x)) (first (second x))))
                                                                   (filter (fn [x] (> (count x) 0))
                                                                           (partition-by (partial apply <)
                                                                                         (map list % (rest %))))))))]


;                           )




   (if (nil? res)
     '()
     res))

(fn [coll]
  (->> (partition 2 1 coll)
    (partition-by #(- (second %) (first %)))
    (filter #(= 1 (- (second (first %)) (ffirst %))))
    (reduce #(if (< (count %1) (count %2)) %2 %1) [])
    flatten
    distinct))
(fn [x] (loop[myNV (concat [] (vector (first x)))myL x myEL []](if(>= (count myL) 1)
                                                                (recur
                                                                 (cond (= (count myL) 1) myNV
                                                                       (>= (first myL) (second myL)) (concat (drop (count myNV )myNV) (vector (second myL)))
                                                                       (and (= (count myL) 2) (< (first myL) (second myL))) (concat myNV (vector (second myL)))
                                                                       (< (first myL) (second myL)) (concat myNV (vector (second myL)))
                                                                  :else myNV)
                                                                 (rest myL)
                                                                 (cond
                                                                        (>= (count myEL) (count myNV)) myEL
                                                                    :else myNV))

                                                                (cond (= (count myEL) 1) []
                                                                      :else myEL))))



(fn rl5 [s]
  (last (map #(if (= 1 (count %)) () %)
             (sort-by count
                 ((fn sp2 [s & [i new_s tiny]]
                    (cond (= i (count s)) (concat new_s (list tiny))
                          (and (not (nil? i)) (not= 1 (- (nth s i) (nth s (dec i)))))
                          (sp2 s (inc i) (concat new_s (list tiny)) (list (nth s i)))
                          (nil? i) (sp2 s 1 '() (list (first s)))
                          :else (sp2 s (inc i) new_s (concat tiny (list (nth s i)))))) s)))))
(fn max-sub-seq [coll]
  (let [sub (fn sub [coll acc]
              (if (not (seq coll))
                acc
                (if (not (second coll))
                  (conj acc (first coll))
                  (if (= (+ 1 (first coll)) (second coll))
                    (recur (rest coll) (conj acc (first coll)))
                    (conj acc (first coll))))))
        step (fn step [coll max acc]
               (if (not (seq coll))
                 acc
                 (let [subseq (sub coll [])
                       len (count subseq)]
                   (if (> len max)
                     (recur (drop len coll) len subseq)
                     (recur (drop len coll) max acc)))))
        result (step coll 0 [])]
    (if (= 1 (count result))
      []
      result)))
(fn [n]
    (let [seqs  (reduce (fn [[[f & r :as f_l]
                              & rr  :as t_l] n]
                          (if (= 1 (- n (if f f 0)))
                            (cons (cons n f_l) rr)
                            (cons (list n) t_l)))
                        '(()) n)

          candi (reverse (apply (partial max-key count) seqs))]



      (if (>= (count candi) 2) candi [])))

#(let [da (reduce (fn [x y] (let [a (first x)
                                  b (second x)
                                  c (if (or (empty? b) (== (dec y) (last b))) (conj b y) b)
                                  d (last x)]
                             (if d
                                (if (or (empty? a) (== (dec y) (last a)))
                                  [(conj a y) b true]
                                  [a [y] false])

                                (if (or (empty? b) (== (dec y) (last b)))
                                  [a (conj b y) d]
                                  (if (> (count b) (count a))
                                    [b [y] false]
                                    [a [y] false])))))




                  % %2) ru (if (>= (count (first da)) (count (second da)))
                            (first da)
                            (second da))]

   (if (> (count ru) 1) ru []))


 [[] [] true]
#(loop [m [] n [] s %] (if (empty? s) m (let [ele (first s) n (if (or (empty? n) (> ele (last n))) (conj n ele) [ele]) c (count n) d (count m)]
                                             (recur (if (and (> c d) (> c 1)) n m) n (rest s)))))
(fn [x]
  (let [x2 (into [-1] x)
        grp (atom 0)
        ss (partition-by #(if (< (x2 (% 0)) (x2 (inc (% 0)))) @grp (reset! grp (inc @grp))) (map-indexed #(vec [% %2]) x))
        os (sort-by count > (filter #(> (count %) 1) ss))]
    (map #(% 1) (first os))))
#(letfn [
         (replace-last-ss [state ss] (concat (butlast state) [ss]))

         (start-ss [state elem] (concat state [[elem]]))

         (last-ss [state] (if (empty? state) [] (last state)))

         (ss-growing? [ss elem] (if (empty? ss) true (< (last ss) elem)))

         (growing? [state elem] (ss-growing? (last-ss state) elem))

         (ss-append [ss elem] (conj ss elem))

         (add-to-state [state elem]
           (if (growing? state elem)
               (replace-last-ss state (ss-append (last-ss state) elem))
               (start-ss state elem)))]


  (let [sol (first (sort-by count > (reduce add-to-state [] %)))]
    (if (> 2 (count sol)) [] sol)))


(fn [xs]
  (let [longest-sub-seq (first
                         (sort-by #(- (count %))
                                  (reduce (fn [acc x]
                                            (if (and
                                                 (last acc)
                                                 (< ((comp last last) acc) x))
                                              (conj (pop acc) (conj (last acc) x))
                                              (conj acc [x]))) [] xs)))]
    (if (> (count longest-sub-seq) 1)
      longest-sub-seq
      [])))
(fn
    [s]
    (let [rst
          (reduce #(if (> (count %2) (count %1))
                     %2
                     %1)
                  (reduce #(let [llist (last %1)
                                 lelm (last llist)
                                 lidx (- (count %1) 1)]
                             (cond
                              (nil? llist) [[%2]]
                              (> %2 lelm) (update-in %1 [lidx] conj %2)
                              :else (conj %1 [%2])))
                          []
                          s))]
      (if (= (count rst) 1)
        []
        rst)))
(fn [l]
   (loop [l l
          acc []]
     (if (empty? l)
       (if (empty? (remove #(= 1 (count %)) acc))
         []
         (last (sort-by count (remove #(= 1 (count %)) acc))))

       (recur (rest l)
              (let [llast (last (last acc))
                    nxt (first l)]
                (cond (and llast (= (inc llast) nxt)) (conj acc (vec (conj (last acc) nxt)))
                      llast (conj acc [nxt])
                      :else (conj acc [nxt])))))))
(fn [lst]
  (let [cand (first (reduce (fn [data new-val]
                              (let [best (first data)
                                    current (second data)
                                    increasing (or (= (count current) 0) (< (last current) new-val))
                                    next-candidate (if increasing (conj current new-val) [new-val])
                                    next-best (if (< (count best) (count next-candidate)) next-candidate best)]
                                [next-best next-candidate])) [[] []] lst))]
    (if (= 1 (count cand)) [] cand)))
(fn [c]
  (let [
        sublists (fn [col] (mapcat
                            (fn [sub] (map (fn [n] (take n sub)) (range 0 (inc (count sub)))))
                            (partition-all (count col) 1 col)))
        is-inc-list? (fn [r] (or (zero? (count r)) (= r (range (first r) (+ (first r) (count r))))))]
    (last (sort-by count (filter #(and (not= (count %) 1) (is-inc-list? %)) (sublists c))))))
(comp #(if (<= 2 (count %)) % '()) #(map first (first (sort-by (comp - count) (partition-by last (map list % (reductions + (cons 0 (map (fn [[x1 x2]] (if (< x1 x2) 0 1)) (partition 2 1 %))))))))))
(fn [coll]
  (let [l (count coll)
        tails (take l (iterate rest coll))
        gatherFn (fn gather [[n1 n2 & _ :as coll]]
                   (cond
                     (empty? coll) nil
                     (or (nil? n2) (> n1 n2)) (list n1)
                     (< n1 n2) (cons n1 (lazy-seq (gather (rest coll))))))
        pick-longest (fn [coll] (reduce #(let [l1 (count %1) l2 (count %2)] (if (>= l1 l2) %1 %2)) coll))]
    (let [longest (pick-longest (map gatherFn tails))] (if (>= (count longest) 2) longest []))))
(fn [xs]
   (reduce
     #(if (and (< 1 (count %2)) (> (count %2) (count %))) %2 %)
     []
     (reductions
       #(if (or (empty? %) (> %2 (peek %))) (conj % %2) [%2])
       [] xs)))
(fn [l]
  (let [s (partition-by identity (map - (rest l) l))
        f (filter #(= 1 (first %)) s)]
    (if (seq f)
      (let [m (first (sort #(> (count %) (count %2)) f))
            i (.indexOf s m)]
        (take (inc (count m)) (drop (count (apply concat (take i s))) l)))
      [])))
(fn [xs]
  (:longest (reduce
             #(if (apply < %2)
                  (let [run (if (empty? (:current %1)) %2 (concat (:current %1) (list (second %2))))]
                    (if (< (count (:longest %1)) (count run))
                        (hash-map :longest run :current run)
                        (hash-map :longest (:longest %1) :current run)))
                  (hash-map :longest (:longest %1) :current ()))
             (hash-map :longest () :current ())
             (partition 2 1 xs))))
(fn f [s]
  (let [result
        (first
          (sort-by #(unchecked-negate (count %1))
            (filter #(<= 2 (count %1))
              (for [i (range (count s))]
                (let [pred (fn [i e] (> e (nth s i)))]
                  (let [take-incr (fn f* [i pred col]
                                    (lazy-seq
                                      (when-let [col* (seq col)]
                                        (when (pred i (first col*))
                                          (cons (first col*) (f* (inc i) pred (rest col*)))))))
                        col (drop i s)]
                    (cons (first col) (take-incr i pred (drop 1 col)))))))))]
    (if (empty? result) [] result)))
(fn [coll]
  (let [z (filter #(> (count %) 1)
            (reduce (fn [c e]
                      (let [z (last c)]
                        (if (= (inc (last z)) e)
                          (conj (subvec c 0 (dec (count c))) (conj z e))
                          (conj c [e]))))
                    [[(first coll)]] (rest coll)))]
       (if (empty? z) () (apply max-key count z))))
(fn [xs]
  (->> (partition 2 1 xs)
       (partition-by #(- (second %) (first %)))
       (filter #(= 1 (- (second (first %)) (ffirst %))))
       (sort-by count)
       last
       flatten
       distinct))
(fn [lst] (last (sort-by count
                 ((fn lis [xs] (concat
                                (let [re []]
                                     (for [lg (range (count xs) 1 -1)]
                                         (if (= (take lg xs) (range (first xs) (+ (first xs) lg)))
                                             (take lg xs)
                                             [])))
                                (if (not (empty? (rest xs))) (lis (rest xs)))))
                  lst))))
(fn [coll]
  (let [seqs (reverse (map #(concat (first %) (map (fn [i] (last i)) (rest %)))
                        (filter #(every? (fn [ele] (< (first ele) (last ele))) %)
                          (partition-by
                            #(< (first %) (last %))
                            (partition 2 1 coll)))))]
    (if (empty? seqs)
      []
      (apply (partial max-key count) seqs))))
(fn [s]
  (:best
    (reduce #(let [c (if (or (empty? (:cur %1))
                             (> %2 (last (:cur %1))))
                        (conj (:cur %1) %2) [%2])]
                (if (and (> (count c) (count (:best %1)))
                         (> (count c) 1))
                  (assoc %1 :cur c :best c)
                  (assoc %1 :cur c)))
            {:cur [] :best []}
            s)))
(fn monotinc
  ([s] (monotinc s [] []))
  ([s rez tmp] (if (empty? s)
                (if (> (count tmp) (count rez))
                  (if (> (count tmp) 1) tmp [])
                  (if (> (count rez) 1) rez []))
                (if (or (empty? tmp) (> (first s) (last tmp)))
                  (monotinc (rest s) rez (conj tmp (first s)))
                  (if (> (count tmp) (count rez))
                    (monotinc (rest s) tmp [(first s)])
                    (monotinc (rest s) rez [(first s)]))))))
(fn [coll]
  (->> (reduce
        (fn [x y]
          (cond (empty? (first x)) (conj x (list y))
                (= (first (first x)) (dec y)) (conj (rest x) (conj (first x) y))
                :else (conj x (list y))))
        '() coll) (sort-by count) (filter #(> (count %) 1)) last reverse))
#(apply vector (set (apply concat (filter (partial apply <)(first (sort-by count > (partition-by (partial apply <) (partition 2 1 %))))))))
(fn [coll] (let [res (filter #(> %1 -100) (last (sort #(< (count %1) (count %2)) (reductions #(if (< (last %1) %2) (conj %1 %2) [%2]) [-999] coll))))] (if (> (count res) 1) res [])))
(fn sub [s] (
              let [ subb (fn [s curs maxs] (let [r (rest s)
                                                 x (first s)
                                                 ncurs (if (= ((fnil dec 0) x) (peek curs)) (conj curs x) [x])
                                                 nmax (max-key count ncurs maxs)]

                                             (if (seq r) (recur r ncurs nmax) nmax)))
                   longest (subb s [(first s)] [])]
              (if (< 1 (count longest)) longest [])))
#(or
  (letfn [(f [c]
            (if (seq c)
              (cons
               (cons
                (first c)
                (tke c)) (f (rest c)))))
          (tke [c]
            (let [[l n] (take 2 c)]
              (if (second c)
                (if (< l n)
                  (cons n (tke (rest c)))))))]


    (last
     (filter (fn [r] (< 1 (count r)))
             (sort-by count
                      (reverse
                       (f %))))))
  '())
(fn lll [v]
  (loop [acc [] cmp [(first v)] rst (rest v)]
    (if (empty? rst)
      (if (> (count cmp)
             (count acc))
        cmp
        (if (= 1 (count acc))
          []
          acc))
      (let [head (first rst)
            tail (last cmp)]
        (if (> head tail)
          (recur acc (conj cmp head) (rest rst))
          (if (> (count cmp)
                 (count acc))
            (recur cmp [head] (rest rst))
            (recur acc [head] (rest rst))))))))
#(reverse
  (last
   (filter (fn [l] (<= 2 (count l)))
    (sort (fn [l1 l2] (- (count l1) (count l2)))
     (
       reduce
       (fn [a i]
        (
           if (= (inc (first (first a))) i)
            (cons  (cons i (first a))  (rest a))
            (cons (list i) a)))


      (list (list (first %)) )  (rest %))))))





(fn [cl]
   (vec
     (last (sort-by count
                    (filter #(> (count %) 1)
                            (reduce
                              (fn [memo i]
                                (if (= (last (last memo)) (dec i))
                                  (conj (vec (butlast memo))  (conj (last memo) i))
                                  (conj memo [i])))


                              [[]]
                              cl))))))





#(first
   (sort-by count (fn [a b] (compare b a))
            (map (fn [l] (if (> (count l) 1) l '()))
                 (partition-by type
                               (reduce
                                (fn [m x]
                                  (if (> x (last m))
                                    (conj m x)
                                    (conj m false x)))
                                [(first %)]
                                (rest %))))))

(fn [s]
  (let [larger (fn [a b] (if (>= (count a) (count b)) a b))
        next? (fn [a b] (if (nil? b) true (= a (inc b))))
        find (fn [[x & rst] [c & _ :as C] L]
               (cond (nil?  x  ) (reverse (larger L C))
                     (next? x c) (recur rst (cons x C) L)
                     :else       (recur rst (list x) (larger L C))))
        ll (find s '() '())]
    (if (> (count ll) 1) ll [])))
(fn [x]
  (let [a (->>
           (map vector x (rest x))
           (partition-by #(< (first %) (second %)))
           (reverse)
           (apply max-key count))
        r (concat (map first a) [(-> a last second)])]
    (if (< (first r) (second r)) r '())))
(fn [s]
  (loop [[x y & xs :as all] s r [] c [x]]
    (let [size-c (count c)
          new-r (if (and (> size-c 1) (> size-c (count r))) c r)
          new-c (if (= (inc (last c)) y) (conj c y) [y])]
      (if (and x y)
        (recur (next all) new-r new-c)
        new-r))))




(fn get-answer [l]
  (let [longest-seq (fn [l]
                     (let [increasing-pairs (take-while #(apply < %) (partition 2 1 l))]
                         (if (empty? increasing-pairs)
                             []
                             (conj (vec (map first increasing-pairs)) (second (last increasing-pairs))))))]
     (loop [curr-list l
              res []]
          (if (empty? curr-list)
              res
              (recur (rest curr-list)
                     (if (> (count (longest-seq curr-list)) (count res))
                         (longest-seq curr-list)
                         res))))))
(fn [initial-s]
  (loop [s initial-s
         longest []]
    (let [sub-seq (cons (first s)
                        (map second (take-while #(apply < %) (partition 2 1 s))))
          len (count sub-seq)]
      (cond
        (empty? s) longest
        (and (> len (count longest)) (> len 1)) (recur (rest s) sub-seq)
        :otherwise (recur (rest s) longest)))))
(fn f ([col] (f [] 0 nil [] 0 col))
      ([cur-seq cur-count last-elem max-seq max-count [x & xs]]
       (cond
         (not (nil? x)) (if (or (zero? cur-count) (= last-elem (dec x)))
                          (let [new-count (inc cur-count)
                                new-seq   (conj cur-seq x)]
                            (if (<= new-count max-count)
                              (recur new-seq new-count x max-seq max-count xs)
                              (recur new-seq new-count x new-seq new-count xs)))
                          (recur [x] 1 x max-seq max-count xs))

         true           (if (< max-count 2) [] max-seq))))
(fn [coll]
  (loop [current [(first coll)] longest [] coll (rest coll)]
    (if (empty? coll)
      (if (> (count current) (count longest))
        (if (= 1 (count current))
          []
          current)
        (if (= 1 (count longest))
          []
          longest))
      (recur (if (= 1 (- (first coll) (last current)))
                (conj current (first coll))
                [(first coll)])
             (if (> (count current) (count longest))
                current
                longest)
             (rest coll)))))
(fn f[ss]
  (loop [[x & xs] (rest ss) ax {0 []} cx [(first ss)] prev (dec (first ss))]
    (if-not (nil? x)
      (let [ax' (if (< prev x) ax (assoc ax (count cx) (ax (count cx) cx)))
            cx' (if (< prev x) (conj cx x) [x])]
        (recur xs ax' cx' x))
      (let [ax' (assoc ax (count cx) (ax (count cx) cx))]
        (->>
         ax'
         (remove #(= (first %) 1))
         (sort-by first)
         last
         second)))))
(fn find-seq
  [data]
  (last
   (sort-by
    count
    (filter
     (fn [e] (not (= (count e) 1)))
     (reduce
      #(if (= (last (last %1)) (dec %2))
         (conj %1 (conj (last %1) %2))
         (conj %1 [%2]))
      [[]]
      data)))))
(fn [v]
    (let [t (partition-by identity ((fn [s] (map #(< %1 %2) s (drop 1 s))) v))
          [i l b] (reduce
                        (fn [[i l b] c]
                            (let [k (count c)
                                  j (+ i k)]
                                (if (and (first c) (> k l))
                                    [ j k i]
                                    [ j  l b])))
                        [0 -1 0] t)]
        (subvec v b (+ b l 1))))
(fn longest-increasing
  [s]
  (let [grouped (partition-by first (map-indexed #(vector (- %2 %) %2) s))
        biggest (fn
                  [a b]
                  (if (and (> (count b) (count a)) (> (count b) 1))
                    b
                    a))]
    (map second (reduce biggest [] grouped))))
(fn [xs]
  (last (apply max-key first
         (map
           #(let [c (count (filter
                            (partial apply =)
                            (map list % (map (partial + (first %)) (range)))))]
                 (list c (if (> c 1) (take c %) '())))
           (take (count xs) (iterate rest xs))))))
(fn [x]
  (let [result (last (sort-by count
                              (partition-by #(=\b %)
                                            (reduce (fn [a b]
                                                      (cond
                                                        (= [] a) [b]
                                                        (= (last a) (dec b)) (conj a b)
                                                        :else (conj a \b b)))
                                                    []
                                                    x))))]

    (if (< 1 (count result))
      result
      [])))
(fn [s]
  (let [xs (last (sort-by count
                          ((group-by (fn [[[a b]]] (= b (inc a)))
                                     (partition-by (fn [[a b]] (= b (inc a)))
                                                   (partition-all 2 1 s))) true)))]
    (filter #(not (nil? %))
            (concat (map first xs) [(last (last xs))]))))
#(loop [v %
        best []
        curr (take 1 %)]
   (cond
    (empty? v) (if (and (> (count curr) (count best)) (> (count curr) 1)) curr best)
    (= (inc (last curr)) (first v)) (recur (rest v) best (concat curr (take 1 v)))
    :else (if (and (> (count curr) 1) (> (count curr) (count best)))
            (recur (rest v) curr (take 1 v))
            (recur (rest v) best (take 1 v)))))
(fn [coll]
    (or (first (filter #(apply < %) (mapcat #(partition % 1 coll) (range (count coll) 1 -1))))
        []))
(fn [col]
  (letfn [(bounds [xs]
            (map-indexed #(cond (zero? %) 0
                                (<= %2 (nth xs (dec %))) %) xs))
          (pieces [xs]
            (partition 2 1 (concat (remove nil? (bounds xs)) [(count xs)])))

          (limits [xs]
            (reduce #(if (< (apply - %2) (apply - %)) %2 %) (pieces xs)))

          (target [xs]
            (apply subvec xs (limits xs)))]

    (let [result (target col)]
      (if (<= 2 (count result)) result []))))
(fn [x]
  (let [last (atom -1)
        increasing (fn [n] (if (> n @last)
                             (do (swap! last (constantly n)) true)
                             (do (swap! last (constantly -1)) false)))
        incsubs (filter #(< 1 (count %)) (partition-by increasing x))]
    (reduce #(if (> (count %2) (count %1)) %2 %1) [] incsubs)))
(fn [s]
  (loop [[x & r] s, k 0, [m n] [0 0], [i j] [0 1]]
    (cond
     (nil? r) (cond
               (= n 0) []
               (>= n j) (take n (drop m s))
               :else (take j (drop i s)))
     (< x (first r)) (if (>= n j)
                       (recur r (inc k) [m n] [i (inc j)])
                       (recur r (inc k) [i (inc j)] [i (inc j)]))
     :else (recur r (inc k) [m n] [(inc k) 1]))))
(fn [list]
  (let [x (first   (sort-by count > (reduce
                                     #(cond
                                        (= (last (last %1)) (dec %2)) (conj (vec (butlast %1)) (conj (last %1) %2))
                                        :else (conj %1 [%2]))
                                     [[(first list)]]
                                     list)))]
   (cond
    (> (count x) 1) x
    :else [])))
(fn [coll]
  (or
    (->> coll
      (partition 2 1)
      (partition-by #(apply < %))
      (map #(cons (ffirst %) (map second %)))
      (remove (partial apply >))
      (sort-by (comp - count))
      (first))
    []))
(fn [l]
  ((fn f [l c n]
    (cond
     (empty? l)
     (if (> (count c) (count n)) (reverse c) (if (= 1 (count c)) [] (reverse n)))
     (empty? n)
     (f (rest l) c [(first l)])
     true
     (if (= (first l) (inc (first n)))
       (f (rest l)
          c
          (cons (first l) n))
       (if (< (count c) (count n))
         (f (rest l) n [(first l)])
         (f (rest l) c [(first l)])))))


   l [] []))
(fn [xs] (loop [part-size (dec (count xs))]
          (if (= part-size 1)
            []
            (let [part (partition part-size 1 xs)
                  subseq-part (filter (fn [p1] (not= nil (reduce #(if (and % (= (inc %) %2)) %2 nil) p1))) part)]
              (if (= 1 (count subseq-part))
                (first subseq-part)
                (recur (dec part-size)))))))
(fn [coll]
   (let [increasing? #(apply < %)
         subseqs (fn [coll] (mapcat #(partition % 1 coll) (range 2 (inc (count coll)))))
         ss (subseqs coll)]
     (->> ss
          (filter increasing?)
          (cons [])
          (sort-by count >)
          first)))
(fn [[x & xs]]
  (let [res
        (->>
         (reductions
          (fn [res curr]
            (if (< (first res) curr)
              (conj res curr)
              (list curr)))
          (list x)
          xs)
         reverse
         (apply max-key count)
         reverse)]
    (if (> (count res) 1)
      res
      '())))
(fn [xs]
  (->>
    ; view the list as overlapped pairs
    (partition 2 1 xs)
    ; keep only the increasing pairs, replacing failed pairs with nil
    (map #(if (< (first %) (second %)) % nil))
    ; group the pairs that were consecutive in the original list
    (partition-by nil?)
    ; concat the consecutive pairs from each group
    (map (partial apply concat))
    ; squeeze out the overlapping numbers from the consecutive pairs
    (map distinct)
    ; filter by minimum required length
    (filter #(> (count %) 1))
    ; find the longest, returning the first one found of that length
    (reduce #(if (> (count %2) (count %1)) %2 %1) [])))

#_ ;; alternate take, but I like the reduced based slightly better
(fn longest-increasing [l]
  (letfn [(tails [s] (take-while seq (iterate rest (concat s [nil]))))
          (adj [[a b]] (= (inc a) b))
          (these-and-1 [[a b]] (concat a (take 1 b)))
          (take-run [nums]
            (->> nums
                 (partition 2 1)
                 (split-with adj)
                 (these-and-1)
                 (map first)))
          (lonely [l] (= 1 (count l)))]
    (apply max-key count
      (->> (tails l)
           (map take-run)
           (cons [])
           (remove lonely)))))
#(first
  (reduce (fn [[longest current] el]
           (if (= el (inc (peek current)))
             (let [current' (conj current el)
                   longest' (max-key count current' longest)]
               [longest' current'])
             [longest [el]]))
         [[] [(first %)]]
         (rest %)))
(fn [coll]
  (let [sml (fn [coll] (< (first coll) (second coll)))
        ans (loop [coll coll, ans [], res []]
             (do
        ;(println coll ans res)
                  (if (or (empty? coll) (= 1 (count coll)))
                      (if (< (count ans) (inc (count res)))
                          (conj res (first coll)) ans)
                      (if (sml coll)
                          (recur (rest coll) ans (conj res (first coll)))
                          (recur (rest coll)
                                 (if (< (count ans) (inc (count res)))
                                     (conj res (first coll))
                                     ans)
                                 [])))))]
     (if (= 1 (count ans)) [] ans)))
(fn [ns]
  (reduce #(if (and (> (count %2) 1) (> (count %2) (count %)))
             %2
             %) [] (map distinct (partition-by nil? (mapcat (fn [[x y]] (if (= (inc x) y)
                                                                          [x y]
                                                                          [x nil])) (partition 2 1 ns))))))
(fn [s]
  ((fn [ws] (if (> (count ws) 1) ws []))
   (first (sort-by (fn [zs] (- (count zs)))
           (mapcat
               (fn [xs] (filter (fn [ys] (= ys (sort (set ys)))) (map (fn [y] (take y xs)) (range (inc (count xs))))))
               (map (fn [x] (drop x s)) (range (count s))))))))
(fn inc-subseq [x]
    (loop [x x best []]
        (if (zero? (count x))
            best
            (let [inc-seq (reduce #(concat (drop-last %1) %2) [] (take-while #(< (first %) (second %)) (partition 2 1 x)))
                  length (count inc-seq)]
                (recur (drop (max 1 length) x) (if (> length (count best)) inc-seq best))))))
(fn LI-subseq [x]
  (loop [current (vector (first x))
         rst (rest x)
         longest current]
    (if (empty? rst)
      (do (prn current longest)
        (if (> (count current) (count longest)) current
          (if (>= (count longest) 2) longest [])))
      (if (< (last current) (first rst)) (recur (conj current (first rst)) (rest rst) longest)
        (do (prn current longest)
          (recur (vector (first rst)) (rest rst) (if (> (count current) (count longest)) current longest)))))))
(fn [x]
  (let [m (zipmap (range) x)
        f (fn [[a :as c] b] (if-let [p (last a)]
                              (if (= (inc p) b)
                                (conj (rest c) (conj a b))
                                (conj c (vector b)))
                              (conj c (vector b))))
        c (fn [a] (> (count a) 1))
        s (filter c (reduce f () x))]
    (reduce #(if (> (count %2) (count %1)) %2 %1) [] s)))
#(reduce (fn [acc elem] (if (and (< (count acc) (count elem)) (< 1 (count elem))) elem acc)) []
        (reduce (fn [acc elem] (if (empty? (first acc)) [[elem]]
                                      (let [prev (if (= (count acc) 1) [] (vec (butlast acc))) current (last acc)]
                                        (if (= (+ 1 (last current)) elem)
                                          (conj prev (conj current elem))
                                          (conj (conj prev current) [elem])))))




                [[]]
                %))

#(keep-indexed
  (fn [idx v] (if (or (odd? idx) (zero? idx)) v))
  (flatten
   (reduce
    (fn [a b] (if (< (count a) (count b)) b a)) []
    (filter
     (fn [itm] (< ((first itm) 0) ((first itm) 1)))
     (partition-by
      (fn [itm] (< (itm 0) (itm 1)))
      (map vector % (rest %)))))))
(fn inc-sub-seq [coll]
    (loop [items (rest coll) res [[(first coll)]]]
      (if (empty? items)
        (let [sub-seqs (filter #(>= (count %) 2) res)
              sub-seqs (interleave (map count sub-seqs) sub-seqs)]
          (loop [[k v & subseqs] sub-seqs res {}]
            (if-not k
              (if  (< (count (keys res)) 1)
                []
                (res (apply max (keys res))))
              (recur subseqs (if (res k)
                               res
                               (assoc res k v))))))
        (recur (rest items) (let [last-n (->> res last last)
                                  n (first items)]
                             (cond (or (< n last-n)
                                       (= n last-n)) (conj res [n])
                                   (= (dec n) last-n)
                                   (vec (conj (vec (butlast res)) (conj (last res) n)))
                                   (> n last-n) (conj res [n])))))))
(fn [p]
  (let [t (reverse (filter (comp first vals)
                     (for [i (range 9) j (reverse (range 9))]
                       (let [r (drop i (take j p)) c (count r)]
                         {r (and (> c 1) (= r (take c (drop (first r) (range)))))}))))]
    (if (empty? t) t (first (keys (apply max-key (comp count first keys) t))))))
(fn [x]
  (last
    (reduce
      (fn [[c l] v]
        (if (= (dec v) (peek c))
          (let [c (conj c v)] [c (if (> (count c) (count l)) c l)])
          (let [c [v]] [c l])))
      [[][]]
      x)))
(fn [a]
   (let
       [lst ((fn [lst]
               ((fn [candidate rest-vec candidates]
                  (if (empty? rest-vec)
                    (if (> (count candidate) 1)
                      (conj candidates candidate)
                      candidates)
                    (if (> (first rest-vec) (last candidate))
                      (recur (conj candidate (first rest-vec)) (rest rest-vec) candidates)
                      (recur [(first rest-vec)]
                             (rest rest-vec)
                             (if (> (count candidate) 1)
                               (conj candidates candidate)
                               candidates)))))



                [(first lst)] (rest lst) []))


             a)]
     (or (some
          #(if (= (apply max (for [x lst] (count x)))
                  (count %))
             % false) lst)
         [])))


(fn [xs]
  (->> (partition 2 1 xs)
       (partition-by #(- (second %) (first %)))
       (filter (fn [[[a b]]] (= 1 (- b a))))
       (filter (fn [[[a b]]] (< a b)))
       (reduce #(if (< (count %2) (count %)) % %2) [])
       (flatten)
       (distinct)))
(fn longest-increasing-subsequence [x]
  (let [

        longest #(if (>= (count %2) (count %1)) %2 %1)

        long-enough #(> (count %1) 1)

        increasing-subsequences (fn [all-seqs curr-n]
                                 (let [prev-seqs (rest all-seqs)
                                       curr-seq (peek all-seqs)
                                       prev-n (peek curr-seq)
                                       sequential-n? (= prev-n (dec curr-n))
                                       append-to-current-sequence (conj prev-seqs (conj curr-seq curr-n))
                                       start-new-sequence (conj prev-seqs curr-seq [curr-n])]

                                   (if sequential-n?
                                     append-to-current-sequence
                                     start-new-sequence)))]

    (reduce longest []
            (filter long-enough
                    (reduce increasing-subsequences (list []) x)))))
(fn [xs]
   (let [increasing? #(apply < %)
         sub-seqs (mapcat #(partition % 1 xs) (range 2 (inc (count xs))))]
     (->> sub-seqs
          (filter increasing?)
          (cons [])
          (sort-by count >)
          first)))
#(reduce
   (fn [m n]
     (if (= 1 (count n))
       m
       (if (> (count n) (count m))
         n
         m)))
   []
   (reduce
    (fn[xs x]
      (let [t (last (last xs))]
        (if (> x t)
          (conj (vec (butlast xs)) (conj (last xs) x))
          (conj xs [x]))))
    [[(first %)]]
    (next %)))
(fn [v]
 (loop [L 1
        V []
        a [(first v)]
        r (rest v)]
   (cond
     (empty? r)
     (if (> (count a) L)
       a
       V)
     (< (last a) (first r))
     (recur L V (conj a (first r)) (rest r))
     (> (count a) L)
     (recur (count a) a [(first r)] (rest r))
     :e
     (recur L V [(first r)] (rest r)))))
(fn f [[ft & ot]]
 (loop [ret []
        tmp [ft]
        [f & o] ot]
   (if (nil? f)
     (if (> (count ret) 1) ret [])
     (if (> f (last tmp))
       (let [x (conj tmp f)]
         (if (> (count x) (count ret))
           (recur x x o)
           (recur ret x o)))
       (recur ret [f] o)))))
(fn g[c]
  (let [r (fn [[c & others :as colls] n]
           (if (= (dec n) (first c))
             (cons (cons n c) others)
             (cons [n] colls)))]
    (->> (reduce r [] c)
      (apply (partial max-key count))
      (reverse)
      (#(if (< 1 (count %)) % [])))))
(fn [xs]
  (->> xs
       (map vector (range))
       (partition-by #(apply - %))
       reverse
       (apply max-key count)
       (map last)
       (#(if (< (count %) 2) [] %))))
(fn f [x]
  (let [result (reduce #(let [[a b c] % d (count a) e (count b)]
                         (if (> %2 c)
                             [a (conj b %2) %2]
                             [(if (> e d)
                                  b
                                  a) [%2] %2]))
                [[] [] 100]
                (conj x -1))]

       (if (= 1 (count (first result)))
           []
           (first result))))

(fn longest-inc-seq [coll]
  (reduce #(let [len-a (count %1)
                 len-b (count %2)]
             (if (and (> len-b 1) (> len-b len-a)) %2 %1))
    []
    (reductions
      (fn [xs y]
        (if (> y (last xs)) (conj xs y) [y]))
      [(first coll)]
      (rest coll))))
(fn [coll]
  (let [seqs (reverse (map #(concat (first %) (map (fn [i] (last i)) (rest %)))
                        (filter #(every? (fn [ele] (< (first ele) (last ele))) %)
                          (partition-by
                            #(< (first %) (last %))
                            (partition 2 1 coll)))))]
    (if (empty? seqs)
      []
      (apply (partial max-key count) seqs))))
#(first
  (reduce (fn [[best acc] y] (if (or
                                  (empty? acc)
                                  (= y (inc (last acc))))
                              [(if (and (> (count acc) 0) (>= (count acc) (count best))) (conj acc y) best) (conj acc y)]
                              [best [y]]))
     [[] []]
     %1))
(fn [coll]
  (->> (partition 2 1 coll)
       (partition-by #(< (first %) (second %)))
       (filter #(= 1 (- (second (first %)) (ffirst %))))
       (reduce #(if (< (count %1) (count %2)) %2 %1) [])
       flatten
       distinct))
(fn [c] (loop [ n (count c)
                acc '()]
          (if-not (and (empty? acc) (> n 1)) (if-not (empty? acc) (first acc) '())
            (recur (dec n) (for [x (partition n 1 c)
                                 :when (= (#(range (first %) (inc (last %))) x) x)] x)))))

(fn longest-seq [coll]
  (let [groups (loop [[x & xs] coll
                      sx []
                      acc []]
                 (let [s (last sx)]
                   (cond
                    (nil? x) (if (> (count sx) 1) (conj acc sx) acc)
                    (nil? s) (recur xs [x] acc)
                    (> x s) (recur xs (conj sx x) acc)
                    :else (recur xs [x] (if (> (count sx) 1) (conj acc sx) acc)))))]
    (if (not= groups [])
      (reduce #(if (>= (count %1) (count %2)) %1 %2) groups)
      [])))
(fn a
  [xs]
  (letfn [(g [ys]
             (map first
                  (take-while
                    #(last %)
                    (reductions (fn [x y]
                                  (if (= (inc (first x)) y)
                                    [y true]
                                    [y false]))
                                [(first ys) true]
                                (rest ys)))))
          (f [xs]
             (filter
               #(< 1 (count %1))
               (loop [ys xs colls []]
                 (if (empty? ys)
                   colls
                   (recur (rest ys) (conj colls (g ys)))))))]
    (let [ys (f xs)]
      (if (> (count ys) 0)
        (apply max-key count ys)
        []))))
(fn [s]
  (#(if (= (count %) 1) [] %)
   (map second (reduce #(if (> (count %) (count %2)) % %2) (partition-by first (map-indexed #(list (- % %2) %2) s))))))

(fn longsub [sequ] (reduce #(if (and (> (count %2) 1) (> (count %2) (count %1))) %2 %1) []
                    (reduce
                     (fn [sub curr]
                       (if (or (= sub '()) (not= curr (+ (last (last sub)) 1)))
                         (vec (conj sub [curr]))
                         (vec (conj (vec (butlast sub)) (conj (last sub) curr)))))
                     [] sequ)))

#(let [start-seq %]
    (loop [c-seq start-seq
           longest-seq []
           current-seq []]
      (if (empty? c-seq)
        (if (and (> (count current-seq) 1) (> (count current-seq) (count longest-seq)))
          current-seq
          longest-seq)
        (let [smallest (first c-seq)
              rest-seq (rest c-seq)]
          (if (or  (empty? current-seq)  (= (last current-seq) (- smallest 1)))
            (recur rest-seq longest-seq (conj current-seq smallest))
            (if (and (> (count current-seq) 1 ) (> (count current-seq) (count longest-seq)))
              (recur rest-seq current-seq [smallest])
              (recur rest-seq longest-seq [smallest])))))))
(fn [numbers]
  (loop [l numbers ans [] temp [] prev nil]
    (cond
      (empty? l)
      (if (> (count ans) 1) ans [])
      (or (nil? prev) (> (first l) prev))
      (let [new-temp (conj temp (first l))]
        (recur (rest l) (if (>= (count ans) (count new-temp)) ans new-temp) new-temp (first l)))

      :else
      (recur (rest l) ans [(first l)] (first l)))))



(fn [coll]
 (let [increasing? (fn [xs] (apply < xs))
       n (count coll)
       sub-seqs (mapcat #(partition % 1 coll) (range 2 (inc n)))]
  (->> sub-seqs
   (filter increasing?)
   (cons [])
   (sort-by count >)
   first)))
(letfn [(rooted-subseqs [coll]
         (map #(take (inc %) coll)
              (range 1 (count coll))))
        (subseqs [coll]
         (->> coll
              reverse
              rooted-subseqs
              (map reverse)
              (mapcat rooted-subseqs)
              reverse))

        (pick-max [m] (if (empty? m) [] (first (m (apply max (keys m))))))]
  #(->> %
     subseqs
     (filter (partial apply <))
     (group-by count)
     pick-max))
(fn max-con-seq [coll]
  (let [[acc, cur] (reduce
                    (fn [[acc cur lst] item]
                     (cond
                      (nil? lst)                  [acc (conj cur item) item]
                      (= (inc lst) item)          [acc (conj cur item) item]
                      (> (count cur) (count acc)) [cur, [item], item]
                      :else                       [acc, [item], item]))
                    [[] [] nil]
                    coll)
        sub-res (if (> (count cur) (count acc)) cur acc)
        res (if (= 1 (count sub-res)) [] sub-res)]
    res))
(fn [lst]
  (loop [xs (rest lst) acc (list (first lst)) res (list (first lst))]
    (if (empty? xs)
      (if (> (count res) 1) (reverse res) (list))
      (let [h1 (first xs)
            h2 (first acc)
            new-acc (if (> h1 h2) (cons h1 acc) (list h1))
            c1 (count new-acc)
            c2 (count res)
            new-res (if (> c1 c2) new-acc res)]
        (recur (rest xs) new-acc new-res)))))
(fn [xs]
  (letfn [(tails [xs]
            (if (seq xs)
              (cons xs (tails (rest xs)))
              ()))
          (longest-prefix [xs]
            (if (seq xs)
              (let [x (first xs)
                    paired (map vector xs (iterate inc x))
                    paired-prefix (take-while (fn [[a b]] (= a b))
                                              paired)]
                (map first paired-prefix))))]
    (let [p (->> xs tails (map longest-prefix) (apply max-key count))]
      (if (> (count p) 1)
        p
        ()))))
(fn longest-sequence
  [coll]
  (let [succ? (fn [a b] (= (+ 1 a) b))
        myconj (fn [c a] (if (> (count a) 1) (conj c a) (conj c [])))]

       (loop [prev-seqs [] curr [] [item & more] coll]
         (if (nil? item)
           (first (sort #(> (count %1) (count %2)) (myconj prev-seqs curr)))
           (if (or (empty? curr)
                   (succ? (last curr) item))
             (recur prev-seqs (conj curr item) more)
             (recur (myconj prev-seqs curr) [item] more))))))




(fn [c]
  (->>  (partition 2 1 c)
       (partition-by #(- (second %) (first %)))
       (filter #(= 1 (- (second (first %)) (ffirst %))))
       (reduce #(if (< (count %1) (count %2)) %2 %1) [])
       flatten
       distinct))
#(->> %
     (partition 2 1)
     (partition-by (fn [a] (apply < a)))
     (filter (fn [a] (apply < (first a))))
     (group-by count)
     (sort-by key >)
     first
     second
     first
     ((juxt (partial map first) (comp last last)))
     flatten
     (filter (comp not nil?)))
(fn l-sub-seq-- [coll]
  (reduce #(let [c (count %2)
                 s (sort %2)]
             (if (and (> c 1)
                      (> c (count %1))
                      (and (= %2 s)
                           (= s (distinct s))))
               %2
               %1))
          '() (mapcat #(take (count %1) (iterate butlast %1))
                      (take (count coll) (iterate rest coll)))))
#(loop [i [] c [] s %]
   ;; i = incumbent c = candidate
  (if (empty? s)
    (cond (and (< (count i) 2) (< (count c) 2)) []
          (> (count i) (count c)) i
          :else c)
    (cond (empty? c) (recur i [(first s)] (rest s))
          (= (first s) (inc (last c))) (recur i (conj c (first s)) (rest s))
          :else
          (recur (if (> (count c) (count i)) c i) [(first s)] (rest s)))))
(fn [coll]
   (loop [res []
          tres []
          curr (first coll)
          rst (rest coll)]
     (if (nil? curr)
       (if (nil? (next res)) [] res)
       (let [f (fn [x y] (= x (dec y)))
             val (f (last tres) curr)
             tr (if val
                  (conj tres curr)
                  [curr])
             r (if (> (count tr) (count res)) tr res)]
         (recur
          r
          tr
          (first rst)
          (rest rst))))))

(fn [seq]
  (letfn [(scan [f seq]
                (if (empty? seq)
                  ()
                  (cons (f seq) (scan f (rest seq)))))
          (take-inc-seq [seq]
             (let [len (count seq)
                   inc-seq-len (loop [n 1]
                                 (if (and (> len n) (< (nth seq (dec n)) (nth seq n)))
                                   (recur (inc n))
                                   (if (< n 2) 0 n)))]
               (take inc-seq-len seq)))]
    (let [inc-seqs (scan take-inc-seq seq)
          max-len (apply max (map count inc-seqs))]
      (first (filter #(= max-len (count %)) inc-seqs)))))
(fn [ints]
  (->> ints
       (partition 2 1)
       (partition-by (fn [[a b]] (< a b)))
       (filter (fn [[[a b] & _]] (< a b)))
       (concat '(()))
       (reverse)
       (apply max-key count)
       (flatten)
       (distinct)
       (vec)))
(fn h [s]
  (letfn [
          (f [[a b & r]]
            (cons a
                  (when (and b (< a b))
                    (f (cons b r)))))
          (g [s]
            (when (seq s)
              (let [a (f s)]
                (cons a
                      (g (nthrest s (count a)))))))]

    (let [x (first (sort-by #(- 0 (count %)) (g s)))]
      (if (> (count x) 1)
        x
        []))))
(fn [col] (if (empty? col)
            []
            (let [res (apply max-key count (reverse ((fn [acc col]
                                                      (if (empty? col)
                                                        acc
                                                        (let [cur (last acc) a (last cur) b (first col)]
                                                          (if (< a b)
                                                            (recur (assoc acc (dec (count acc)) (conj cur b)) (rest col))
                                                            (recur (conj acc [b]) (rest col))))))

                                                     [[(first col)]] (rest col))))]
              (if (> (count res) 1) res []))))
#(let [all_results
       (map
         (fn [[start end]]
           (subvec % start (inc end)))

         (partition 2
             (loop [c % n [] last_ false count_ 0]
               (let [this (nth c 0 0.9)
                     next_ (nth c 1 1.1)
                     good (= (inc this) next_)]
                 (println this)
                 (if
                   (= (count c) 0)
                   n
                   (if
                     (not= last_ good)
                     (recur (rest c) (conj n count_) good (inc count_))
                     (recur (rest c) n good (inc count_))))))))]






  (reduce (fn [a b] (if (> (count a) (count b)) a b)) [] all_results))

(fn [coll]
 (let [ my-find (fn my-find [colle]
                 (if (empty? colle) nil
                   (loop [coll (rest colle), startv (first colle), max-times 1, tmp (first colle), tmp-times 1]
                     (if (empty? coll)
                       (take max-times (iterate inc startv))
                       (let [x (first coll)]
                         (if (= x (+ tmp-times tmp))
                           (if (> (inc tmp-times) max-times)
                             (recur (rest coll) tmp (inc tmp-times) tmp (inc tmp-times))
                             (recur (rest coll) startv max-times tmp (inc tmp-times)))

                           (recur (rest coll) startv max-times, x, 1)))))))]
   (let [res (my-find coll)]
    (if (< 1(count res))
      res
      []))))
(fn [lst]
  (or (last (apply sorted-set (filter #(>= (count %) 2) (reduce
                                                         (fn [lsts el]
                                                            (let [
                                                                  current-list (first lsts)
                                                                  prev-el (last current-list)]

                                                             (if (< prev-el el)
                                                                 (cons (conj current-list el) (rest lsts))
                                                                 (cons [el] lsts))))
                                                         [[(first lst)]] (rest lst))))) []))

(fn [coll]
  (loop [coll coll
         n (count coll)]
    (if (< n 2)
      '()
      (let [s (some #(if (= % (take n (iterate inc (first %)))) %) (partition n 1 coll))]
        (if (nil? s)
          (recur coll (dec n))
          s)))))
(fn longest [coll]
  (loop [coll coll longest (seq '())]
    (if (= (first coll) nil)
     (if (> (count longest) 1)
        longest
      [])
     (let [next-inc-seq (loop [coll coll inc-seq [-1]]
                             (if (or (= (first coll) nil) (<= (first coll) (last inc-seq)))
                                 (rest inc-seq)
                              (recur (rest coll) (conj inc-seq (first coll)))))]
        (recur (subvec coll (count next-inc-seq))
              (if (>= (count longest) (count next-inc-seq))
                longest
               next-inc-seq))))))
(fn longest-inc-sub-seq
     ([src]
      (longest-inc-sub-seq src [] []))
     ([src result temp]
      (let [sizeT (count temp) sizeR (count result) item (first src) others (rest src)]
        (cond (empty? src) (if (> sizeT sizeR 1) temp result)
              (empty? temp) (recur others result [item])
              (> item (last temp)) (recur others result (conj temp item))
              (< sizeT 2) (recur others result [item])
              (< sizeR sizeT) (recur others temp [item])
              :else (recur others result [item])))))
(fn longsub [coll]
 (sequence (last (sort-by count (filter #(< 1 (count %))(reduce (fn [acc el]
                                                                 (cond (= (dec el) (peek (peek acc)))
                                                                  (conj (pop acc) (conj (peek acc) el))
                                                                  :else (conj acc [el])))
                                                         []
                                                         coll))))))
(fn [coll]
  (->> (partition 2 1 coll)
    (partition-by #(- (second %) (first %)))
    (filter #(= 1 (- (second (first %)) (ffirst %))))
    (reduce #(if (< (count %1) (count %2)) %2 %1) [])
    flatten
    distinct))
(fn [coll]
    (let [geti (fn [z]
                (loop [result []
                       progress []
                       elts z]
                    (cond
                        (empty? elts) (conj result progress)
                        (or (empty? progress) (< (last progress) (first elts)))
                        (recur result (conj progress (first elts)) (rest elts))
                        :else
                        (recur (conj result progress) [(first elts)] (rest elts)))))
          ls (filter #(< 1 (count %)) (geti coll))
          _ (map count ls)
          m (if (not (empty? _)) (apply max _) 0)]
      (if (= 0 m) []
        (first (filter #(= m (count %)) ls)))))
(fn [col]
  (let [diffs (partition-by (partial apply -) (partition 2 1 col))
        seqs  (filter
                 (fn [s]
                   (and (>= (count s) 1)
                        (= -1 (apply - (first s)))))
                 diffs)]
    (if (seq seqs)
      (let [longest (->> seqs reverse (apply max-key count))]
        (range (ffirst longest) (inc (second (last longest)))))
      [])))
(fn [s]
  (let [take-larger (fn [x y] (if (>= (count x) (count y)) x y))]
   (loop [s s acc [] largest []]
     (cond
      (empty? s) (let [result (take-larger largest acc)] (if (> 2 (count result)) [] result))
       (apply < (conj acc (first s)))
       (recur (rest s) (conj acc (first s)) largest)
       :else (recur (rest s) [(first s)] (take-larger largest acc))))))
(fn [cs] ;gosh this is horrible - really need to figure out a nice way here...
  (loop [sub [] longest [] cs cs]
    (if (= cs '())
      (let [x (if (> (count sub) (count longest)) sub longest)] (if (= (count x) 1) [] x))
      (recur (if (or (= sub []) (> (first cs) (last sub)))
               (conj sub (first cs))
               [(first cs)])
             (if (>= (count longest) (count sub)) longest sub)
             (rest cs)))))
(fn lis [s]
  (loop [c s
         inseq false
         best []
         current []]
    (if (empty? (next c))
      best
      (let [n (first (next c))
            current (cond (and inseq (> n (last current)))
                        (conj current n)
                        (> n (first c))
                        [(first c) n]
                        :else [])
            nbest (if (> (count current) (count best)) current best)]
        (recur (next c) (not (empty? current)) nbest current)))))
(fn compl [q] (let [ conq (fn conq [l]
                              (if (and (not(empty? l)) (not(nil? (first (next l)))))
                                (let [f (first l) n (first (next l)) z (conq (rest l))]
                                  (if (= n (+ f 1))
                                    (cons f (if (or (empty? z) (nil? z)) [n] z))))))

                       aseq (fn aseq [l] (if (empty? l) [] (let [z (conq l)] (if (nil? z) (aseq (rest l)) (cons z (if (> (count z) 0) (aseq (take-last (- (count l) (count z)) l)) []))))))

                       res (aseq q)

                       fin (first (reverse(sort-by count res)))]
                   (if (nil? fin) [] fin)))
#(first (reduce

         (fn [[l g p] c]
           (if (> c p)
             (let [g (conj g c)]
               [(if (> (count g) (count l)) g l) g c])
             [l [c] c]))

         [[] [(first %)] (first %)] %))
(fn lis [s]
   (if (= 1 (count s))
     '()
     (if (apply < s)
       s
       (let [a (lis (rest s))
             b (lis (butlast s))]
         (if (> (count a) (count b))
           a
           b)))))
(fn [s]
    (loop [z s curr [] best []]
         (if (empty? z)
          best
          (let [i (first z)
                  newCurr (if (or (empty? curr) (> i (last curr)))
                            (conj curr i)
                            [i])
                newBest (if (and (> (count newCurr) 1) (> (count newCurr) (count best))) newCurr best)]

               (recur (rest z) newCurr newBest)))))
#(let [r (apply (partial max-key count) (reverse ((fn f
                                                   ([[a b & c]] (if b (if (< a b)
                                                                       (let [[x & y] (f (cons b c))] (into [(into [a] x)] y))
                                                                       (into [[a]] (f (cons b c))))
                                                                 [[a]])))
                                                  %)))] (if (= 1 (count r)) [] r))
(fn [l] (reverse (reduce #(if (>= (count %1) (count %2)) %1 %2) '() (filter #(< 1 (count %))
                                                                     ((fn increasing-subseq [l]
                                                                        (loop [s '() r l p -1000]
                                                                          (cond
                                                                            (empty? r)      (list s)
                                                                            (> (first r) p) (recur (cons (first r) s) (rest r) (first r))
                                                                            :else           (cons s (increasing-subseq r))))) l)))))
(fn [x]
  (let [paired (map list x (rest x))
        pred (fn [[a b]] (< a b))
        split (partition-by pred paired)
        increasing (filter (fn [a] (pred (first a))) split)]

    (if (not (seq increasing))
      []
      (let [longest-len (apply max (map count increasing))
            longest-seqs (filter #(= longest-len (count %)) increasing)
            first-seq (first longest-seqs)]
        (concat (map first first-seq) [(last (last first-seq))])))))




#(first
  (reduce (fn [[l acc] n]
           (cond
            (empty? acc)   [l [n]]
            (< (last acc) n)
            (let [newacc (conj acc n)]
                (if (< (count l) (count newacc)) [newacc newacc] [l newacc]))
            true [l [n]]))


          [[] []] %))
(fn myIncreasingSequence
  [coll]
  (let [min (- (apply min coll) 1)]
    (let [result (reduce #(if (>= (count %1) (count %2) 1) %1 %2) []
                  (partition-by #(= min %)(reduce #(if (<= %2 (last %1)) (conj %1 min %2) (conj %1 %2)) (vector (first coll)) (rest coll))))]
     (if (>= 1 (count result)) [] result))))
(fn [s]
  (let [foo
        (filter
         #(= (inc (first %)) (last %))
         (first
           (sort
             #(> (count %) (count %2))
             (partition-by
               #(- (last %) (first %))
                (partition 2
                  (interleave (drop-last s) (rest s)))))))]
    (concat (first foo) (map last (rest foo)))))
(fn longest
   ([lst] (longest (rest lst) [(first lst)] []))
   ([lst cur best]
    (let [new-best (if (and (> (count cur) 1) (> (count cur) (count best))) cur best)
          new-num (first lst)
          last-num+1 (inc (last cur))]
      (cond (empty? lst) new-best
            (= new-num last-num+1) (recur (rest lst) (conj cur new-num) new-best)
            :else (recur (rest lst) [new-num] new-best)))))
(fn [xs]
    (reverse (loop [xs xs t (list (first xs))  winner '()]
               (cond
                (empty? xs) winner
                (>= (first t) (first xs))
                (recur (rest xs) (list (first xs)) winner)
                :else
                (recur (rest xs) (cons (first xs) t)
                       (if (>= (count winner) (count (cons (first xs) t)))
                         winner
                         (cons (first xs) t)))))))
(fn longest-inc-seq [coll]
 (reduce #(let [len-a (count %1)
                len-b (count %2)]
           (if (and (> len-b 1) (> len-b len-a)) %2 %1))
  []
  (reductions
   (fn [xs y]
    (if (> y (last xs)) (conj xs y) [y]))
   [(first coll)]
   (rest coll))))
(fn [xs]
  (letfn [(biggest [xs ys]
            (if (>= (count xs) (count ys)) xs ys))]
    (loop [rest-xs xs last-sln [] next-sln []]
      (if-let [x (first rest-xs)]
        (let [y (last next-sln)]
          (if (or (not y) (> x y))
            (recur (rest rest-xs) last-sln (conj next-sln x))
            (recur (rest rest-xs) (biggest last-sln next-sln) [x])))
        (let [sln (biggest last-sln next-sln)]
          (if (> (count sln) 1) sln []))))))
(fn max-inc [seq]
 (let [x
       (apply max-key count
        (reverse
            ((fn break-when-decreasing
                [seq]
                ((fn go [vec seq]
                    (cond
                        (empty? seq) vec
                        (<= (first seq) (peek (peek vec))) (go (conj vec [(first seq)]) (rest seq))
                        :else (go (conj (pop vec) (conj (peek vec) (first seq))) (rest seq))))


                 [[(first seq)]]
                 (rest seq)))


             seq)))] (if (= (count x) 1) [] x)))
(let [longest
      (fn [leftover best current]
        (if (nil? leftover)
          (if (>= (count best) 2) best [])
          (if (> (first leftover) (last current))
            (if (> (count (conj current (first leftover))) (count best))
              (recur (next leftover) (conj current (first leftover)) (conj current (first leftover)))
              (recur (next leftover) best (conj current (first leftover))))

            (recur (next leftover) best [(first leftover)]))))]


 (fn [z] (longest (next z) [(first z)] [(first z)])))

(fn [-seq]
   (->> (map
         (fn [it index]
           (loop [next-items (next (last (split-at index -seq))) stock [it]]
             (if (= (first next-items) (inc (last stock)))
               (recur (rest next-items) (conj stock (first next-items)))
               stock)))

         -seq (range (count -seq)))

        (map  #(if (= 1 (count %)) [] %))
        (sort #(compare (count %2) (count %1)))
        (first)))



(fn [s]
  (loop [buf-max []
         buf []
         [i1 & r] s]

    (cond
     (empty? r)  (let [buf (conj buf i1)]
                  (if (and (> (count buf) (count buf-max)) (> (count buf) 1))
                    buf
                    buf-max))
     (< i1 (first r)) (recur buf-max (conj buf i1) r)
     :else (let [buf (conj buf i1)]
             (if (and (> (count buf) (count buf-max)) (> (count buf) 1))
               (recur buf [] r)
               (recur buf-max [] r))))))





(fn [nums]
  (let [res
        (apply max-key count
          (reverse
            (reduce
             (fn [runs [a b]]
               (let [next-iter (update-in runs [(dec (count runs))] conj a)]
                (if (< a b)
                  next-iter
                  (conj next-iter []))))
             [[]]
             (conj (vec (partition 2 1 nums)) [(last nums) Integer/MAX_VALUE]))))]

   (if (> (count res) 1) res [])))
(fn [bl]
  (apply max-key count '()
         (filter #(> (count %) 1)
                 (reductions
                  (fn [l i]
                    (if (= (last l) (dec i))
                      (concat l (list i))
                      (list i)))
                  '() bl))))
#(reduce (fn [longest s] (let [c (count s)] (if (and ( > c 1) ( > c (count longest))) s longest)))
         []
         (map (partial map second)
              ((fn separate [pairs]
                 (when (seq pairs)
                   (let [run (cons (first pairs) (take-while (partial apply <) (rest pairs)))]
                     (cons run (separate (drop (count run) pairs))))))
               (map vector (cons nil %) %))))
(fn [coll]
   (let [seqs (reverse (map #(concat (first %) (map (fn [i] (last i)) (rest %)))
                        (filter #(every? (fn [ele] (< (first ele) (last ele))) %)
                          (partition-by
                            #(< (first %) (last %))
                            (partition 2 1 coll)))))]
    (if (empty? seqs)
     []
     (apply (partial max-key count) seqs))))
(fn long-cons
  ([col]
   (long-cons (vector (first col)) [] (rest col)))
  ([col-acum col-best col]
   (if (empty? col)
     (if (and (> (count col-acum) 1) (> (count col-acum) (count col-best)))
         col-acum
         col-best)
     (if (= (last col-acum) (dec (first col)))
         (long-cons (conj col-acum (first col))
                    (if (and (> (count col-acum) 1) (> (count col-acum) (count col-best)))
                      col-acum
                      col-best)
                    (rest col))
       (if (> (count col-acum) (count col-best))
           (long-cons (vector (first col))
                      (if (and (> (count col-acum) 1) (> (count col-acum) (count col-best)))
                        col-acum
                        col-best)
                      (rest col))
           (long-cons (vector (first col)) col-best (rest col)))))))




(fn longest
  ([l] (longest [] [] nil l))
  ([ge now lst l]
   (if (empty? l) (let [as (if (< (count ge) (count now)) now ge)] (if (< 1 (count as)) as []))
     (if (and ((complement nil?) lst) (= (inc lst) (first l)))
       (longest ge (conj now (first l)) (first l) (next l))
       (longest (if (< (count ge) (count now)) now ge) [(first l)] (first l) (next l))))))
(fn [v]
  (loop [v v
         tmp []
         best []]
    (let [a (first v)
          best (if (> (count tmp) (count best)) tmp best)]
      (if-not a
        (if (> (count best) 1)
          best [])
        (if (and (seq tmp) (> a (last tmp)))
            (recur (rest v) (conj tmp a) best)
            (recur (rest v) [a] best))))))

#(loop [input % curr [] output []]
   (let [max (if (and (> (count curr) 1) (> (count curr) (count output))) curr output)]
     (cond
       (empty? input) max
       (empty? curr) (recur (rest input) (conj curr (first input)) max)
       (> (first input) (last curr))
       (recur (rest input) (conj curr (first input)) max)
       :else (recur input [] max))))

(fn [coll]
  (let [r (->>
           (reduce (fn [res elem]
                       (if-let [prev (last res)];存在的话
                         (if (> elem (last prev))
                           (conj res (conj prev elem))
                           (conj res [elem]))
                         (conj res [elem]))) [] coll)
           (group-by count)
           (vals)
           (last)
           (first))]
   (if (= 1 (count r))
     []
     r)))
(fn longest-inc-sub-seq [s]
    (let [longest (first (sort (fn [a b] (> (count a) (count b)))
                          (filter
                             (fn [ss] (>= (count ss) 2))
                             (map (fn [x] (take-while #(not (nil? %)) (reduce #(if (empty? %1)
                                                                                [%2]
                                                                                (if (= (last %1) (dec %2))
                                                                                   (conj %1 %2)
                                                                                   (conj %1 nil)))

                                                                       []
                                                                       x)))
                              (map vec (partition-all (count s) 1 s))))))]
      (cond
         (empty? longest) []
         :else longest)))

(fn [x]
    (let [res
              (reduce #(if (> (count %2) (count %1)) %2 %1)
                (reduce (fn [[acc k] i]
                          (if (or (empty? k) (= (inc (last k)) i))
                            (vector acc (conj k i))
                            (if (> (count k) (count acc))
                              (vector k (vector i))
                              (vector acc (vector i)))))



                  (vector (vector) (vector (first x)))
                  (rest x)))]



      (if (> (count res) 1)
        res
        ())))



(fn [xs] (let [min-size (fn [xs] (if (>= (count xs) 2) xs []))
               sort-count (fn [xs]
                           (map second (sort-by first (map-indexed #(list (- (* (count %2) 10) %1) %2) xs))))
               increasing (fn [xs]
                           (let [ aux (fn aux[prev, xs, acc]
                                        (if (empty? xs)
                                          acc
                                          (if (< prev (first xs))
                                            (aux (first xs) (rest xs) (conj acc (first xs)))
                                            acc)))]
                             (sort (aux (dec (first xs)) xs, '()))))]

          (min-size (last  (sort-count  ( map-indexed (fn [i,x] (increasing (drop i xs)) ) xs))))))

(fn [lst]
  ((reduce (fn [[f rv res] e]
             (let [newrv (conj rv e)]
               [#(= % (inc e))
                (if (f e) newrv [e])
                (if (and (f e) (> (count newrv) 1) (> (count newrv) (count res)))
                  newrv
                  res)]))
           [#(not (nil? %)) [] []]
           lst) 2))
(fn longest-subsequence [coll]
  (apply (fn longest [& colls]
          (if (empty? colls) ()
             (reduce (fn longer [a b]
                      (if (>= (count a) (count b))
                        a b)) colls))) (filter #(apply < %) ((fn subsequences [coll]
                                                              (for [a (range (count coll))
                                                                    b (range (count coll))
                                                                    :when (and
                                                                            (> 2 (- a b))
                                                                            (< a b))]
                                                                (subvec coll a (inc b)))) coll))))
(fn [coll](let [lt (fn [[x y]] (< x y))]
           (->>
            (partition 2 1 coll)
            (partition-by lt)
            (map #(filter lt %))
            (map flatten)
            (map #(map first (partition-by identity %)))
            (reverse)
            (apply max-key count))))
(fn a [col] (let [x (last (keep #(if-not(nil? %) %) (for [i (range 2 (inc (count col)))] (->> (partition i 1 col) (keep #(if(apply < %) %)) (first)))))] (if-not(nil? x)(identity x)(identity []))))
(fn long-inc-subseq [coll]
  (->> (partition 2 1 coll)
    (partition-by #(< (first %) (second %)))
    (filter #(< (ffirst %) (second (first %))))
    (sort #(> (count %1) (count %2)))
    (first)
    (reduce #(if (empty? %1) (into %1 %2) (conj %1 (second %2))) [])))
(fn[s]
  (->> s
    (reductions (fn [p v] (if (-> p last (= (dec v))) (conj p v) [v])) [])
    (filter (comp seq rest))
    (sort-by count >)
    first vec))
(fn [xs]
  (loop [i 0 longest []]
    (if (< i (count xs))
      (let [increasing (loop [j i ret []]
                         (if (and (< j (count xs)) (or (empty? ret) (> (get xs j) (last ret))))
                           (recur (inc j) (conj ret (get xs j)))
                           ret))]
        (if (and (> (count increasing) (count longest)) (> (count increasing) 1))
          (recur (inc i) increasing)
          (recur (inc i) longest)))
      longest)))
(fn [s]
    (loop [rem-seq s rslt [] curr-seq []]
      (cond (empty? rem-seq)
            (vec (last (sort #(compare (count %1) (count %2)) (if (> (count curr-seq) 1)
                                                               (reverse (cons (reverse curr-seq) rslt))
                                                               (reverse rslt)))))
            (or (empty? curr-seq) (= (- (first rem-seq) (first curr-seq)) 1))
            (recur (rest rem-seq) rslt (cons (first rem-seq) curr-seq))
            :else
            (if (> (count curr-seq) 1)
              (recur (rest rem-seq) (cons (reverse curr-seq) rslt) [(first rem-seq)])
              (recur (rest rem-seq) rslt [(first rem-seq)])))))
#(first (reduce (fn [[l c] x]
                 (if (and (seq c) (> x (last c)))
                   (let [nc (conj c x)]
                     (if (> (count nc) (count l))
                       [nc nc]
                       [l nc]))


                   [l (vector x)]))


         [() ()]
         %))
(fn myf [s] (first (reduce (fn[a x](let [[m c] a nextc (conj c x)]
                                    (if (<= x (peek c))
                                     [m [x]]
                                     (if (< (count m)(count nextc))
                                       [nextc nextc]
                                       [m nextc]))))

                       [[] [(first s)]] (rest s))))
(fn [coll]
  (->> (partition 2 1 coll)
       (partition-by #(- (second %) (first %)))
       (filter #(= 1 (- (second (first %)) (ffirst %))))
       (apply max-key count [])
       flatten
       distinct))
(fn [coll]
  (->> (partition 2 1 coll)
    (partition-by #(- (second %) (first %)))
    (filter #(= 1 (- (second (first %)) (ffirst %))))
    (reduce #(if (< (count %1) (count %2)) %2 %1) [])
    flatten
    distinct))
(fn sub [x] (letfn [(head [x] (cond (= (count x) 1) x :else (cond (= (first x) (dec (second x)))
                                                                  (cons (first x) (head (rest x)))
                                                                  :else (list (first x)))))
                    (allhead [x] (cond (empty? x) nil :else (cons (head x) (allhead (rest x)))))]
                   (let [sol (first (reverse (sort-by count (allhead x))))]
                        (cond (= (count sol) 1) []
                              :else sol))))
(fn [coll]
   (let [mymax (fn ([] 0)
                 ([& args] (apply max args)))
         subs (filter (fn [[[x y] & ns] & seqs]
                        (< x y))
                      (partition-by (fn [[a b]] (< a b))
                                    (partition 2 1 coll)))
         maxcount (apply mymax (map count subs))
         right-one (first (drop-while #(not= maxcount (count %))
                                      subs))
         result (flatten [(mapv first (butlast right-one))
                          (last right-one)])]
     (if (= result '(nil))
       []
       result)))
(fn longest-subseq
  ([sequ] (longest-subseq sequ ()))
  ([sequ longest]
   (let [func (fn [s]
                (let [ss (take-while #(< (first %1) (last %1)) (partition 2 1 s))]
                   (if (empty? ss) () (concat (map first ss) [(last (last ss))]))))
         sub  (func sequ)]
     (cond
       (empty? sequ)       longest
       (empty? sub)        (longest-subseq (rest sequ) longest)
       (not (empty? sequ)) (longest-subseq (drop (count sub) sequ) (if (> (count sub) (count longest)) sub longest))))))
(fn [col]
    (:best (reduce
            (fn [dict x]

                (let [best (:best dict)
                      prev (:current dict)
                      current (if (or (empty? prev)(> x (last prev)))
                                 (conj prev x)
                                 [x])]
                 (if (and (> (count current) (count best)) (> (count current) 1))
                     {:best current
                      :current current}
                     {:best best
                      :current current})))
            {:best []
             :current []}
            col)))
(fn [c]
 (let [[_ n i]
       (reduce
        (fn [[n m j] e]
          (let [i (and (< (nth e 0) 0) (> (count e) m))]
            [(+ n (count e))
             (if i (count e) m)
             (if i n j)]))
        [0 0 0]
        (partition-by neg? (map #(apply - %) (partition 2 1 c))))]
   (if (> n 0)
     (subvec c i (+ i n 1))
     [])))
(fn myfun2 [lst]
  (letfn [(consecutive? [[a b]] (= (- b a) 1))]
         (->> (partition 2 1 lst)
              (drop-while (comp not consecutive?))
              (partition-by consecutive?)
              (take-nth 2)
              (sort-by (comp - count))
              (first)
              (flatten)
              (distinct))))
(fn [x]
  (loop [l [] c [] [x1 & r] x]
    (let [ll (if (and (> (count c) 1) (> (count c) (count l))) c l)]
      (if (nil? x1) ll
        (if (= (last c) (dec x1))
          (recur l (conj c x1) r)
          (recur ll [x1] r))))))
(fn [s]
  (let [increasing? (fn [xs] (apply < xs))
        n (count s)
        sub-seqs (mapcat #(partition % 1 s) (range 2 (inc n)))]
    (->> sub-seqs
         (filter increasing?)
         (cons [])
         (sort-by count >)
         first)))
(fn LCS
  [xs]
  (loop [coll (rest xs) result [(first xs)]]
    ;; (pprint result)
    (if (empty? coll)
      (if (< (count result) 2)
        []
        result)
      (if (> (first coll) (last result))
        (recur (rest coll) (conj result (first coll)))
        (let [ret (LCS coll)]
          (if (> (count ret) (count result))
            ret
            (if (< (count result) 2)
              []
              result)))))))
(fn party [xs]
  (let [bef-aft (fn [xs] (map list xs (concat [nil] (butlast xs))))
        reducer #(if (nil? (second %2)) (conj % 0) (if (> (first %2) (second %2)) (conj % (last %)) (conj % (inc (last %)))))
        paired (map list xs (reduce reducer [] (bef-aft xs)))
        chunks (map #(map first %) (partition-by second paired))
        longest (reduce #(if (>= (count %) (count %2)) % %2) chunks)]

    (if (> (count longest) 1) longest [])))

(fn longest-increasing-subsequence
  [coll]
  (letfn [(subsequence-map [coll n] (filter #(= (range (first %) (+ n (first %))) %) (partition-all n 1 coll)))]
      (loop [n (count coll)
             s (seq coll)
             result []]
        (if (not (empty? (subsequence-map s n)))
          (if (= 1 n)
            []
           (into [] (first (subsequence-map s n))))
          (recur (dec n) s result)))))
(fn [s] (letfn [(gen-subseq [s] (mapcat (fn [s] (map #(subvec s 0 %) (range 0 (inc (count s)))))
                                      (map #(subvec s %) (range 0 (inc (count s))))))]
               (last (sort #(< (count %1) (count %2)) (reverse (filter #(not (= (count %) 1)) (filter #(= % (vec (apply sorted-set %))) (gen-subseq s))))))))
(fn [coll] (or (first (filter #(apply < %) (mapcat #(partition % 1 coll) (range (count coll) 1 -1)))) []))
(fn longest-inc-seq [coll]
  (reduce #(let [len-a (count %1)
                 len-b (count %2)]
             (if (and (> len-b 1) (> len-b len-a)) %2 %1))
    []
    (reductions
      (fn [xs y]
        (if (> y (last xs)) (conj xs y) [y]))
      [(first coll)]
      (rest coll))))
(fn p53-longest-increasing-sub-seq [s]
  (let [begin-sequences
        (fn [s] (rest (reductions (fn [a b] (conj a b)) [] s)))
        end-sequences
        (fn [s] (reverse (reduce (fn [mem a] (conj (map (fn [b] (flatten [b a])) mem) (list a))) [] s)))
        longest
        (fn [s] (reduce (fn [longest x] (if (> (count x) (count longest)) x longest)) [] s))]
    (->> (apply concat (map begin-sequences (end-sequences s)))
         (filter (fn [s] (apply < s)))
         (filter (fn [s] (> (count s) 1)))
         (longest))))

#(apply max-key %
   (for [x (%2 (% %3) 0 -1)
         y (%2 (- x 1))
         :let [z (subvec %3 y x)]]
     (if (apply < z) z [])))
count range
(fn test
  [input]
  (->> (partition 2 1 input)
       (partition-by #(- (second %) (first %)))
       (filter #(= 1 (- (second (first %)) (ffirst %))))
       (reduce #(if (> (count %2) (count %1)) %2 %1 ) [])
       flatten
       distinct))
(fn [coll] (reduce)
       #(let [max (if (> (count %1) (count %2)) %1 %2)]
          (if (> (count max) 1)))
      max
      [
          (reduce
           #(let [n (last (last %1))]
              (if (and n (= (inc n) %2)))))
       (update-in %1 [(dec (count %1))] conj %2)
       (conj %1 [%2]
           [] coll)])
