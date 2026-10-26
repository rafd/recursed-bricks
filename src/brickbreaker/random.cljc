(ns brickbreaker.random)

;; Park-Miller PRNG. Products stay below 2^53, so clj and cljs give the same numbers.
(def modulus 2147483647)

(defn normalize-seed [seed]
  (inc (mod seed (dec modulus))))

(defn next-seed [seed]
  (mod (* 16807 seed) modulus))

(defn seed->fraction [seed]
  (/ (double seed) modulus))

(defn random-bricks
  [{:keys [seed columns rows fill-percent]}]
  (let [cells (for [row (range rows)
                    column (range columns)]
                [column row])
        seeds (rest (iterate next-seed (normalize-seed seed)))]
    (->> (map vector cells seeds)
         (filter (fn [[_cell cell-seed]]
                   (< (* 100 (seed->fraction cell-seed)) fill-percent)))
         (map first)
         set)))

#_(random-bricks {:seed 42 :columns 10 :rows 10 :fill-percent 60})
