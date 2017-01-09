(defn throw-catch [f]
  (try
    (f)
    (catch ArithmeticException e "no dividing by zero buddy")
    (catch Exception e (str "you are so bad " (.getMessage e)))
    (finally (println "will always return"))))


(defn print-seq [s]
  (when (seq s)
    (prn (first s))
    (recur (rest s))))


(defn compatible [tag1 tag2]
  (let [N {"NULL" true "NN" true "IN" true "VB" false}
        V {"NULL" true "VB" true "NN" false "IN" false}
        n (and (N tag1) (N tag2))
        v (and (V tag1) (V tag2))]
    (or n v)))

(for [[eng_word eng_tag] e
      [esp_word esp_tag] f
      :when (compatible eng_tag esp_tag)]
  [eng_word esp_word])



(pmap #(mapcat (fn[x] [(first x) (/ (last x) s)]) %) (partition-all 10 (take 100 unigrams))))


(defn feats_diff[a b]
  (let [vals (into (into #{} (keys a)) (keys b))
        vals_a (map #(get a % 0) vals)
        vals_b (map #(get b % 0) vals)
        diff (zipmap vals (map - vals_a vals_b))
        ]
    (into {} (filter (fn[x] (not (= (last x) 0))) diff))))

(defn update_weights[weights updates]
  (send weights (partial merge-with +) weights updates))
        
(defn getlines [reader]
  (loop [acc []]
    (let [line (.readLine reader)]
      (if (empty? line)
        acc
        (recur (conj acc line))))))
          

(pmap (fn[x] (map (comp #({(first %) (Double. (last %))}) #(str/split % #"\s+")) x)) (partition-all 5 a))


(def model (reduce merge 
                   (pmap (fn[x] (apply hash-map (mapcat 
                                                 (comp #(vec [(first %) (Double. (last %))]) #(str/split % #"\s+")) 
                                                 x))) 
                         (partition-all 100 model_str)))))


(for [k (range 10)
      u U
      v V]
  (let [current [k u v]]
    (map W)


(def fib
  (memoize
   (fn[n]
     (cond
       (<= n 0) 0
       (< n 2) 1
       :else (+ (fib (- n 1)) (fib (- n 2)))))))


(def mem-fib (memoize fib))

(defn viterbi[U V W sent])



(defn make_map[w u v sent k]
  (if (< k 0)
    {}
    {:y2 w :y1 u :y v :word (get sent k "") :i k :words sent}))

(def model-value (comp inner_prod tri_uni make_map))

(def pi-table
  (memoize
   (fn[k u v]
     (cond 
       (= k (- 1))
       (cond
         (and (= v "*") (= u "*")) [1 nil]
         :else [0 nil])
       :else (last (sort (map (fn[w] [(+ ((pi-table (- k 1) w u) 0) (model-value w u v a k)) w]) W)))))))

    


(defn tags[n top]
  (loop [n (rseq (vec (range (- n 2))))
         top top]
    (if (seq n)
      (recur (rest n) (conj top ((pi-table (+ (first n) 2) (first n) (second n)) 1)))
      top)))



(defn patt[x] [(first x) (first (tags x))])

(def not_resolved (comp flatten vals (partial filter not_one?)))


(defn get_features [data] (map #(join ":" (patt %)) (filter one? data)))

(defn high_ent[k pat] (comp (partial red k pat) flatten vals (partial filter not_one?)))

(def one? (comp #(= (count %) 1) tags))

(def not_one? (complement one?))

(def tags (fn[[k v]] (apply hash-set (map :y v))))

(def all_hists (flatten histories))

(defn red[k pad data] (reduce (partial merge-with into) (map (fn[x](hash-map (join ":" (conj (-> (select-keys x k) vals) pad)) [x])) data)))

(def tag (red [:word] "TAG" all_hists))

(def uni (filter one? tag))

(def uni_feats (get_features uni))

(def prev  ((high_ent [:y1 :word] "PREV:TAG") tag))

(def prev_feats (get_features prev))

(def prev_prev ((high_ent [:y2 :y1 :word] "PREV-1:PREV:TAG") prev))

(def prev_prev_feats (get_features prev_prev))

(def pos_prev_prev ((high_ent [:pos :y2 :y1 :word] "POS:PREV-1:PREV:TAG") prev_prev))

(def pos_prev_prev_feats (get_features pos_prev_prev))

(def prev_pos_pos_prev_prev ((high_ent [:pos1 :pos :y2 :y1 :word] "POS-1:POS:PREV-1:PREV:TAG") pos_prev_prev))

(def prev_pos_pos_prev_prev_feats (get_features prev_pos_pos_prev_prev))

(def pos_pos_pos_prev_prev ((high_ent [:pos2 :pos1 :pos :y2 :y1 :word] "POS-1:POS:PREV-1:PREV:TAG") prev_pos_pos_prev_prev))

(def pos_pos_pos_prev_prev_feats (get_features pos_pos_pos_prev_prev))


(def skip_tag ((high_ent [:y2] "POS-1:SKIP:TAG") pos_pos_pos_prev_prev))



