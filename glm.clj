(ns nlp.glm)


(import [java.io File FileReader BufferedReader FileWriter BufferedWriter])

(require '[clojure.string :as str])

(require '[clojure.core.reducers :as r])

(require '[clojure.set :as s])


(defn reader[file] (-> file File. FileReader. BufferedReader.))


(defn getLines[reader]
  (with-open [file reader]
    (loop [acc []]
      (if-let [line (.readLine file)]
        (recur (conj acc line))
        acc))))


(defn make_gold_set[key_tag data]
  (let [all_keys (set (keys (first data)))
        all_but_words (s/difference all_keys #{key_tag})
        values (zipmap all_keys (map (comp vec #(map % data)) all_keys))]
    (with-meta (key_tag values) (assoc (select-keys values all_but_words) :n (count data)))))

(def gold_set (partial make_gold_set :words))



(defn join[sep s]
  (apply str (interpose sep s)))


(defn ngrams[sequence]
  (fn[label]
    (let [sq (-> sequence seq (conj "*" "*") vec)
          n (+ (- (count sq) 3) 1)
          len (range n)]
      (map (comp label seq #(subvec sq % (+ % 3))) len))))


(def U ["*" "O" "I-GENE"])

(def W U)

(def V ["O" "I_GENE"])


(defn trigram[{:keys [y2 y1 y]}]
  (join ":" ["TRIGRAM" y2 y1 y]))

(defn unigram[{:keys [y word]}]
  (join ":" ["TAG" word y]))

(defn make_features[feats history]
    (zipmap (map #(% history) feats) (repeat (count feats) 1)))


(def tri_uni (partial make_features [unigram trigram]))


(def training_file "/media/chege/New Volume/courses/1st priority/nlp/assignment/h4 - p/train.data")

(def training_reader (reader training_file))

(def training_str (getLines training_reader))

(def model_file "/media/chege/New Volume/courses/1st priority/nlp/assignment/h4 - p/tag.model")

(def model_reader (reader model_file))

(def model_str (getLines model_reader))

(def model
  (r/fold (partial merge-with +)
  (pmap
   (fn[x] (reduce merge (map
           (comp
            #(hash-map (first %) (Double. (last %)))
            #(str/split % #"\s+")
            )
           x)))
   (partition-all 500 model_str))))

(def select-model-features (partial select-keys model))

(defn prod[model]
  (fn[sent]
  (r/fold + (map #(* (get model (key %) 0) (val %)) sent))))

(def inner_prod (prod model))


(defn training_with_meta[tags]
  (partial map
           (comp gold_set
                 #(map
                   (fn[x](zipmap tags (str/split x #"\s")))
                   %)
              #(str/split % #"\t"))))

(def training ((training_with_meta [:words :pos :tags]) training_str))

(defrecord hist[pos2 pos1 pos y2 y1 y i word words])

(defn history[{:keys [:pos2 :pos1 :pos :y2 :y1 :y :i :word :words]}] (hist. pos2 pos1 pos y2 y1 y i word words))

(defn make_hists[data]
  (let [pos ((-> data meta :pos ngrams) (partial zipmap [:pos2 :pos1 :pos]))
        tags ((-> data meta :tags ngrams) (partial zipmap [:y2 :y1 :y]))
        n (-> data meta :n)
        i (map hash-map (repeat n :i) (range n))
        word (map hash-map (repeat n :word) (map data (range n)))
        words (map #(hash-map % data) (repeat n :words))
        ]
    (map (comp history merge) pos tags i word words)))

(def histories (map make_hists training))


(def features (map (comp #(apply (partial merge-with +) %) #(map tri_uni %)) histories))
