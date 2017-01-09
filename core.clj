(ns nlp.core)

(import [java.io File FileReader BufferedReader FileWriter BufferedWriter])

(require '[clojure.string :as str])

(require '[clojure.core.reducers :as r])


(defn open[file]
  (-> file File. FileReader. BufferedReader.))

(defn getLines[reader]
  (loop [acc []]
    (let [line (.readLine reader)]
      (if (empty? line)
        acc
        (recur (conj acc (str "NULL " line)))))))

(defn get_words[sent] (str/split sent #"\s+"))

(defn reader[file]
  (open file))


(def eng_reader (reader "/media/chege/New Volume/courses/1st priority/nlp/assignment/h3 - p/corpus.eng"))

(def esp_reader (reader "/media/chege/New Volume/courses/1st priority/nlp/assignment/h3 - p/corpus.esp"))

(def esp_sents (getLines esp_reader))

(def eng_sents (getLines eng_reader))

(def eng_words (map get_words eng_sents))

(def esp_words (map get_words esp_sents))

(defn make_map[e f]
  (let [l (count e)
        m (- (count f) 1)
        J (range l)
        I (range 1 m)]
    {:e e :f f :l l :m m :I I :J J}))

(def maps (map make_map eng_words, esp_words))

(defn get_values
  [{:keys [J I l m]}]
     (map (fn[i] [i l m J]) I))

(def values (mapcat get_values maps))

(def parallel (map (fn[e f] [e (rest f)]) eng_words, esp_words))

(defn get_counts[value]
  (subvec value 0 3))


(defn get_alignment[value]
  (let [{i 0, l 1, m 2, J 3} value]
    (map (fn[j] [j,i,l,m]) J)))

(defn get_bigrams[parallel]
  (let [e (first parallel)
        f (last parallel)]
  (mapcat (fn[j] (map (fn[i] [i j]) e)) f)))

(defn fn_counts
  [map-fun fun batch doc]
  (r/fold (partial merge-with +)
          (pmap #(frequencies (map-fun fun %))
                (partition-all batch doc))))

(def make_counts #(fn_counts map get_counts 500 %))

(def make_alignments #(fn_counts mapcat get_alignment 1000 %))

(def make_bigrams #(fn_counts mapcat get_bigrams 1000 %))

(def make_unigrams #(fn_counts mapcat get_words 500 %))

(def unigrams (make_unigrams eng_sents))

(def bigrams (make_bigrams parallel))

(def alignments (make_alignments values))

(def counts (make_counts values))

(.close eng_reader)

(.close esp_reader)
