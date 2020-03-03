(ns test-xml-parser.core
  (:require
   [clojure.xml :as xml]
   [clojure.zip :as zip]
   [clojure.pprint :as pprint]
   [clojure.string :as str])
  (:import [java.io File]))

(defn deep-merge [a & maps]
  (if (map? a)
    (apply merge-with deep-merge a maps)
    (apply merge-with deep-merge maps)))

(defn zip-str [s]
  (zip/xml-zip
   (xml/parse (java.io.ByteArrayInputStream. (.getBytes s)))))

(defn find-testcase-tag [xml-content]
  (let [filter-result (filter #(= (:tag %) :testcase) xml-content)]
    (if (empty? filter-result)
      (if (contains? (first xml-content) :content)
        (find-testcase-tag (:content (first xml-content)))
        nil)
      filter-result)))

(defn find-tags [xml-content tag-key]
  (let [filter-result (filter #(= (:tag %) tag-key) xml-content)]
    (if (empty? filter-result)
      (if (contains? (first xml-content) :content)
        (find-tags (:content (first xml-content)) tag-key)
        nil)
      filter-result)))

(defn filter-tags [xml-content tag-key]
  (let [filter-result (filter #(= (:tag %) tag-key) xml-content)]
    filter-result))

(defn get-tag [xml-content tag-key]
  (let [filter-result (filter #(= (:tag %) tag-key) xml-content)]
    (if (empty? filter-result)
      (if (contains? (first xml-content) :content)
        (find-tags (:content (first xml-content)) tag-key)
        nil)
      filter-result)))

(defn hierarchy [keyseq xs]
  (reduce (fn [m [ks x]]
            (update-in m ks conj x))
          {}
          (for [x xs]
            [(map x keyseq) (apply dissoc x keyseq)])))

(defn group-testcase-by-name [testcase-list]
  (group-by #(last (str/split (:classname (:attrs %)) #"\." )) testcase-list))

(defn group-testcase-data [data]
  (->> data
       (group-by #(last (str/split (:classname (:attrs %)) #"\." )))
       (map (fn [[k vals]] [k (first (map :attrs vals))]))
       (into {})))

(defn get-data [arg]
  (let [zip-val (zip-str arg)]
    (if (= (:tag (first zip-val)) :testsuites)
      (group-testcase-data (filter-tags (zip/down zip-val) :testcase))
      (group-testcase-data (filter-tags zip-val :testcase)))))

(defn get-files-data [files]
  (let [grouped-files (for [file files] (get-data file))]
    grouped-files))

(defn single-file-parse-n-merge-data [arg parsed-content]
  (let [grouped-map   (get-data arg)
        merge-content (for [x parsed-content]
                        (merge (get grouped-map (:name x)) x))]
    merge-content))

(defn parse-n-merge-data [grouped-files-map parsed-content]
  (let [merge-content (merge (get grouped-files-map (:name parsed-content)) parsed-content)]
    merge-content))

(defn send-directory [directory parsed-content]
  (let [filtered-files   (filter (fn [file] (str/ends-with? (.getAbsolutePath file) ".xml")) (file-seq directory))
        filtered-paths   (for [file filtered-files] (.getAbsolutePath file))
        files            (for [path filtered-paths] (slurp path))
        [grouped-data]     (get-files-data files)
        result           (for [parsed parsed-content] (parse-n-merge-data grouped-data parsed))]
    result))

(defn get-dir-by-path [path]
  (let [directory (clojure.java.io/file path)]
    (send-directory directory '("" "" ""))))

(defn -main [& [arg]]
  (if-not (empty? arg)
    (let [result (get-dir-by-path arg)]
      (pprint/pprint {"Result:" result}))
    (throw (Exception. "Must have at least one argument!"))))
