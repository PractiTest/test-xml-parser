(ns test-xml-parser.core
  (:require
   [clojure.xml :as xml]
   [clojure.zip :as zip]
   [clojure.pprint :as pprint]
   [clojure.string :as str])
  (:import [java.io File]))

(defn zip-str [s]
  (zip/xml-zip
   (xml/parse (java.io.ByteArrayInputStream. (.getBytes s)))))

(defn filter-tags [xml-content tag-key]
  (let [filter-result (filter #(= (:tag %) tag-key) xml-content)
        print   (pprint/pprint "filter-result")
        print   (pprint/pprint filter-result)
        print   (pprint/pprint "xml-content")
        print   (pprint/pprint tag-key)
        print   (pprint/pprint xml-content)
        ]
    filter-result))

(defn group-testcase-data [data]
  (->> data
       (group-by #(last (str/split (:classname (:attrs %)) #"\." )))
       (map (fn [[k vals]] [k (first (map :attrs vals))]))
       (into {})))

(defn get-data [arg]
  (let [zip-val (zip-str arg)
        print   (pprint/pprint "zip-val")
        print   (pprint/pprint zip-val)
        ]
    (if (= (:tag (first zip-val)) :testsuites)
      (group-testcase-data (filter-tags (:content (first (:content (first zip-val)))) :testcase))
      (group-testcase-data (filter-tags (:content (first zip-val)) :testcase)))))

(defn get-files-data [files]
  (let [grouped-files (for [file files] (get-data file))]
    grouped-files))

(defn single-file-parse-n-merge-data [arg parsed-content]
  (let [grouped-map   (get-data arg)
        print         (pprint/pprint "IN single-file-parse-n-merge-data")
        print         (pprint/pprint grouped-map)
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
