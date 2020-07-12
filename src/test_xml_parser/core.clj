(ns test-xml-parser.core
  (:require
   [clojure.xml     :as xml]
   [clojure.zip     :as zip]
   [clojure.pprint  :as pprint]
   [clojure.string  :as str]
   [clojure.java.io :as io]
   )
  (:import [java.io File]))

(defn zip-str [s]
  (zip/xml-zip
   (xml/parse (java.io.ByteArrayInputStream. (.getBytes s)))))

(defn filter-tags [xml-content tag-key]
  (let [filter-result (filter #(= (:tag %) tag-key) xml-content)]
    filter-result))

(defn group-testcase-data [data]
  (->> data
       (group-by #(last (str/split (:classname (:attrs %)) #"\." )))
       (map (fn [[k vals]] [k (first (map :attrs vals))]))
       (into {})))

(defn debomify
  [^String line]
  (let [bom "\uFEFF"]
    (if (.startsWith line bom)
      (.substring line 1)
      line)))

(defn file-bom [file]
  (let [domless-file   (debomify (slurp file))
        full-path      (str/split file #"/")
        directory      (str/join "/" (butlast full-path))
        filename       (last full-path)
        new-path       (str directory "/tmp/" filename)]
    (io/delete-file (str directory "/tmp"))
    (.mkdir (java.io.File. (str directory "/tmp")))
    (spit new-path domless-file)))

(defn get-data [arg]
  (let [zip-val (zip-str arg)]
    (if (= (:tag (first zip-val)) :testsuites)
      (group-testcase-data (filter-tags (:content (first (:content (first zip-val)))) :testcase))
      (group-testcase-data (filter-tags (:content (first zip-val)) :testcase)))))

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
  (let [file-seqs        (file-seq (io/file directory))
        filtered-files   (filter (fn [file] (str/ends-with? (.getAbsolutePath file) ".xml")) file-seqs)
        filtered-paths   (for [file filtered-files] (.getAbsolutePath file))
        files            (for [path filtered-paths] (slurp path))
        [grouped-data]     (get-files-data files)
        result           (for [parsed parsed-content] (parse-n-merge-data grouped-data parsed))]
    result))

(defn remove-bom [directory]
  (let [file-seqs        (file-seq (io/file directory))
        filtered-files   (filter (fn [file] (str/ends-with? (.getAbsolutePath file) ".xml")) file-seqs)
        filtered-paths   (for [file filtered-files] (.getAbsolutePath file))
        files            (doall (map file-bom filtered-paths))]
    files))

(defn return-file [file]
  (pprint/pprint (slurp file)))

(defn return-files [directory]
  (let [filtered-files   (filter (fn [file] (str/ends-with? (.getAbsolutePath file) ".xml")) (file-seq directory))
        filtered-paths   (for [file filtered-files] (.getAbsolutePath file))
        files            (for [path filtered-paths] (return-file path))]
    files))

(defn get-dir-by-path [path]
  (let [directory (clojure.java.io/file path)]
    (send-directory directory '("" "" ""))))

(defn -main [& [arg]]
  (if-not (empty? arg)
    (let [result (get-dir-by-path arg)]
      (pprint/pprint {"Result:" result}))
    (throw (Exception. "Must have at least one argument!"))))
