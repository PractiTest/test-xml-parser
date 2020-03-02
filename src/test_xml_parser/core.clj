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
      (group-testcase-data (find-tags (zip/down zip-val) :testcase))
      (group-testcase-data (find-tags zip-val :testcase)))))

(defn parse-n-merge-data [arg parsed-content]
  (let [grouped-map   (get-data arg)
        merge-content (for [x parsed-content]
                        (merge (get grouped-map (:name x)) x))]
    (pprint/pprint merge-content)
    (println "grouped-map")
    (pprint/pprint grouped-map)
    merge-content))

(defn is-not-dir? [path]
  (let [file (clojure.java.io/file path)]
    (not (.isDirectory file))))

(defn send-directory [directory parsed-content]
  (let [file-paths       (for [file (file-seq directory)] (.getAbsolutePath file))
        [_ & files]      (for [file (file-seq directory)]
                           (when (is-not-dir? (.getAbsolutePath file)) (slurp (.getAbsolutePath file))))
        result           (doseq [i (range 0 (count files))]
                           ;; (do

                           (println "index " i)
                           (println "(count files) " (count files))
                           (println "(count parsed-content) " (count (first parsed-content)))
                             ;; (parse-n-merge-data (nth files i) (nth parsed-content i))
                             ;; (println "field: " (nth files i))
                             ;; (println "field: " (nth parsed-content i))
                             ;; )
                           )]
    (println "=== files ==")
    (pprint/pprint files)
    (println "=== parsed-content ==")
    (pprint/pprint parsed-content)
    ;; (println (count files))
    (println "=== Result ==")
    ;; (pprint/pprint files)
    (println result)
    ;; result
    ))

(defn get-dir-by-path [path]
  (let [directory (clojure.java.io/file path)]
    (send-directory directory '("" "" ""))))

(defn -main [& [arg]]
  (if-not (empty? arg)
    (let [result (get-dir-by-path arg)]
      ;; (doseq [arg args]
      ;;   ;; (pprint/pprint arg)
      ;;   (let [zip-val (zip-str arg)]
      ;;     ;; (pprint/pprint zip-val)
      ;;     ;; (pprint/pprint (:tag zip-val))
      ;;     ;; (println "=============")
      ;;     (if (= (:tag (first zip-val)) :testsuites)
      ;;       (merge result (zip/node (zip/right (zip/down (zip/down zip-val)))))
      ;;       (merge result (zip/node (zip/right (zip/down zip-val))))
      ;;       )
      ;;     ;; (pprint/pprint (group-testcase-by-name (find-testcase-tag (zip-str (slurp arg)))))
      ;;     ;; (println "=============")
      ;;     ;; (pprint/pprint (group-testcase-data (find-tags (zip-str arg) :testcase)))
      ;;     ;; (println "=============")
      ;;     ;; (pprint/pprint (find-tags (zip-str arg) :system-out))
      ;;     ;; (println "=============||==============")
      ;;     ))
      (println "Result:")
      (pprint/pprint result))
    (throw (Exception. "Must have at least one argument!"))))
