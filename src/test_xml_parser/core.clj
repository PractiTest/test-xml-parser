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

(defn -main [& [arg]]
  (if-not (empty? arg)
    (let [result (get-data arg)]
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
