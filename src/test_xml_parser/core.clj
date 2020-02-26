(ns test-xml-parser.core
  (:require
   [clojure.xml :as xml]
   [clojure.zip :as zip]
   [clojure.pprint :as pprint]
   [clojure.string :as str]))

(defn zip-str [s]
  (zip/xml-zip
   (xml/parse (java.io.ByteArrayInputStream. (.getBytes s)))))

(defn find-testcase-tag [xml-content]
  (let [filter-result (filter #(= (:tag %) :testcase) xml-content)]
    (if (empty? filter-result)
      (if (contains? (first xml-content) :content)
        (find-testcase-tag (:content (first xml-content)))
        xml-content)
      filter-result)))

(defn -main [& args]
  (print "IN Main"))
