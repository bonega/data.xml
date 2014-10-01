(ns clojure.data.xml.test-roundtrip
  "Tests for namespaced XML"
  {:author "Herwig Hochleitner"}
  (:require [clojure.data.xml :refer :all]
            [clojure.test :refer :all]))

(def test-xml
  (str "<a xmlns=\"DU:\" xmlns:p=\"PU:\">" \newline
       "  <b ba=\"baval\">Some b text</b>" \newline
       "  <p:c ca=\"caval\" p:cpa=\"cpaval\" />" \newline
       "</a>"))

(defns "DU:" :p "PU:")

(deftest raw-roundtrip
  (is (= (parse-str* nil test-xml)
         (parse-str* nil (emit-str-raw (parse-str-raw test-xml))))))

(deftest resolved-roundtrip
  (testing "basic"
    (is (= (parse-str* nil test-xml)
           (parse-str* nil (emit-str (parse-str* nil test-xml))))))
  (testing "changing prefix"
    (let [raw (parse-str-raw test-xml)
          res (parse-str* nil test-xml)
          ress (emit-str (assoc-in res [:attrs :xmlns/q] "PU:"))]
      (is (not= raw (parse-str-raw ress)))
      (is (= res (parse-str* nil ress)))))
  (testing "namespace-local custom keywords"
    (is (= (parse-str-raw test-xml)
           (parse-str-raw (emit-str (parse-str test-xml)))))
    ;; unfortunately, we can't make this happen with builtin =
    (is (not= (parse-str* nil test-xml)
              (parse-str test-xml)))
    (is (= (element ::a)
           (parse-str "<a xmlns=\"DU:\"/>")))
    (is (= (element ::p:a)
           (parse-str "<a xmlns=\"PU:\"/>")))
    (is (= (element (xml-name "{TU:}a"))
           (parse-str "<a xmlns=\"TU:\"/>")))))
