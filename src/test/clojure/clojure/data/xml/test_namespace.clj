;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns clojure.data.xml.test-namespace
  "Tests for namespaced XML"
  {:author "Herwig Hochleitner"}
  (:require [clojure.data.xml.impl :refer :all]
            [clojure.data.xml.impl.xmlns :refer :all]
            [clojure.data.xml :refer [defns]]
            [clojure.data.xml.test-utils :refer [run-in-ns]]
            [clojure.string :as str]
            [clojure.test :refer :all]))

(run-in-ns 'test.ns #(eval `(defns :D "DAV:")))
(run-in-ns 'test.ns-default #(eval `(defns "data.xml:"
                                      :D "DAV:")))

(deftest test-name-resolution
  (testing "Global keyword resolution to qnames"
    (are [name res] (= (qname name) (qname res))
         :foo "foo"
         :test.ns/D:foo "{DAV:}foo"))
  (testing "Keyword resolution error cases"
    (is (thrown? Exception (qname :XYZ/foo))
        "Testing unknown prefix with no default namespace")
    (is (thrown? Exception (qname :test.ns/XYZ:foo))
        "Testing unknown prefix with a default namespace")))

;; NamespaceContextImpl unit tests

(defn- get-xmlns []
  (get @clj-ns-xmlns (name (ns-name *ns*))))

(def ns* (run-in-ns 'test.ns get-xmlns))
(def ns*default (run-in-ns 'test.ns-default get-xmlns))


(deftest test-ns-ctx
  (is (= "" (uri-from-prefix ns* "")))
  (is (= "data.xml:" (uri-from-prefix ns*default "")))
  (is (= "DAV:" (uri-from-prefix ns* "D")))
  (is (= "DAV:" (get ns* "D")))
  (is (thrown? Exception (uri-from-prefix ns* "XYZ")))
  (is (thrown? Exception (uri-from-prefix (assoc ns* "D" nil) "D")))
  (is (thrown? Exception (uri-from-prefix (assoc ns* "D" "") "D")))
  (is (thrown? Exception (get (assoc ns* "D" "") "D")))
  (is (thrown? Exception (get ns* "XYZ")))
  (is (= :none (get (assoc ns* "D" "") "D" :none)))
  (is (= "D" (prefix-from-uri ns* "DAV:")))
  (is (= "D" (prefix-from-uri (assoc ns* "E" "DAV:") "DAV:")))
  (is (= "" (prefix-from-uri ns*default "data.xml:")))
  (is (thrown? Exception (prefix-from-uri ns* "XYZ:")))
  (is (thrown? Exception (prefix-from-uri (assoc ns* "D" nil) "DAV:")))
  (is (thrown? Exception (prefix-from-uri (assoc ns* "D" "") "DAV:")))
  (is (= "D" (prefix-from-uri (assoc ns* "E" "DAV:") "DAV:")))
  (testing "Alternate prefixes"
    (let [ns* (assoc ns*
                "E" "DAV:"
                "F" "DAV:")]
      (is (= "D" (prefix-from-uri (assoc ns* "E" nil) "DAV:")))
      (is (= "DAV:" (uri-from-prefix (assoc ns* "E" nil) "D")))
      (is (= "D" (prefix-from-uri (assoc ns* "XY" nil) "DAV:")))
      (is (= "DAV:" (uri-from-prefix (assoc ns* "XY" nil) "D")))
      (is (thrown? Exception (uri-from-prefix (assoc ns* "E" nil) "E")))
      (is (= "E" (prefix-from-uri (assoc ns* "D" nil) "DAV:")))
      (is (= "DAV:" (uri-from-prefix (assoc ns* "D" nil) "E")))
      (is (= "F" (prefix-from-uri (assoc ns* "E" nil "D" nil) "DAV:")))
      (is (= "DAV:" (uri-from-prefix (assoc ns* "E" nil "D" nil) "F")))
      (is (thrown? Exception (uri-from-prefix (assoc ns* "E" "OTHER:" "E" nil) "E")))
      (is (= "F" (prefix-from-uri (assoc ns* "E" "OTHER:" "D" nil) "DAV:")))

      (is (= {"xml" "http://www.w3.org/XML/1998/namespace",
              "xmlns" "http://www.w3.org/2000/xmlns/",
              "D" "DAV:"}
             (prefix-bindings ns*)))

      (is (= {"xml" "http://www.w3.org/XML/1998/namespace",
              "xmlns" "http://www.w3.org/2000/xmlns/",
              "E" "DAV:"}
             (prefix-bindings (dissoc ns* "D")))))))

