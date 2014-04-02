;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns ^{:doc "Tests for namespaced XML"
      :author "Herwig Hochleitner"}
  clojure.data.xml.test-namespace
  (:use clojure.test
        clojure.data.xml
        clojure.data.xml.impl
        [clojure.data.xml.test-utils :only [test-stream lazy-parse*]]))

(def ns* (to-namespace {"D" "DAV:"}))
(def ns*default (to-namespace {"D" "DAV:" "" "data.xml:"}))

(deftest test-ns-ctx
  (is (= "DAV:" (uri-from-prefix ns* "D")))
  (is (nil? (uri-from-prefix ns* "XYZ")))
  (is (nil? (uri-from-prefix (assoc-prefix ns* "D" nil) "D")))
  (is (nil? (uri-from-prefix (assoc-prefix ns* "D" "") "D")))
  (is (= "DAV:" (prefix-from-uri (assoc-prefix ns* "E" "DAV:") "E")))
  (is (= "D" (prefix-from-uri ns* "DAV:")))
  (is (nil? (prefix-from-uri ns* "XYZ:")))
  (is (nil? (prefix-from-uri (assoc-prefix ns* "D" nil) "DAV:")))
  (is (nil? (prefix-from-uri (assoc-prefix ns* "D" "") "DAV:")))
  (is (= "D" (prefix-from-uri (assoc-prefix ns* "E" "DAV:") "DAV:"))))

(deftest test-name-resolution
  (testing "Name resolution without namespace context"
    (are [name res] (= (name-info name) res)
         :foo {:uri nil :prefix nil :name "foo"}
         :D/foo {:uri nil :prefix "D" :name "foo"}
         "foo" {:uri nil :prefix nil :name "foo"}
         "D/foo" {:uri nil :prefix "D" :name "foo"}))
  (testing "Name resolution with namespace context"
    (are [name res] (= (name-info name ns*) res)
         :foo {:uri "" :prefix "" :name "foo"}
         :D/foo {:uri "DAV:" :prefix "D" :name "foo"}
         "foo" {:uri "" :prefix "" :name "foo"}
         "D/foo" {:uri "DAV:" :prefix "D" :name "foo"})
    (are [name res] (= (name-info name ns*default) res)
         :foo {:uri "data.xml:" :prefix "" :name "foo"}
         :D/foo {:uri "DAV:" :prefix "D" :name "foo"}
         "foo" {:uri "data.xml:" :prefix "" :name "foo"}
         "D/foo" {:uri "DAV:" :prefix "D" :name "foo"}))
  (testing "Name resolution error cases"
    (is (thrown? Exception (name-info :XYZ/foo ns*))
        "Testing unknown prefix with no default namespace")
    (is (thrown? Exception (name-info :XYZ/foo ns*default))
        "Testing unknown prefix with a default namespace")))

#_(deftest test-tolerance
  (is (= ""
         (emit-str
          #clojure.data.xml.Element
          {:tag #xml/name{:name "foo", :uri "DAV:"}
           :attrs {#xml/name{:name "xmlns", :uri "DAV:"} "DAV:"}
           :content ("bar")}))))

