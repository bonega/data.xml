;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns ^{:doc "Tests for XML streaming behavior"
      :author "Herwig Hochleitner"}
  clojure.data.xml.test-streaming
  (:require [clojure.data.xml :refer :all]
            [clojure.test :refer :all])
  (:import java.lang.ref.WeakReference
           (java.io PipedInputStream PipedOutputStream
                    OutputStreamWriter)))

(defn gen-large-xml [n]
  (let [end-parsed (promise)]
    [(concat ["<root>"]
             (repeat n "<el/>")
             (lazy-seq
              (deliver end-parsed true)
              ["</root>"]))
     end-parsed]))

(defn start-large-parse [n]
  (let [out-stream (PipedOutputStream.)
        in-stream (PipedInputStream. out-stream 64)
        [xml-strs end-reached] (gen-large-xml n)
        delivery (future (doseq [s xml-strs]
                           (.write out-stream (.getBytes s "UTF-8"))))
        xml (parse in-stream)
        result [xml delivery end-reached (WeakReference. (first (:content xml)))]]
    (is (not (realized? end-reached)) "Too eager parsing")
    result))

(deftest test-no-head
  (let [n 4096
        [xml delivery end-reached ref] (start-large-parse n)
        needles (drop (- n 42) (:content xml))]
    (is (= :el (:tag (first needles))))
    (is (not (realized? end-reached)) "Still too eager parsing")
    (is (= n (count (:content xml))))
    (is (nil? @delivery))
    (is @end-reached)
    ;; This is incidentally also a test of locals clearing
    (System/gc)
    (is (nil? (.get ref))
        "GC didn't collect first child
  NOTICE this might also be because of a bug in locals clearing
         or another reason for missing effect of System/gc.")
    (is (= 42 (count needles)))))

(deftest test-retained-head
  (let [n 4096
        [xml delivery end-reached ref] (start-large-parse n)
        needles (drop (- n 42) (:content xml))]
    (is (= :el (:tag (first needles))))
    (is (not (realized? end-reached)) "Still too eager parsing")
    (is (= n (count (:content xml))))
    (is (nil? @delivery))
    (is @end-reached)
    (System/gc)
    (is (element? (.get ref)) "GC already collected first child,
  which is technically impossible, because we refer to its parent a line after this")
    (is (= :el (:tag (first (:content xml)))))
    (is (= 42 (count needles)))))
