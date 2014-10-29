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
  (let [in-stream (PipedInputStream. 64)
        out-stream (PipedOutputStream. in-stream)
        [xml-strs end-reached] (gen-large-xml n)
        delivery (future (with-open [out-stream out-stream]
                           (doseq [s xml-strs]
                             (.write out-stream (.getBytes s "UTF-8")))))
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

(defn read-full [is ba i]
  (let [c (.read is ba i (- (alength ba) i))]
    (if (< (+ i c) (alength ba))
      (recur is ba (+ i c))
      ba)))

(deftest end-to-end
  ;; Test that the star of a document can already be emitted while it
  ;; isn't even fully parsed yet. Also while releasing head.
  (let [n 4096
        [xml delivery end-reached ref] (start-large-parse n)
        in-stream (PipedInputStream. 64)
        out-stream (PipedOutputStream. in-stream)
        writing (future (try (with-open [out-stream out-stream]
                               (emit xml (OutputStreamWriter. out-stream)))
                             (catch Exception e (.printStackTrace e))))
        bs (byte-array 125)]
    (read-full in-stream bs 0)
    (is (= "<?xml version=\"1.0\" encoding=\"UTF-8\"?><root><el></el><el></el><el></el><el></el><el></el><el></el><el></el><el></el><el></el>"
           (String. bs "UTF-8")))
    (System/gc)
    (is (nil? (.get ref))
        "GC didn't collect first child
  NOTICE this might also be because of a bug in locals clearing
         or another reason for missing effect of System/gc.")
    (is (not (realized? end-reached)))))
