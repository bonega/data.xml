(ns clojure.data.xml.emit
  (:require (clojure.data.xml event node)
            [clojure.data.xml.impl :refer [attr-info parse-attrs str-empty?
                                           tag-info uri-from-prefix]])
  (:import (clojure.data.xml.event Event)
           (clojure.data.xml.node CData Comment Element)
           (clojure.lang APersistentMap)
           (java.io OutputStreamWriter)
           (java.nio.charset Charset)
           (javax.xml.stream XMLStreamWriter)
           (javax.xml.transform OutputKeys Transformer
                                TransformerFactory)))

(defn write-attributes [attrs ^XMLStreamWriter writer]
  (doseq [[k v] attrs]
    (let [ns-ctx (.getNamespaceContext writer)
          {:keys [prefix uri name] :as i} (attr-info k ns-ctx)]
      (when (and (empty? prefix) (not (empty? uri)))
        (throw (ex-info (str "No prefix for attribute URI: " uri)
                        {:default-uri (uri-from-prefix ns-ctx "")
                         :name k
                         :info i})))
      (if (empty? prefix)
        (.writeAttribute writer name v)
        (.writeAttribute writer prefix uri name v)))))

(defn write-ns-attributes [default attrs ^XMLStreamWriter writer]
  (when default
    (.setDefaultNamespace writer default)
    (.writeDefaultNamespace writer default))
  (doseq [[k v] attrs]
    (.setPrefix writer k v)
    (.writeNamespace writer k v)))

(defn emit-start-tag [event ^XMLStreamWriter writer]
  (let [{:keys [nss uris attrs default] :as parse} (parse-attrs (:attrs event))
        ns-ctx (.getNamespaceContext writer)
        {:keys [uri name prefix] :as i} (tag-info (:name event) ns-ctx parse)]
    (when (and (empty? prefix) (not (empty? uri))
               (not= uri default)
               (not= uri (uri-from-prefix ns-ctx "")))
      (throw (ex-info (str "No prefix for URI: " uri)
                      {:default-uri (uri-from-prefix ns-ctx "")
                       :name (:name event)
                       :info i})))
    (.writeStartElement writer prefix name uri)
    (write-ns-attributes default nss writer)
    (write-attributes attrs writer)))

(defn emit-cdata [^String cdata-str ^XMLStreamWriter writer]
  (when-not (str-empty? cdata-str)
    (let [idx (.indexOf cdata-str "]]>")]
      (if (= idx -1)
        (.writeCData writer cdata-str )
        (do
          (.writeCData writer (subs cdata-str 0 (+ idx 2)))
          (recur (subs cdata-str (+ idx 2)) writer))))))

(defn emit-event [event ^XMLStreamWriter writer]
  (case (:type event)
    :start-element (emit-start-tag event writer)
    :end-element (.writeEndElement writer)
    :chars (.writeCharacters writer (:str event))
    :cdata (emit-cdata (:str event) writer)
    :comment (.writeComment writer (:str event))))

(defprotocol EventGeneration
  "Protocol for generating new events based on element type"
  (gen-event [item]
    "Function to generate an event for e.")
  (next-events [item next-items]
    "Returns the next set of events that should occur after e.  next-events are the
     events that should be generated after this one is complete."))

;; Same implementation for Element defrecords and plain maps
(let [impl-map {:gen-event (fn gen-event [element]
                             (Event. :start-element (:tag element) (:attrs element) nil))
                :next-events (fn next-events [element next-items]
                               (cons (:content element)
                                     (cons (Event. :end-element (:tag element) nil nil) next-items)))}]
  (extend Element EventGeneration impl-map)
  (extend APersistentMap EventGeneration impl-map))

(extend-protocol EventGeneration

  Event
  (gen-event [event] event)
  (next-events [_ next-items]
    next-items)

  clojure.lang.Sequential
  (gen-event [coll]
    (gen-event (first coll)))
  (next-events [coll next-items]
    (if-let [r (seq (rest coll))]
      (cons (next-events (first coll) r) next-items)
      (next-events (first coll) next-items)))

  String
  (gen-event [s]
    (Event. :chars nil nil s))
  (next-events [_ next-items]
    next-items)

  Boolean
  (gen-event [b]
    (Event. :chars nil nil (str b)))
  (next-events [_ next-items]
    next-items)

  Number
  (gen-event [b]
    (Event. :chars nil nil (str b)))
  (next-events [_ next-items]
    next-items)

  CData
  (gen-event [cdata]
    (Event. :cdata nil nil (:content cdata)))
  (next-events [_ next-items]
    next-items)

  Comment
  (gen-event [comment]
    (Event. :comment nil nil (:content comment)))
  (next-events [_ next-items]
    next-items)

  nil
  (gen-event [_]
    (Event. :chars nil nil ""))
  (next-events [_ next-items]
    next-items))

(defn flatten-elements [elements]
  (when (seq elements)
    (lazy-seq
     (let [e (first elements)]
       (cons (gen-event e)
             (flatten-elements (next-events e (rest elements))))))))

(defn check-stream-encoding [^OutputStreamWriter stream xml-encoding]
  (when (not= (Charset/forName xml-encoding) (Charset/forName (.getEncoding stream)))
    (throw (Exception. (str "Output encoding of stream (" xml-encoding
                            ") doesn't match declaration ("
                            (.getEncoding stream) ")")))))

(defn ^Transformer indenting-transformer []
  (doto (-> (TransformerFactory/newInstance) .newTransformer)
    (.setOutputProperty (OutputKeys/INDENT) "yes")
    (.setOutputProperty (OutputKeys/METHOD) "xml")
    (.setOutputProperty "{http://xml.apache.org/xslt}indent-amount" "2")))
