;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns ^{:doc "Functions to parse XML into lazy sequences and lazy trees and
  emit these as text."
  :author "Chris Houser"}
  clojure.data.xml
  (:require [clojure.string :as str]
            [clojure.walk :refer [postwalk]])
  (:import (javax.xml.stream XMLInputFactory
                             XMLStreamReader
                             XMLStreamConstants)
           (java.nio.charset Charset)
           (java.io Reader)))

(def xmlns-uri "http://www.w3.org/2000/xmlns/")

(defrecord XmlName [name uri])
(defn xml-name
  ([name] (XmlName. name nil))
  ([name uri] (XmlName. name uri)))

; Represents a parse event.
; type is one of :start-element, :end-element, or :characters
(defrecord Event [type name attrs str])

(defn event [type name & [attrs str]]
  (Event. type name attrs str))

(defn qualified-name [event-name]
  (cond
   (instance? clojure.lang.Named event-name)
   {:prefix (namespace event-name)
    :name (name event-name)}

   (instance? clojure.data.xml.XmlName event-name)
   event-name

   :else (let [i (.lastIndexOf ^String event-name "/")]
           (if (neg? i)
             {:name event-name}
             {:prefix (subs event-name 0 i)
              :name (subs event-name (inc i))}))))

(defn- write-attributes [attrs ^javax.xml.stream.XMLStreamWriter writer]
  (doseq [[k v] attrs]
    (let [{attr-ns :prefix attr-name :name attr-uri :uri} (qualified-name k)
          prefix (or attr-ns  (.getPrefix writer attr-uri))]
      (if (empty? prefix)
        (.writeAttribute writer attr-name v)
        (.writeAttribute writer prefix
                         (or attr-uri
                             (.getNamespaceURI (.getNamespaceContext writer)
                                               attr-ns)
                             "")
                         attr-name v)))))

(defn- write-ns-attributes [default attrs ^javax.xml.stream.XMLStreamWriter writer]
  (when default
    (.setDefaultNamespace writer default)
    (.writeDefaultNamespace writer default))
  (doseq [[k v] attrs]
    (.setPrefix writer k v)
    (.writeNamespace writer k v)))

(defn- parse-attrs [attrs]
  (when attrs
    (reduce-kv (fn [res k v*]
                 (let [{kns :prefix kv :name kuri :uri} (qualified-name k)
                       v (str v*)]
                   (cond
                    (and (nil? kns) (= "xmlns" kv))
                    (assoc res :default v)

                    (or (= xmlns-uri kuri)
                        (= "xmlns" kns))
                    (-> res
                        (assoc-in [:nss kv] v)
                        (assoc-in [:uris v] kv))
                  
                    :else
                    (assoc-in res [:attrs k] v))))
               {:default nil
                :attrs {}
                :uris {}
                :nss {}}
               attrs)))

; Represents a node of an XML tree
(defrecord Element [tag attrs content])
(defrecord CData [content])
(defrecord Comment [content])

(defn emit-start-tag [event ^javax.xml.stream.XMLStreamWriter writer]
  (let [{:keys [nss uris attrs default]} (parse-attrs (:attrs event))
        {nspace :prefix qname :name nsuri :uri} (qualified-name (:name event))]
    (if (and (nil? nspace)
             (nil? nsuri))
      (.writeStartElement writer qname)
      (.writeStartElement writer
                          (or nspace
                              (get uris nsuri)
                              (.getPrefix writer nsuri)

                              ;; due to the glaring lack of (.getDefaultNamespace writer)
                              ;; we can only assume here that nsuri matches the
                              ;; currently set default namespace
                              ;; + we can only get rid of prefixes that happen to point
                              ;; to the default ns, by preprocessing XmlNames to keywords
                              ;; in the tree

                              ;; FIXME: this information could be threaded along event
                              "")
                          qname
                          (or nsuri
                              (get nss nspace)
                              (.getNamespaceURI (.getNamespaceContext writer)
                                                nspace)
                              "")))
    (write-ns-attributes default nss writer)
    (write-attributes attrs writer)))

(defn str-empty? [s]
  (or (nil? s)
      (= s "")))

(defn emit-cdata [^String cdata-str ^javax.xml.stream.XMLStreamWriter writer]
  (when-not (str-empty? cdata-str) 
    (let [idx (.indexOf cdata-str "]]>")]
      (if (= idx -1)
        (.writeCData writer cdata-str )
        (do
          (.writeCData writer (subs cdata-str 0 (+ idx 2)))
          (recur (subs cdata-str (+ idx 2)) writer))))))

(defn emit-event [event ^javax.xml.stream.XMLStreamWriter writer]
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
  (extend clojure.lang.IPersistentMap EventGeneration impl-map))

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

(defn element [tag & [attrs & content]]
  (Element. tag (or attrs {}) (remove nil? content)))

(defn cdata [content]
  (CData. content))

(defn xml-comment [content]
  (Comment. content))

;=== Parse-related functions ===
(defn seq-tree
  "Takes a seq of events that logically represents
  a tree by each event being one of: enter-sub-tree event,
  exit-sub-tree event, or node event.

  Returns a lazy sequence whose first element is a sequence of
  sub-trees and whose remaining elements are events that are not
  siblings or descendants of the initial event.

  The given exit? function must return true for any exit-sub-tree
  event.  parent must be a function of two arguments: the first is an
  event, the second a sequence of nodes or subtrees that are children
  of the event.  parent must return nil or false if the event is not
  an enter-sub-tree event.  Any other return value will become
  a sub-tree of the output tree and should normally contain in some
  way the children passed as the second arg.  The node function is
  called with a single event arg on every event that is neither parent
  nor exit, and its return value will become a node of the output tree.

  (seq-tree #(when (= %1 :<) (vector %2)) #{:>} str
            [1 2 :< 3 :< 4 :> :> 5 :> 6])
  ;=> ((\"1\" \"2\" [(\"3\" [(\"4\")])] \"5\") 6)"
 [parent exit? node coll]
  (lazy-seq
    (when-let [[event] (seq coll)]
      (let [more (rest coll)]
        (if (exit? event)
          (cons nil more)
          (let [tree (seq-tree parent exit? node more)]
            (if-let [p (parent event (lazy-seq (first tree)))]
              (let [subtree (seq-tree parent exit? node (lazy-seq (rest tree)))]
                (cons (cons p (lazy-seq (first subtree)))
                      (lazy-seq (rest subtree))))
              (cons (cons (node event) (lazy-seq (first tree)))
                    (lazy-seq (rest tree))))))))))

(defn event-tree
  "Returns a lazy tree of Element objects for the given seq of Event
  objects. See source-seq and parse."
  [events]
  (ffirst
   (seq-tree
    (fn [^Event event contents]
      (when (= :start-element (.type event))
        (Element. (.name event) (.attrs event) contents)))
    (fn [^Event event] (= :end-element (.type event)))
    (fn [^Event event] (.str event))
    events)))

(defprotocol AsElements
  (as-elements [expr] "Return a seq of elements represented by an expression."))

(defn sexp-element [tag attrs child]
  (cond
   (= :-cdata tag) (CData. (first child))
   (= :-comment tag) (Comment. (first child))
   :else (Element. tag attrs (mapcat as-elements child))))

(extend-protocol AsElements
  clojure.lang.IPersistentVector
  (as-elements [v]
    (let [[tag & [attrs & after-attrs :as content]] v
          [attrs content] (if (map? attrs)
                            [(into {} (for [[k v] attrs]
                                        [k (str v)]))
                             after-attrs]
                            [{} content])]
      [(sexp-element tag attrs content)]))

  clojure.lang.ISeq
  (as-elements [s]
    (mapcat as-elements s))

  clojure.lang.Keyword
  (as-elements [k]
    [(Element. k {} ())])

  java.lang.String
  (as-elements [s]
    [s])

  nil
  (as-elements [_] nil)

  java.lang.Object
  (as-elements [o]
    [(str o)]))

(defn sexps-as-fragment
  "Convert a compact prxml/hiccup-style data structure into the more formal
   tag/attrs/content format. A seq of elements will be returned, which may
   not be suitable for immediate use as there is no root element. See also
   sexp-as-element.

   The format is [:tag-name attr-map? content*]. Each vector opens a new tag;
   seqs do not open new tags, and are just used for inserting groups of elements
   into the parent tag. A bare keyword not in a vector creates an empty element.

   To provide XML conversion for your own data types, extend the AsElements
   protocol to them."
  ([] nil)
  ([sexp] (as-elements sexp))
  ([sexp & sexps] (mapcat as-elements (cons sexp sexps))))

(defn sexp-as-element
  "Convert a single sexp into an Element"
  [sexp]
  (let [[root & more] (sexps-as-fragment sexp)]
    (when more
      (throw
       (IllegalArgumentException.
        "Cannot have multiple root elements; try creating a fragment instead")))
    root))


(defn- attr-prefix [^XMLStreamReader sreader index]
  (let [p (.getAttributePrefix sreader index)]
    (when-not (str/blank? p)
      p)))

(defn- attr-hash [^XMLStreamReader sreader] (into {}
    (for [i (range (.getAttributeCount sreader))]
      [(keyword (attr-prefix sreader i) (.getAttributeLocalName sreader i))
       (.getAttributeValue sreader i)])))

(defn- attr-hash [^XMLStreamReader sreader] (into {}
    (concat
     (for [i (range (.getAttributeCount sreader))]
      [(keyword (attr-prefix sreader i) (.getAttributeLocalName sreader i))
       (.getAttributeValue sreader i)])
     (for [i (range (.getNamespaceCount sreader))
           :let [prefix (.getNamespacePrefix sreader i)]]
       [(if prefix
          (keyword "xmlns" prefix)
          :xmlns)
        (.getNamespaceURI sreader i)]))))

(defn- xml-tag [^XMLStreamReader sreader]
  (let [prefix (.getPrefix sreader)
        name (.getLocalName sreader)]
    (if (str/blank? prefix)
      (keyword name)
      (keyword prefix name))))

; Note, sreader is mutable and mutated here in pull-seq, but it's
; protected by a lazy-seq so it's thread-safe.
(defn- pull-seq
  "Creates a seq of events.  The XMLStreamConstants/SPACE clause below doesn't seem to 
   be triggered by the JDK StAX parser, but is by others.  Leaving in to be more complete."
  [^XMLStreamReader sreader]
  (lazy-seq
   (loop []
     (condp == (.next sreader)
       XMLStreamConstants/START_ELEMENT
       (cons (event :start-element
                    (xml-tag sreader)
                    (attr-hash sreader) nil)
             (pull-seq sreader)) 
       XMLStreamConstants/END_ELEMENT
       (cons (event :end-element
                    (xml-tag sreader)
                    nil nil)
             (pull-seq sreader))
       XMLStreamConstants/CHARACTERS
       (if-let [text (and (not (.isWhiteSpace sreader))
                          (.getText sreader))]
         (cons (event :characters nil nil text)
               (pull-seq sreader))
         (recur))
       XMLStreamConstants/END_DOCUMENT
       nil
       (recur);; Consume and ignore comments, spaces, processing instructions etc
       ))))

(def ^{:private true} xml-input-factory-props
  {:allocator javax.xml.stream.XMLInputFactory/ALLOCATOR
   :coalescing javax.xml.stream.XMLInputFactory/IS_COALESCING
   :namespace-aware javax.xml.stream.XMLInputFactory/IS_NAMESPACE_AWARE
   :replacing-entity-references javax.xml.stream.XMLInputFactory/IS_REPLACING_ENTITY_REFERENCES
   :supporting-external-entities javax.xml.stream.XMLInputFactory/IS_SUPPORTING_EXTERNAL_ENTITIES
   :validating javax.xml.stream.XMLInputFactory/IS_VALIDATING
   :reporter javax.xml.stream.XMLInputFactory/REPORTER
   :resolver javax.xml.stream.XMLInputFactory/RESOLVER
   :support-dtd javax.xml.stream.XMLInputFactory/SUPPORT_DTD})

(defn- new-xml-input-factory [props]
  (let [fac (javax.xml.stream.XMLInputFactory/newInstance)]
    (doseq [[k v] props
            :let [prop (xml-input-factory-props k)]]
      (.setProperty fac prop v))
    fac))

(defn source-seq
  "Parses the XML InputSource source using a pull-parser. Returns
   a lazy sequence of Event records.  Accepts key pairs
   with XMLInputFactory options, see http://docs.oracle.com/javase/6/docs/api/javax/xml/stream/XMLInputFactory.html
   and xml-input-factory-props for more information. Defaults coalescing true."
  [s & {:as props}]
  (let [fac (new-xml-input-factory (merge {:coalescing true} props))
        ;; Reflection on following line cannot be eliminated via a
        ;; type hint, because s is advertised by fn parse to be an
        ;; InputStream or Reader, and there are different
        ;; createXMLStreamReader signatures for each of those types.
        sreader (.createXMLStreamReader fac s)]
    (pull-seq sreader)))

(defn parse
  "Parses the source, which can be an
   InputStream or Reader, and returns a lazy tree of Element records. Accepts key pairs
   with XMLInputFactory options, see http://docs.oracle.com/javase/6/docs/api/javax/xml/stream/XMLInputFactory.html
   and xml-input-factory-props for more information. Defaults coalescing true."
  [source & props]
  (event-tree (apply source-seq source props)))

(defn parse-str
  "Parses the passed in string to Clojure data structures.  Accepts key pairs
   with XMLInputFactory options, see http://docs.oracle.com/javase/6/docs/api/javax/xml/stream/XMLInputFactory.html
   and xml-input-factory-props for more information. Defaults coalescing true."
  [s & props]
  (let [sr (java.io.StringReader. s)]
    (apply parse sr props)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; XML Emitting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn check-stream-encoding [^java.io.OutputStreamWriter stream xml-encoding]
  (when (not= (Charset/forName xml-encoding) (Charset/forName (.getEncoding stream)))
    (throw (Exception. (str "Output encoding of stream (" xml-encoding
                            ") doesn't match declaration ("
                            (.getEncoding stream) ")")))))

(defn emit
  "Prints the given Element tree as XML text to stream.
   Options:
    :encoding <str>          Character encoding to use"
  [e ^java.io.Writer stream & {:as opts}]
  (let [^javax.xml.stream.XMLStreamWriter writer (-> (javax.xml.stream.XMLOutputFactory/newInstance)
                                                     (.createXMLStreamWriter stream))]

    (when (instance? java.io.OutputStreamWriter stream)
      (check-stream-encoding stream (or (:encoding opts) "UTF-8")))
    
    (.writeStartDocument writer (or (:encoding opts) "UTF-8") "1.0")
    (doseq [event (flatten-elements [e])]
      (emit-event event writer))
    (.writeEndDocument writer)
    stream))

(defn emit-str
  "Emits the Element to String and returns it"
  [e]
  (let [^java.io.StringWriter sw (java.io.StringWriter.)]
    (emit e sw)
    (.toString sw)))

(defn ^javax.xml.transform.Transformer indenting-transformer []
  (doto (-> (javax.xml.transform.TransformerFactory/newInstance) .newTransformer)
    (.setOutputProperty (javax.xml.transform.OutputKeys/INDENT) "yes")
    (.setOutputProperty (javax.xml.transform.OutputKeys/METHOD) "xml")
    (.setOutputProperty "{http://xml.apache.org/xslt}indent-amount" "2")))

(defn indent
  "Emits the XML and indents the result.  WARNING: this is slow
   it will emit the XML and read it in again to indent it.  Intended for 
   debugging/testing only."
  [e ^java.io.Writer stream & {:as opts}]
  (let [sw (java.io.StringWriter.)
        _ (apply emit e sw (apply concat opts))
        source (-> sw .toString java.io.StringReader. javax.xml.transform.stream.StreamSource.)
        result (javax.xml.transform.stream.StreamResult. stream)]
    (.transform (indenting-transformer) source result)))

(defn indent-str
  "Emits the XML and indents the result.  Writes the results to a String and returns it"
  [e]
  (let [^java.io.StringWriter sw (java.io.StringWriter.)]
    (indent e sw)
    (.toString sw)))

(defn resolve-name
  "Resolve a prefixed xml name within namespace environment.
   - `default-uri` corresponds to an innermost xmlns= attribute
   - `prefixes` is a string map of {prefix uri ...}, corresponding
     to all active xmlns:prefix= attrs"
  [name default-uri prefixes]
  (if (instance? clojure.data.xml.XmlName name)
    name
    (let [{nspace :prefix qname :name} (qualified-name name)]
      (xml-name qname (if nspace
                        (or (get prefixes nspace)
                            (throw (ex-info (str "Prefix couldn't be resolved: " nspace)
                                            {:name name :prefixes prefixes})))
                        default-uri)))))

(defmacro with-xmlns
  "Lexically replace keywords, whose namespace appears in prefixes,
   with XmlNames, constructed from the keyword name and uri found in prefixes."
  [prefixes form]
  {:pre [(map? prefixes) (every? string? (keys prefixes))]}
  (postwalk (fn [form]
              (if-let [uri (and (keyword? form)
                                (get prefixes (namespace form)))]
                `(xml-name ~(name form) ~uri)
                form))
            form))

(defn walk-resolve-names
  "Transforms an xml tree, so that every tag and attribute name are resolved to an
   #XmlName[:name :uri]. This uses xmlns* attributes to generate the XmlNames"
  ([xml] (walk-resolve-names xml nil {"xmlns" xmlns-uri}))
  ([xml default-uri prefixes]
     (let [{:keys [tag attrs content]} xml
           {:keys [nss default]} (parse-attrs attrs)
           default-uri* (or default default-uri)
           prefixes* (merge prefixes nss)]
       (cond
        tag (apply element (resolve-name tag default-uri* prefixes*)
                   (into {} (map #(vector (resolve-name %1 default-uri* prefixes*) %2)
                                 (keys attrs) (vals attrs)))
                   (map #(resolve-walk % default-uri* prefixes*)
                        content))
        content (assoc xml :content (map #(resolve-walk % default-uri* prefixes*)
                                         content))
        :else xml))))

(defn walk-cleanup-prefixes
  "Removes redundant prefixes"
  ([xml] (walk-cleanup-prefixes xml nil {xmlns-uri "xmlns"}))
  ([xml default-uri uri-prefixes]
     (let [{:keys [tag attrs content]} xml
           {:keys [nss default] only-attrs :attrs} (parse-attrs attrs)
           default-uri* (or default default-uri)
           {:keys [uris new*]} (reduce-kv (fn [res pf uri]
                                            (if (get-in res [:uris uri])
                                              (update-in res [:new*] conj uri)
                                              (update-in res [:uris] assoc uri pf)))
                                          {:uris uri-prefixes :new* #{}} nss)
           to-kw (fn [{:keys [name uri]}]
                   (if-let [pf (and (not= uri default-uri*) (get uris uri))]
                     (keyword pf name)
                     (do (assert (= uri default-uri*)
                                 (str "Tag uri " uri " != default uri " default-uri*))
                         (keyword name))))]
       (cond
        tag (apply element (to-kw tag)
                   (-> (if default {:xmlns default} {})
                       (into (map #(vector (to-kw %1) %2)
                                  (keys only-attrs) (vals only-attrs)))
                       (into (map #(vector (keyword "xmlns" %1) %2)
                                  (map uris new*) new*)))
                   (map #(walk-cleanup-prefixes % default-uri* uris)
                        content))
        content (assoc xml :content (map #(walk-cleanup-prefixes % default-uri* uris)
                                         content))
        :else xml))))
