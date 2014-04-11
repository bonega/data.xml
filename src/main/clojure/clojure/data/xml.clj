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
            [clojure.walk :refer [postwalk]]
            [clojure.data.xml.impl :as impl :refer
             [export-api parse-attrs get-name get-prefix get-uri resolve-tag! resolve-attr!
              tag-info attr-info
              xmlns-attribute make-qname default-ns-prefix null-ns-uri
              uri-from-prefix prefix-from-uri str-empty?]]
            (clojure.data.xml
             [event :as event]
             [parse :refer [pull-seq]]
             [emit :refer [indenting-transformer check-stream-encoding flatten-elements]]
             [node :as node]
             [syntax :refer [as-elements]]))
  (:import (javax.xml.stream XMLInputFactory
                             XMLStreamReader
                             XMLStreamConstants)
           (javax.xml.namespace QName NamespaceContext)
           (javax.xml XMLConstants)
           (java.nio.charset Charset)
           (java.io Reader)
           (clojure.data.xml.node Element CData Comment)))

(export-api impl/element?
            event/event
            node/element node/cdata node/xml-comment)

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

(defmacro with-xmlns
  "Lexically replace keywords, whose namespace appears in prefixes,
   with #xml/name's, constructed from the keyword name and uri found in prefixes.
   This is for embedding QName instances into clojure code."
  [prefixes form]
  {:pre [(map? prefixes) (every? string? (keys prefixes))]}
  (postwalk (fn [form]
              (if-let [uri (and (keyword? form)
                                (get prefixes (str (namespace form))))]
                (QName. uri (name form) (str (namespace form)))
                form))
            form))

(defn name=
  "This implementation deviates from QName.equals in that it uses the prefix to
   determine equality, if either URI is nil, thus unknown (as opposed to the
   default uri \"\")
   This is done in order to provide equality between QName and Keyword in a way
   that makes sense as long as they share a common root with appropriate xmlns binding"
  [n1 n2]
  (and (= (get-name n1) (get-name n2))
       (let [u1 (get-uri n1)
             u2 (get-uri n2)]
         (if-not (and u1 u2)
           (= (get-prefix n1) (get-prefix n2))
           (= u1 u2)))))

(defn resolve-attribute
  [att namespace-context]
  (if-let [prefix (get-prefix att)]
    (make-qname (resolve-attr! att namespace-context))
    att))

(defn resolve-tag
  "Resolve a prefixed xml name within namespace environment.
   - `default-uri` corresponds to an innermost xmlns= attribute
   - `prefixes` is a string map of {prefix uri ...}, corresponding
     to all active xmlns:prefix= attrs"
  [name* namespace-context]
  (if (get-uri name*)
    name*
    (make-qname (resolve-tag! name* namespace-context))))
