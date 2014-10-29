;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns clojure.data.xml
  "Functions to parse XML into lazy sequences and lazy trees and
  emit these as text."
  {:author "Chris Houser"}
  (:require [clojure.data.xml.emit :refer [check-stream-encoding
                                           emit-event
                                           emit-event-raw
                                           flatten-elements
                                           indenting-transformer
                                           emit*]]
            [clojure.data.xml.event :as event]
            [clojure.data.xml.impl :as impl :refer
             [export-api]]
            [clojure.data.xml.impl.xmlns :as xmlns]
            [clojure.data.xml.node :as node]
            [clojure.data.xml.parse :as parse
             :refer [event-tree
                     new-xml-input-factory
                     pull-seq
                     infoset-tag-builder
                     raw-tag-builder]]
            [clojure.data.xml.syntax :refer [as-elements]]
            [clojure.walk :refer [postwalk]])
  (:import (javax.xml.namespace QName)
           (java.io Writer OutputStreamWriter StringWriter StringReader)
           (javax.xml.stream XMLStreamWriter XMLOutputFactory)
           (javax.xml.transform.stream StreamSource StreamResult)))

(export-api impl/element? impl/qname impl/element
            event/event
            node/element node/element* node/cdata node/xml-comment
            xmlns/to-namespace xmlns/into-namespace xmlns/assoc-prefix
            xmlns/dissoc-prefix xmlns/empty-namespace
            xmlns/default-ns-prefix xmlns/null-ns-uri ns-env-meta-key
            xmlns/xml-ns-prefix xmlns/xmlns-attribute xmlns/xmlns-attribute-ns-uri)

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
  "Parse the XML InputSource source using a pull-parser. Returns
   a lazy sequence of Event records.  Accepts key pairs
   with XMLInputFactory options, see http://docs.oracle.com/javase/6/docs/api/javax/xml/stream/XMLInputFactory.html
   and xml-input-factory-props for more information.
   Defaults coalescing true and supporting-external-entities false.

   Options:
     :xmlns-meta <bool>          Attach xmlns as ns-env-meta-key
     :xmlns-attr <bool>          Generate attributes for xmlns declarations
     :preserve-whitespace <bool> Leave whitespace characters in result
     :resolve <bool>             Whether to resolve the uri of parsed qnames
         true  -- Generate java.util.QName instances or plain keywords for names in null-ns-uri
         false -- Generate old-style namespaced keywords, where the prefix becomes kw-ns

     :factory <map kv->bool>     xml-input-factory properties, default :coalescing
         #{:allocator :coalescing :namespace-aware
           :replacing-entity-references :reporter :resolver
           :support-dtd :supporting-external-entities :validating}"
  [s & {:as props}]
  (parse/source-seq s props))

(defn parse-raw
  "Parse the source into old style raw elements, using the kw namespace to save the prefix"
  [source & {:as props}]
  (parse/parse source
               (merge {:resolve false
                       :xmlns-meta false
                       :xmlns-attrs true}
                      props)
               (raw-tag-builder (:xmlns-attrs props))))

(defn parse-str-raw
  "Parse the source string into old style raw elements, using the kw namespace to save the prefix"
  [s & props]
  (let [sr (StringReader. s)]
    (apply parse-raw sr props)))

(defn parse*
  "Parse the source into resolved elements, by default with xmlns-meta,
     see source-seq for options

   ns can be an xml namspace, clojure namespace, a symbol or nil.
   - If non-nil, the parser uses the ns to emit keywords for bound uris
   - If it's a clojure namespace or symbol, the xml namespace is looked up
     in the global defns table"
  [ns source & {:as props}]
  (let [opts (merge {:resolve true
                     :xmlns-meta true
                     :xmlns-attrs false}
                    props)]
    (parse/parse source opts
                 (infoset-tag-builder ns (:xmlns-meta opts)))))

(defn parse-str*
  "Parse the source string into resolved elements, by default with xmlns-meta,
     see source-seq for options

   ns can be an clojure namespace, a symbol or nil.
   - If non-nil, the parser uses the ns to emit keywords for bound uris
   - xml namespace is looked up in the global defns table"
  [ns source & props]
  (let [sr (StringReader. source)]
    (apply parse* ns sr props)))

(defmacro parse
  "Macro to parse and resolve the source and generate keywords
   for names whose uri is announced by defns in the calling namespace.
     see source-seq for options"
  [source & props]
  `(parse* ~*ns* ~source ~@props))

(defmacro parse-str
  "Macro to parse and resolve the source string and generate keywords
   for names whose uri is announced by defns in the calling namespace.
     see source-seq for options"
  [source & props]
  `(parse-str* ~*ns* ~source ~@props))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; XML Emitting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn emit-raw
  "Prints the given Raw Element tree as XML text to stream.
   Options:
    :encoding   <str>   Character encoding to use
    :fragment   <bool>  Whether to write start/end-document
    :xmlns-meta <bool>  Generate xmlns clauses from metadata in ns-env-meta-key"
  [e stream & {:as opts}]
  (emit* e stream opts emit-event-raw))

(defn emit-str-raw
  "Emits the Raw Element to String and returns it"
  [e & opts]
  (let [^StringWriter sw (StringWriter.)]
    (apply emit-raw e sw opts)
    (.toString sw)))

(defn emit
  "Prints the given Element as XML text to stream.
   Options:
    :encoding   <str>   Character encoding to use
    :fragment   <bool>  Whether to write start/end-document
    :xmlns-meta <bool>  Generate xmlns clauses from metadata in ns-env-meta-key"
  [e stream & {:as opts}]
  (emit* e stream opts emit-event))

(defn emit-str
  "Emits the Element to String and returns it"
  [e & opts]
  (let [^StringWriter sw (StringWriter.)]
    (apply emit e sw opts)
    (.toString sw)))

(defn indent
  "Emits the XML and indents the result.  WARNING: this is slow
   it will emit the XML and read it in again to indent it.  Intended for
   debugging/testing only."
  [e ^Writer stream & opts]
  (let [sw (StringWriter.)
        _ (apply emit e sw opts)
        source (-> sw .toString StringReader. StreamSource.)
        result (StreamResult. stream)]
    (.transform (indenting-transformer) source result)))

(defn indent-str
  "Emits the XML and indents the result.  Writes the results to a String and returns it"
  [e & opts]
  (let [^StringWriter sw (StringWriter.)]
    (apply indent e sw opts)
    (.toString sw)))

(defmacro defns
  "Define mappings for *ns* to keyword -> qname mapping table.
   Optional first string arg is for the default prefix (\"ns:\" => xmlns=\"ns:\").
   Followed by keyword -> string prefix mappings (:p \"ns:\" => xmlns:p=\"ns:\")"
  {:arglists '([xmlns &{:as prefix-xmlnss}] [&{:as prefix-xmlnss}])}
  [& args]
  (let [xns (when (string? (first args))
              (first args))
        nss (if xns
              (next args)
              args)
        nsks (map name (take-nth 2 nss))
        nsvs (take-nth 2 (next nss))
        nss' (interleave nsks nsvs)
        nss'' (if xns
                (list* "" xns nss')
                nss')
        nsn (str (ns-name *ns*))]
    `(swap! impl/clj-ns-xmlns assoc ~nsn
            (assoc-prefix empty-namespace ~@nss''))))

(defmacro alias-ns
  "Define a clojure namespace alias for shortened keyword and symbol namespaces.
   If namespace doesn't exist, it is created.
   ## Example
   (in-ns 'my.impl.webdav)
   (defns \"DAV:\")
   (in-ns 'user)
   (alias-ns dav my.impl.webdav)
   {:tag ::dav/propfind :content []}"
  [alias ns-sym & ans]
  `(do (create-ns '~ns-sym)
       (alias '~alias '~ns-sym)
       ~(when (seq ans)
          `(alias-ns ~@ans))))
