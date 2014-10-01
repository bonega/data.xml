;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns clojure.data.xml.parse
  "Parsing functionality. This namespace is not public API, but will stay stable between patch versions."
  {:author "Chris Houser, Herwig Hochleitner"}
  (:require [clojure.data.xml.event :refer [event]]
            [clojure.data.xml.impl :refer [static-case xml-name min-qname
                                           raw-name raw-uri clj-ns-xmlns]]
            [clojure.data.xml.impl.xmlns :refer
             [xmlns-attribute into-namespace ns-env-meta-key empty-namespace
              xml-ns-prefix prefix-from-uri]]
            [clojure.data.xml.node :as node]
            [clojure.string :as str])
  (:import (clojure.data.xml.event Event)
           (javax.xml.stream XMLStreamConstants XMLStreamReader
                             XMLInputFactory)))

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
  ([parent exit? node coll]
     ;; compatibility and demonstration
     (seq-tree (fn [e t _] (parent e t)) exit? node coll nil))
  ([parent exit? node coll last-parent]
     (lazy-seq
      (when-let [[event] (seq coll)]
        (let [more (rest coll)]
          (if (exit? event)
            (cons nil more)
            (let [tree (seq-tree parent exit? node more last-parent)]
              (if-let [p (parent event (lazy-seq (first tree)) last-parent)]
                (let [subtree (seq-tree parent exit? node (lazy-seq (rest tree)) p)]
                  (cons (cons p (lazy-seq (first subtree)))
                        (lazy-seq (rest subtree))))
                (cons (cons (node event) (lazy-seq (first tree)))
                      (lazy-seq (rest tree)))))))))))

(defn event-tree
  "Returns a lazy tree of Element objects for the given seq of Event
  objects. See source-seq and parse."
  [events to-element]
  (ffirst
   (seq-tree
    (fn [^Event event contents parent]
      (when (= :start-element (.type event))
        (to-element parent (.name event) (.nss event) (.attrs event) contents)))
    (fn [^Event event] (= :end-element (.type event)))
    (fn [^Event event] (.str event))
    events
    nil)))

(defn attr-prefix [^XMLStreamReader sreader index]
  (let [p (.getAttributePrefix sreader index)]
    (when-not (str/blank? p)
      p)))

(defn attr-hash [^XMLStreamReader sreader resolve xmlns]
  (let [tr (reduce (fn [tr i]
                     (assoc!
                      tr (if resolve
                           (min-qname (.getAttributeName sreader i))
                           (keyword (attr-prefix sreader i)
                                    (.getAttributeLocalName sreader i)))
                      (.getAttributeValue sreader i)))
                   (transient {})
                   (range (.getAttributeCount sreader)))]
    (persistent! 
     (if xmlns
       (reduce (fn [tr i]
                 (assoc!
                  tr (if-let [prefix (.getNamespacePrefix sreader i)]
                       (keyword xmlns-attribute prefix)
                       :xmlns)
                  (.getNamespaceURI sreader i)))
               tr (range (.getNamespaceCount sreader)))
       tr))))

(defn nss-hash [^XMLStreamReader sreader]
  (persistent!
   (reduce (fn [tr i]
             (assoc! tr (.getNamespacePrefix sreader i)
                     (.getNamespaceURI sreader i)))
           (transient {}) 
           (range (.getNamespaceCount sreader)))))

(defn xml-tag [^XMLStreamReader sreader resolve]
  (if resolve
    (.getName sreader)
    (let [prefix (.getPrefix sreader)
          name (.getLocalName sreader)]
      (if (str/blank? prefix)
        (keyword name)
        (keyword prefix name)))))

; Note, sreader is mutable and mutated here in pull-seq, but it's
; protected by a lazy-seq so it's thread-safe.
(defn pull-seq
  "Creates a seq of events.  The XMLStreamConstants/SPACE clause below doesn't seem to 
   be triggered by the JDK StAX parser, but is by others.  Leaving in to be more complete."
  [^XMLStreamReader sreader
   [xmlns :as xmlnss]
   {:as opts :keys [resolve xmlns-attrs preserve-whitespace]
    :or {resolve true xmlns-attrs false}}]
  (lazy-seq
   (loop []
     (static-case (.next sreader)
       XMLStreamConstants/START_ELEMENT
       (let [nsh (nss-hash sreader)
             [xmlns' :as xmlnss'] (when xmlnss
                                    (cons (into-namespace xmlns nsh)
                                          xmlnss))]
         (cons (event :start-element
                      (xml-tag sreader resolve)
                      (attr-hash sreader resolve xmlns-attrs)
                      nil
                      nsh
                      (when xmlnss' {ns-env-meta-key xmlns'}))
               (pull-seq sreader xmlnss' opts))) 
       XMLStreamConstants/END_ELEMENT
       (cons (event :end-element (xml-tag sreader resolve))
             (pull-seq sreader (next xmlnss) opts))
       XMLStreamConstants/CHARACTERS
       (if-let [text (or (and preserve-whitespace
                              (.isWhiteSpace sreader)
                              (.getText sreader))
                         (and (not (.isWhiteSpace sreader))
                              (.getText sreader)))]
         (cons (event :characters nil nil text)
               (pull-seq sreader xmlnss opts))
         (recur))
       XMLStreamConstants/END_DOCUMENT
       nil
       (recur) ;; Consume and ignore comments, spaces, processing instructions etc
       ))))

(def xml-input-factory-props
  {:allocator XMLInputFactory/ALLOCATOR
   :coalescing XMLInputFactory/IS_COALESCING
   :namespace-aware XMLInputFactory/IS_NAMESPACE_AWARE
   :replacing-entity-references XMLInputFactory/IS_REPLACING_ENTITY_REFERENCES
   :supporting-external-entities XMLInputFactory/IS_SUPPORTING_EXTERNAL_ENTITIES
   :validating XMLInputFactory/IS_VALIDATING
   :reporter XMLInputFactory/REPORTER
   :resolver XMLInputFactory/RESOLVER
   :support-dtd XMLInputFactory/SUPPORT_DTD})

(defn new-xml-input-factory [props]
  (let [fac (XMLInputFactory/newInstance)]
    (doseq [[k v] props
            :let [prop (xml-input-factory-props k)]]
      (.setProperty fac prop v))
    fac))

(defn source-seq
  "see data.xml/source-seq"
  [s {:keys [factory xmlns-meta] :as opts}]
  (when-let [key (and *assert* (some #{:allocator :coalescing :namespace-aware
                                       :replacing-entity-references :reporter :resolver
                                       :support-dtd :supporting-external-entities :validating}
                                     (keys opts)))]
    (throw (ex-info "Config format updated, please put input-factory-props in a map under :factory" {:key key :try `(source-seq '~'s :factory ~opts)})))
  (let [fac (new-xml-input-factory (merge {:coalescing true} factory))
        ;; Reflection on following line cannot be eliminated via a
        ;; type hint, because s is advertised by fn parse to be an
        ;; InputStream or Reader, and there are different
        ;; createXMLStreamReader signatures for each of those types.
        sreader (.createXMLStreamReader fac s)]
    (pull-seq sreader (when xmlns-meta (list empty-namespace)) opts)))

(defn parse [s props tag-emitter]
  (event-tree (source-seq s props) tag-emitter))

;; Tag constructors as param fns for parser

(defn min-xml-name [nc nsn xn]
  (if (keyword? xn)
    xn
    (let [u (raw-uri xn)
          n (raw-name xn)
          p (prefix-from-uri nc u nil)]
      (if p
        (keyword nsn (if (str/blank? p)
                       n
                       (str p ":" n)))
        xn))))

(defn infoset-tag-builder [ns gen-meta]
  (let [nsn (cond (not ns) nil
                  (symbol? ns) (name ns)
                  :else (name (ns-name ns)))
        nctx (when nsn
               (get @clj-ns-xmlns nsn))]
    (when (and ns (not nctx))
      (throw (ex-info (str "not found " ns) {:ns ns :nsn nsn})))
    (fn [parent tag nss attrs content]
      (node/element* (if nctx
                       (min-xml-name nctx nsn tag)
                       tag)
                     (if nctx 
                       (persistent!
                        (reduce-kv (fn [r k v]
                                     (assoc! r (min-xml-name nctx nsn k) v))
                                   (transient {}) attrs))
                       attrs)
                     content
                     (when gen-meta
                       {ns-env-meta-key (-> parent meta ns-env-meta-key
                                            (into-namespace nss))})))))

(defn raw-tag-builder [gen-xmlns]
  (fn [parent tag nss attrs content]
    (node/element* tag
                   (if gen-xmlns
                     (persistent!
                      (reduce-kv #(assoc! %1 (keyword xml-ns-prefix %2) %3)
                                 (transient attrs) nss))
                     attrs)
                   content)))
