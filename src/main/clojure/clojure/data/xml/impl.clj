;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns clojure.data.xml.impl
  "Shared private code for data.xml namespaces"
  {:author "Herwig Hochleitner"}
  (:require [clojure.data.xml.node :refer [element* map->Element]]
            [clojure.string :as str]
            [clojure.data.xml.impl.xmlns :refer 
             [default-ns-prefix null-ns-uri xml-ns-prefix xml-ns-uri
              xmlns-attribute xmlns-attribute-ns-uri ns-env-meta-key empty-namespace
              uri-from-prefix prefix-from-uri]])
  (:import (clojure.data.xml.node Element)
           (clojure.lang ILookup Keyword)
           (java.io Writer)
           (javax.xml XMLConstants)
           (javax.xml.namespace NamespaceContext QName)))

(set! *warn-on-reflection* false)

;; Name

;;;; print-dup/*data-reader* support for #xml/name

(defmethod print-dup QName [^QName qn ^Writer writer]
  (let [dup-str (get-method print-dup String)]
    (.write writer "#xml/name{")
    (let [u (.getNamespaceURI qn)
          n (.getLocalPart qn)
          p (.getPrefix qn)]
      (when u
        (.write writer ":uri ")
        (dup-str u writer)
        (when n (.write writer " ")))
      (when n
        (.write writer ":name ")
        (dup-str n writer)
        (when p (.write writer " ")))
      (when p
        (.write writer ":prefix ")
        (dup-str p writer)))
    (.write writer "}")))

(defmethod print-dup Element [el ^Writer writer]
  (let [print-map (get-method print-method clojure.lang.IPersistentMap)]
    (.write writer "#xml/element")
    (print-map el writer)))

(defmethod print-method QName [qn writer]
  (print-dup qn writer))
(defmethod print-method Element [el writer]
  (print-dup el writer))

(definline min-qname [qn]
  `(let [^QName qn# ~qn]
     (cond
      (instance? clojure.lang.Named qn#) (keyword qn#)
      (str/blank? (.getNamespaceURI qn#)) (keyword nil (.getLocalPart qn#))
      :else qn#)))

;;;; Unifying protocol for keyword, string, QName

(defonce clj-ns-xmlns
  (atom {}))

(defn kw-ns-uri [ns pf]
  (let [xmlns (if (instance? clojure.lang.ILookup ns)
                ns
                (get @clj-ns-xmlns
                     (cond (instance? clojure.lang.Namespace ns) (ns-name ns)
                           (symbol? ns) (name ns)
                           :else ns)))
        uri (and xmlns (get xmlns (or pf default-ns-prefix)))]
    (when (str/blank? uri)
      (throw (ex-info (str "Reference Error: Namespace " ns " has no "
                           (if-not (str/blank? pf)
                             (str "prefix " pf " mapped to an xmlns")
                             "default xmlns"))
                      {:ns ns :pf pf :xmlns xmlns})))
    uri))

(def reify-kw
  (memoize
   (fn [kw]
     (let [n (name kw)
           ns (namespace kw)]
       (cond (and (str/blank? ns)
                  (= xmlns-attribute n))
             (QName. xmlns-attribute-ns-uri xmlns-attribute default-ns-prefix)

             (= ns xml-ns-prefix)
             (QName. xmlns-attribute-ns-uri n xml-ns-prefix)

             :else (let [[has-prefix pf* n*]
                         (re-matches #"([^:]+):(.+)" n)
                         pf (or pf* default-ns-prefix)]
                     (cond
                      ns (QName. (kw-ns-uri ns pf)
                                 (or n* n) pf)
                      has-prefix (throw (ex-info "No global prefixes" {:prefix pf}))
                      :else (keyword nil n))))))))

(def reify-str
  (memoize
   (fn [s]
     (min-qname
      (QName/valueOf s)))))

(defn xml-name
  ([val]
     (cond
      (instance? QName val) (min-qname val)
      (keyword? val) (reify-kw val)
      (string? val) (reify-str val)
      (map? val) (let [{:keys [uri name prefix]} val]
                   (xml-name uri name prefix))
      :else (throw (IllegalArgumentException. (str "Not a valid qname: " val)))))
  ([uri name] (xml-name uri name nil))
  ([uri name prefix]
     (if (str/blank? uri)
       (keyword nil name)
       (QName. uri name (or prefix default-ns-prefix)))))

(defn xml-element [{:keys [tag attrs content]}]
  (element* (xml-name tag)
            (persistent!
             (reduce-kv #(assoc! %1 (xml-name %2) %3)
                        (transient {}) attrs))
            (map #(if (map? %) (xml-element %) %)
                 content)))

(defn set-reader-tags! []
  (set! *data-readers*
        (assoc *data-readers*
          'xml/name #'xml-name
          'xml/element #'xml-element)))

(defn install-reader-tags!! []
  (alter-var-root #'*data-readers* assoc
                  'xml/name #'xml-name
                  'xml/element #'xml-element))
;;

(defprotocol RawName
  "A protocol for values, that can occur as an xml name, i.e. tags and attr names"
  (raw-uri [name])
  (raw-name [name])
  (raw-prefix [name]))

(extend-protocol RawName
  QName
  (raw-uri [qn] (str (.getNamespaceURI qn)))
  (raw-name [qn] (str (.getLocalPart qn)))
  (raw-prefix [qn] (str (.getPrefix qn)))
  Keyword
  (raw-uri [kw] null-ns-uri)
  (raw-name [kw] (str (name kw)))
  (raw-prefix [kw] (str (namespace kw)))
  String
  (raw-uri [s] (raw-uri (reify-str s)))
  (raw-name [s] (raw-name (reify-str s)))
  (raw-prefix [s] default-ns-prefix))

(defn parse-attrs [attrs]
  (when attrs
    (reduce-kv (fn [res k v*]
                 (let [uri (raw-uri k)
                       name (raw-name k)
                       prefix (raw-prefix k)
                       v (str v*)]
                   (cond
                    (= xmlns-attribute name)
                    (assoc res :default v)

                    (or (= xmlns-attribute-ns-uri uri)
                        (= xmlns-attribute prefix))
                    (-> res
                        (assoc-in [:nss name] v)
                        (assoc-in [:uris v] name))
                    :else
                    (assoc-in res [:attrs k] v))))
               {:default nil
                :attrs {}
                :uris {}
                :nss {}}
               attrs)))

(defmacro static-case
  "Variant of case where keys are evaluated at compile-time"
  [val & cases]
  `(case ~val
     ~@(mapcat (fn [[field thunk]]
                 [(eval field) thunk])
               (partition 2 cases))
     ~@(when (odd? (count cases))
         [(last cases)])))

;;; Public API that must be available to subpackages

;; the export-api macro creates a var mirroring an existing var in another namespace

(defn- export-form [var-name]
  (let [vsym (symbol (name var-name))]
    `[(def ~vsym ~var-name)
      (alter-meta! (var ~vsym)
                   (constantly (assoc (meta (var ~var-name))
                                 :wrapped-by (var ~vsym))))]))

(defmacro export-api [& names]
  (cons 'do (mapcat export-form names)))

;; API

(defn element?
  "Test if an element can be interpreted as an xml node (by test whether it has a :tag)"
  [node]
  (boolean (:tag node)))
