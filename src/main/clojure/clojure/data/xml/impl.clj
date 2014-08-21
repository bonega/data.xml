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
              xmlns-attribute xmlns-attribute-ns-uri ns-env-meta-key]])
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

(defn min-qname [^QName qn]
  (if (str/blank? (.getNamespaceURI qn))
    (keyword (.getLocalPart qn))
    qn))

;;;; Unifying protocol for keyword, string, QName

(defonce nss (ref {}))
(defonce pnss (ref {}))

(defn kw-ns-uri [ns pf]
  (let [uri (if-not (str/blank? pf)
              (-> @pnss
                  (get ns)
                  (get pf))
              (get @nss ns))]
    (when-not uri
      (throw (ex-info (str "Reference Error: Namespace " ns " has no "
                           (if pf (str "prefix " pf " mapped to an xmlns")
                               " default xmlns"))
                      {:ns ns :pf pf :lookup-ref (if pf pnss nss)})))
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
                      :else null-ns-uri)))))))

(defn reify-qname [val]
  (cond
   (instance? QName val) val
   (keyword? val) (reify-kw val)
   (string? val) (min-qname (QName/valueOf val))
   (map? val) (let [{:keys [uri name prefix]} val]
                (xml-name uri name prefix))
   :else (throw (IllegalArgumentException. (str "Not a valid qname: " val)))))

(defn xml-name
  ([val]
     (reify-qname val))
  ([uri name] (xml-name uri name nil))
  ([uri name prefix]
     (if (str/blank? uri)
       (keyword name)
       (QName. (or uri null-ns-uri) name (or prefix default-ns-prefix)))))

#_(defn xml-element [{:keys [tag attrs content]}]
    (element* (xml-name tag)
              (persistent!
               (reduce-kv #(assoc! %1 (xml-name %2) %3)
                          (transient {}) attrs))
              (map #(if (map? %) (xml-element %) %)
                   content)))

(alter-var-root #'*data-readers* assoc
                'xml/name #'xml-name)
;;

(defprotocol RawName
  "A protocol for values, that can occur as an xml name, i.e. tags and attr names"
  (raw-uri [name])
  (raw-name [name])
  (raw-prefix [name]))

(extend-protocol RawName
  QName
  (raw-uri [qn] (.getNamespaceURI qn))
  (raw-name [qn] (.getLocalPart qn))
  (raw-prefix [qn] (.getPrefix qn))
  Keyword
  (raw-uri [kw] nil)
  (raw-name [kw] (name kw))
  (raw-prefix [kw] (namespace kw))
  String
  (raw-uri [s] nil)
  (raw-name [s] (let [i (.lastIndexOf s "/")]
                  (if (pos? i)
                    (subs s (inc i))
                    s)))
  (raw-prefix [s]  (let [i (.lastIndexOf s "/")]
                     (if (pos? i)
                       (subs s 0 i)
                       default-ns-prefix))))

(defn raw-parse-attrs [attrs]
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

(defn resolve-tag! [name ns-ctx]
  (let [{:keys [uri prefix] :as info} (tag-info name ns-ctx)]
    (if (and (empty? uri) (not (empty? prefix)))
      (throw (ex-info (str "Prefix couldn't be resolved: " prefix)
                      {:name name :context ns-ctx}))
      info)))

(defn resolve-attr! [name ns-ctx]
  (let [{:keys [uri prefix] :as info} (attr-info name ns-ctx)]
    (if (and (not (empty? prefix)) (empty? uri))
      (throw (ex-info (str "Prefix couldn't be resolved: " prefix)
                      {:name name :context ns-ctx}))
      info)))

(defn str-empty? [s]
  (or (nil? s)
      (= s "")))

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
