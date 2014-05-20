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
  (:require [clojure.data.xml.node :refer [map->Element]])
  (:import (clojure.data.xml.node Element)
           (clojure.lang ILookup Keyword)
           (java.io Writer)
           (javax.xml XMLConstants)
           (javax.xml.namespace NamespaceContext QName)))

;; Constants

(def ^:const default-ns-prefix XMLConstants/DEFAULT_NS_PREFIX)
(def ^:const null-ns-uri XMLConstants/NULL_NS_URI)
(def ^:const xml-ns-prefix XMLConstants/XML_NS_PREFIX)
(def ^:const xml-ns-uri XMLConstants/XML_NS_URI)
(def ^:const xmlns-attribute XMLConstants/XMLNS_ATTRIBUTE)
(def ^:const xmlns-attribute-ns-uri XMLConstants/XMLNS_ATTRIBUTE_NS_URI)

;; Namespace

(defprotocol XmlNamespace
  (uri-from-prefix [ns prefix])
  (prefix-from-uri [ns uri]))

(extend-protocol XmlNamespace
  NamespaceContext
  (uri-from-prefix [nc prefix]
    (.getNamespaceURI nc prefix))
  (prefix-from-uri [nc uri]
    (.getPrefix nc uri)))

(deftype XmlNamespaceImpl [forward back alt_back default]
  ;; A basic bijectional map to look up namespace prefixes and uris
  XmlNamespace
  (uri-from-prefix [_ prefix]
    (if (= default-ns-prefix prefix)
      default
      (get forward prefix)))
  (prefix-from-uri [_ uri]
    (if (= default uri)
      default-ns-prefix
      (get back uri)))
  ILookup
  (valAt [this k]
    (get forward k))
  (valAt [this k default]
    (get forward k default)))

(defn assoc-prefix [^XmlNamespaceImpl nc & pfuris]
  (loop [forward (transient (.-forward nc))
         back (transient (.-back nc))
         alt-back (transient (.-alt_back nc))
         pfuris pfuris
         default (.-default nc)]
    (if-let [[pf uri & rst] (seq pfuris)]
      (if (= default-ns-prefix pf)
        (recur forward back alt-back rst (str uri))
        (let [had-uri (forward pf)
              alt-pfs (alt-back had-uri [])]
          (cond
           ;; dissoc
           (empty? uri) (if (= pf (back had-uri))
                          ;; prefix is current
                          (if-let [new-pf (first alt-pfs)]
                            ;; have alternate prefix
                            (recur (dissoc! forward pf)
                                   (assoc! back had-uri new-pf)
                                   (assoc! alt-back had-uri (subvec alt-pfs 1))
                                   rst default)
                            ;; last binding
                            (recur (dissoc! forward pf)
                                   (dissoc! back had-uri)
                                   alt-back rst default))
                          ;; prefix is alternate
                          (recur (dissoc! forward pf)
                                 back
                                 (assoc! alt-back had-uri (vec (remove #{pf} alt-pfs)))
                                 rst default))
           ;; assoc to existing uri
           (get back uri) (recur (assoc! forward pf uri)
                                 back
                                 (cond-> alt-back
                                         true (assoc! uri
                                                      (conj (alt-back uri []) pf))
                                         had-uri (assoc! had-uri (vec (remove #{pf} alt-pfs))))
                                 rst default)
           ;; assoc to new uri
           :else (recur (assoc! forward pf uri)
                        (assoc! back uri pf)
                        (cond-> alt-back
                                had-uri (assoc! had-uri (vec (remove #{pf} alt-pfs))))
                        rst default))))
      (XmlNamespaceImpl. (persistent! forward)
                         (persistent! back)
                         (persistent! alt-back)
                         default))))

(defn dissoc-prefix [nc & prefixes]
  (apply assoc-prefix nc (mapcat vector prefixes (constantly ""))))

(def ^:const empty-namespace
  (assoc-prefix (XmlNamespaceImpl. {} {} {} null-ns-uri)
                xml-ns-prefix     xml-ns-uri
                xmlns-attribute   xmlns-attribute-ns-uri))

(defn into-namespace [en prefix-map]
  (apply assoc-prefix (or en empty-namespace) (apply concat prefix-map)))

(defn to-namespace [prefix-map]
  (into-namespace empty-namespace prefix-map))

;; Name

;;;; print-dup/*data-reader* support for #xml/name

(defmethod print-method QName [^QName qn ^Writer writer]
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

(defmethod print-method Element [el ^Writer writer]
  (let [print-map (get-method print-method clojure.lang.IPersistentMap)]
    (.write writer "#xml/element")
    (print-map el writer)))

(defn xml-name
  ([val]
     (cond
      (string? val) (QName/valueOf val)
      (map? val) (let [{:keys [uri name prefix]} val]
                   (xml-name uri name prefix))
      :else (throw (IllegalArgumentException. (str "Not a valid qname: " val)))))
  ([uri name prefix] (QName. (or uri null-ns-uri) name (or prefix default-ns-prefix))))

(alter-var-root #'*data-readers* assoc
                'xml/name #'xml-name
                'xml/element #'map->Element)

;;;; Unifying protocol for keyword, string, QName

(defprotocol XmlName
  "A protocol for values, that can occur as an xml name, i.e. tags and attr names"
  (get-uri [name])
  (get-name [name])
  (get-prefix [name]))

(extend-protocol XmlName
  QName
  (get-uri [qn]    (.getNamespaceURI qn))
  (get-name [qn]   (.getLocalPart qn))
  (get-prefix [qn] (.getPrefix qn))
  Keyword
  (get-uri [_]     nil)
  (get-name [kw]   (name kw))
  (get-prefix [kw] (namespace kw))
  String
  (get-uri [_]     nil)
  (get-name [s]    (let [i (.lastIndexOf s "/")]
                     (if (pos? i)
                       (subs s (inc i))
                       s)))
  (get-prefix [s]  (let [i (.lastIndexOf s "/")]
                     (when (pos? i)
                       (subs s 0 i)))))

(defn tag-info
  ([xn] (tag-info xn empty-namespace))
  ([xn ns-ctx] (tag-info xn ns-ctx {:nss {} :uris {} :default nil}))
  ([xn ns-ctx {:keys [nss uris default]}]
     (let [u (get-uri xn)
           n (get-name xn)
           p (or (get-prefix xn) default-ns-prefix)]
       (if u
         {:uri u :name n :prefix (or (get uris u)
                                     (prefix-from-uri ns-ctx u)
                                     default-ns-prefix)}
         {:name n :prefix p :uri (or (if (= p "")
                                       default
                                       (get nss p))
                                     (uri-from-prefix ns-ctx p)
                                     null-ns-uri)}))))

(defn attr-info
  ([xn] (attr-info xn empty-namespace))
  ([xn ns-ctx]
     (let [u (get-uri xn)
           n (get-name xn)
           p (or (get-prefix xn) default-ns-prefix)]
       (if u
         {:uri u :name n :prefix (or (prefix-from-uri ns-ctx u)
                                     default-ns-prefix)}
         {:name n :prefix p :uri (if (= p "")
                                   null-ns-uri
                                   (uri-from-prefix ns-ctx p))}))))

(defn parse-attrs [attrs]
  (when attrs
    (reduce-kv (fn [res k v*]
                 (let [{:keys [uri name prefix]} (attr-info k)
                       v (str v*)]
                   (cond
                    (and (empty? prefix) (= xmlns-attribute name))
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
