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
  (:import (clojure.lang ILookup Keyword PersistentQueue)
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
        (cond
         (empty? uri) (let [had-uri (forward pf)
                            alt-pfs (alt-back had-uri)]
                        (if-let [new-pf (peek alt-pfs)]
                          (recur (dissoc! forward pf)
                                 (assoc! back had-uri new-pf)
                                 (assoc! alt-back had-uri (pop alt-pfs))
                                 rst default)
                          (recur (dissoc! forward pf)
                                 (dissoc! back had-uri)
                                 alt-back rst default)))
         (get back uri) (recur forward back
                               (assoc! alt-back uri
                                       (conj (or (alt-back uri)
                                                 PersistentQueue/EMPTY)
                                             pf))
                               rst default)
         :else (recur (assoc! forward pf uri)
                      (assoc! back uri pf)
                      alt-back rst default)))
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

(defn to-namespace [prefix-map]
  (apply assoc-prefix empty-namespace (apply concat prefix-map)))

;; Name

;;;; print-dup/*data-reader* support for #xml/name

(defmethod print-dup QName [^QName qn ^java.io.Writer writer]
  (let [dup-str (get-method print-dup String)]
    (.write writer "#xml/qname{")
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

(defn make-qname [{:keys [uri name prefix]}]
  (QName. (or uri null-ns-uri) name (or prefix default-ns-prefix)))

(alter-var-root #'*data-readers* assoc
                'xml/name #'make-qname)

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

(defn name-info
  ([xn] (name-info xn empty-namespace))
  ([xn ns-ctx] (name-info xn ns-ctx {:nss {} :uris {} :default nil}))
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

(defn parse-attrs [attrs]
  (when attrs
    (reduce-kv (fn [res k v*]
                 (let [{:keys [uri name prefix]} (name-info k)
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

(defn resolve! [name ns-ctx]
  (let [{:keys [uri prefix] :as info} (name-info name ns-ctx)]
    (if (and (empty? uri) (not (empty? prefix)))
      (throw (ex-info (str "Prefix couldn't be resolved: " prefix)
                      {:name name :context ns-ctx}))
      info)))

