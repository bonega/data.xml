(ns clojure.data.xml.impl.xmlns
  (:import (javax.xml XMLConstants)
           (javax.xml.namespace NamespaceContext)
           (clojure.lang ILookup)))

(set! *warn-on-reflection* true)

;; Constants

(def ^:const default-ns-prefix XMLConstants/DEFAULT_NS_PREFIX)
(def ^:const null-ns-uri XMLConstants/NULL_NS_URI)
(def ^:const xml-ns-prefix XMLConstants/XML_NS_PREFIX)
(def ^:const xml-ns-uri XMLConstants/XML_NS_URI)
(def ^:const xmlns-attribute XMLConstants/XMLNS_ATTRIBUTE)
(def ^:const xmlns-attribute-ns-uri XMLConstants/XMLNS_ATTRIBUTE_NS_URI)
(def ^:const ns-env-meta-key :clojure.data.xml/namespace-environment)

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

(defn new-xmlns [parent nc]
  (let [back (.-back parent)]
    (remove (fn [[p u]]
              (contains? back u))
            (.-forward nc))))

(defn assoc-prefix [^XmlNamespaceImpl nc & pfuris]
  (assert (zero? (mod (count pfuris) 2)))
  (if-not (seq pfuris)
    nc
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
                           default)))))

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
