(ns clojure.data.xml.impl.xmlns
  (:import (clojure.lang ILookup)
           (javax.xml XMLConstants)
           (javax.xml.namespace NamespaceContext))
  (:require [clojure.string :as str]))

;; # xmlns utilities

;; ## Constants

(def ^:const default-ns-prefix XMLConstants/DEFAULT_NS_PREFIX)
(def ^:const null-ns-uri XMLConstants/NULL_NS_URI)
(def ^:const xml-ns-prefix XMLConstants/XML_NS_PREFIX)
(def ^:const xml-ns-uri XMLConstants/XML_NS_URI)
(def ^:const xmlns-attribute XMLConstants/XMLNS_ATTRIBUTE)
(def ^:const xmlns-attribute-ns-uri XMLConstants/XMLNS_ATTRIBUTE_NS_URI)
(def ^:const ns-env-meta-key :clojure.data.xml/namespace-environment)

;; ## Namespace

(defprotocol XmlNamespace
  (uri-from-prefix [ns prefix])
  (prefix-from-uri [ns uri]))

(defn assert-uri [nc uri]
  (or uri (throw (ex-info (str "Uri " uri " not bound") {:ns-ctx nc :uri uri}))))

(defn assert-prefix [nc pf]
  (or pf (throw (ex-info (str "Prefix " pf " not bound") {:ns-ctx nc :pf pf}))))

(extend-protocol XmlNamespace
  NamespaceContext
  (uri-from-prefix [nc prefix]
    (assert-uri nc (.getNamespaceURI nc prefix)))
  (prefix-from-uri [nc uri]
    (assert-prefix nc (.getPrefix nc uri))))

;; XmlNamespaceImpl is a persistent bidirectional map, for storing prefix - uri pairs.
;; It also has a slot for storing alternate prefixes for a uri
;; see assoc-prefix

(deftype XmlNamespaceImpl [forward back alt_back default]
  Object
  (toString [_]
    (str "{.-forward " forward
         ", .-back " back
         ", .-alt_back " alt_back
         ", .-default " default "}"))
  XmlNamespace
  (uri-from-prefix [nc prefix]
    (assert-uri nc (if (str/blank? prefix)
                     default
                     (get forward (str prefix)))))
  (prefix-from-uri [nc uri]
    (assert-prefix nc (if (= default (str uri))
                        default-ns-prefix
                        (get back (str uri)))))
  ILookup
  (valAt [this k]
    (uri-from-prefix this k))
  (valAt [this k default-val]
    (let [uri (if (str/blank? k)
                default
                (get forward k))]
      (if (str/blank? uri)
        default-val
        uri))))

(defn prefix-bindings
  "# Emitter helper, to find emittable xmlns clauses
  This is designed to - in concert with assoc-prefix - minimize emitted xmlns clauses.
  ## Usage
  - `(prefix-bindings nc)` `=>` current active prefixes in nc
  - `(prefix-bindings nc parent)` `=>` active prefix bindings, whose uri is _not_ bound in parent"
  ([nc] (persistent!
         (reduce-kv #(assoc! %1 %3 %2) (transient {}) (.-back nc))))
  ([nc parent]
     (let [uris (.-back parent)]
       (persistent!
        (reduce-kv (fn [res u p]
                     (if (contains? uris u)
                       res
                       (assoc! res p u)))
                   (transient {})
                   (.-back nc))))))

(defn assoc-prefix
  "# Establish new clauses in xmlns.
  Urls that are already bound to a prefix keep their old prefix in the reverse map,
  but get recorded as alternate prefixes. Thus when a prefix is being unbound, by
  associating it with \"\", the outermost alternate prefix will be restored.

  This mimicks the behavor of an xml parser.

  ## Example
      (assoc-prefix nc
        :pf \"uri\"
        :del-pf null-ns-uri)"
  [^XmlNamespaceImpl nc & pfuris]
  (assert (zero? (mod (count pfuris) 2)))
  (if-not (seq pfuris)
    nc
    (loop [forward (transient (.-forward nc))
           back (transient (.-back nc))
           alt-back (transient (.-alt_back nc))
           pfuris pfuris
           default (.-default nc)]
      (if-let [[pf' uri' & rst] (seq pfuris)]
        (let [pf (or pf' default-ns-prefix)
              uri (or uri' null-ns-uri)]
          (if (= default-ns-prefix pf)
            (recur forward back alt-back rst uri)
            (let [had-uri (forward pf)
                  alt-pfs (alt-back had-uri [])]
              (cond
               ;; dissoc
               (= null-ns-uri uri) (if (= pf (back had-uri))
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
                                             had-uri (assoc! had-uri (vec (remove #{pf} alt-pfs)))
                                             true (as-> ab
                                                        (assoc! ab uri (conj (ab uri []) pf))))
                                     rst default)
               ;; assoc to new uri
               :else (recur (assoc! forward pf uri)
                            (assoc! back uri pf)
                            (cond-> alt-back
                                    had-uri (assoc! had-uri (vec (remove #{pf} alt-pfs))))
                            rst default)))))
        (XmlNamespaceImpl. (persistent! forward)
                           (persistent! back)
                           (persistent! alt-back)
                           default)))))

(defn dissoc-prefix
  "# Dissociate a prefix in xmlns
  This amounts to `(assoc-prefix nc prefix null-ns-uri)`,
  which restores an alternate prefix for the bound uri, if available."
  [nc & prefixes]
  (apply assoc-prefix nc (mapcat vector prefixes (repeat null-ns-uri))))

(def ^:const empty-namespace
  (assoc-prefix (XmlNamespaceImpl. {} {} {} null-ns-uri)
                xml-ns-prefix     xml-ns-uri
                xmlns-attribute   xmlns-attribute-ns-uri))

(defn into-namespace [en prefix-map]
  (apply assoc-prefix (or en empty-namespace)
         (interleave (keys prefix-map)
                     (vals prefix-map))))

(defn to-namespace [prefix-map]
  (into-namespace empty-namespace prefix-map))
