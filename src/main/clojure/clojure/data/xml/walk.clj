;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns clojure.data.xml.walk
  "Tree walkers to transform xml in a namespace aware way"
  {:author "Herwig Hochleitner"}
  (:require
   [clojure.data.xml.impl :refer
    [parse-attrs assoc-prefix uri-from-prefix prefix-from-uri resolve-tag! resolve-attr!
     default-ns-prefix empty-namespace xmlns-attribute xmlns-attribute-ns-uri
     get-name get-uri tag-info attr-info]]
   [clojure.data.xml :refer [element? element resolve-tag resolve-attribute]]
   [clojure.zip :as z :refer [zipper]])
  (:import clojure.data.xml.Element))

(defn- update-ns-ctx [ns attrs]
  (let [{:keys [nss default]} (parse-attrs attrs)]
    (apply assoc-prefix ns
           (apply concat (cond-> nss
                                 default (->> (cons ["" default])))))))

(defn- update-ns-ctx* [node ns-ctx attrs]
  (with-meta node :clojure.data.xml/ns-ctx
    (update-ns-ctx ns-ctx attrs)))

(defn ns-context [node]
  (:clojure.data.xml/ns-ctx (meta node) empty-namespace))

(defn- z-content
  ([element] (z-content element (:content element)))
  ([element content]
     (let [ns-ctx (ns-context element)]
       (map #(update-ns-ctx* % ns-ctx (:attrs %))
            content))))

(defn xml-zip
  "Returns a zipper for xml elements (as from xml/parse),
  given a root element. You can get call ns-context on an element returned by the zipper
  to get the namespace context at that point."
  [root]
  (zipper #(or (:tag %) (:content %))
          (comp seq z-content)
          (fn [{:keys [tag attrs] :as node} children]
            (with-meta (apply element tag attrs children)
              (meta node)))
          (update-ns-ctx* root (ns-context root) (:attrs root))))

(defn element-walk
  "A lazy tree walker, calling (f node ns-ctx*) at each element, parent first.
   Uses return value as new node, recurs on new content.
   If returned node has :clojure.data.xml/ns-ctx in its metadata, the new ns-ctx
   will be used within content"
  [node ns-ctx f]
  (let [node* (if (element? node)
                (f node (update-ns-ctx ns-ctx (:attrs node)))
                node)
        ns-ctx* (or (:clojure.data.xml/ns-ctx (meta node*))
                    (if (element? node*)
                      (update-ns-ctx ns-ctx (:attrs node*))
                      ns-ctx))]
    (if-let [content (:content node*)]
      (assoc node* :content (map #(element-walk % ns-ctx* f) content))
      node*)))

(defn walk-resolve-names
  "Transforms an xml tree, so that every tag and attribute name are resolved to an
   #XmlName[:name :uri]. This uses xmlns* attributes to generate the XmlNames.
   If env-metadata is true, then xmlns attributes are removed and instead and
   the full namespace context is returned as :clojure.data.xml/ns-ctx metadata"
  ([xml] (walk-resolve-names xml true empty-namespace))
  ([xml env-metadata] (walk-resolve-names xml env-metadata empty-namespace))
  ([xml env-metadata ns-ctx]
     (element-walk
      xml ns-ctx
      (fn [{:keys [tag attrs content] :as node} ns-ctx]
        (if (element? node)
          (if env-metadata
            (with-meta
              (Element. (resolve-tag tag ns-ctx)
                        (into {} (remove #(let [[name] %]
                                            (or (= xmlns-attribute (get-name name))
                                                (= xmlns-attribute-ns-uri (get-uri name))))
                                         (map #(vector (resolve-attribute %1 ns-ctx) %2)
                                              (keys attrs) (vals attrs))))
                        content)
              {:clojure.data.xml/ns-ctx ns-ctx})
            (Element. (resolve-tag tag ns-ctx)
                      (into {} (map #(vector (resolve-attribute %1 ns-ctx) %2)
                                    (keys attrs) (vals attrs)))
                      content))
          node)))))

(defn- get-xmlns-updates [{:keys [tag attrs content]} ns-ctx]
  (let [{:keys [nss default] just-attrs :attrs} (parse-attrs attrs)
        ;; amend ns-ctx and keep track of added prefixes
        ;; add a "" prefix, when encountering a non-repeated xmlns=
        ns-info (if (and default (not= (uri-from-prefix ns-ctx "")
                                       default))
                  {:ns-ctx* (assoc-prefix ns-ctx "" default)
                   :xmlns* {"" default}
                   :just-attrs just-attrs}
                  {:ns-ctx* ns-ctx
                   :xmlns* {}
                   :just-attrs just-attrs})]
    (reduce-kv (fn [{:keys [ns-ctx* xmlns*] :as res} prefix uri]
                 (if (prefix-from-uri ns-ctx* uri)
                   res
                   (assoc res
                     :ns-ctx* (assoc-prefix ns-ctx* prefix uri)
                     :xmlns*  (assoc xmlns* prefix uri))))
               ns-info nss)))

(defn- emit-xmlns [attrs* prefix name]
  (if (= "" prefix)
    ;; we can get away with hardcoding the xmlns
    ;; attribute here, because it is not rebindable.
    (assoc! attrs* :xmlns name)
    (assoc! attrs* (keyword "xmlns" prefix))))

(defn walk-cleanup-xmlns
  "Removes redundant prefixes (alias prefixes, repeated xmlns= attrs)
   from an xml-tree"
  ([xml] (walk-cleanup-xmlns xml empty-namespace))
  ([xml ns-ctx]
     (element-walk xml ns-ctx
                   (fn [{:keys [tag attrs content] :as node} ns-ctx]
                     ;; calculate new namespace context and added xmlns*
                     (let [{:keys [ns-ctx* xmlns* just-attrs]}
                           (get-xmlns-updates node ns-ctx)]
                       (Element. tag (as-> (transient {}) attrs*
                                           (reduce-kv emit-xmlns attrs* xmlns*)
                                           (reduce-kv assoc! attrs* just-attrs)
                                           (persistent! attrs*))
                                 content))))))

(defn- to-kw [n ns-ctx]
  (if (keyword? n) n
      (let [{:keys [name prefix uri] :as i} (resolve-tag! n ns-ctx)]
        (if (= default-ns-prefix prefix)
          (keyword name)
          (keyword prefix name)))))

(defn walk-emit-prefixes
  ([xml ns-ctx]
     (element-walk xml ns-ctx
                   (fn [{:keys [tag attrs content] :as node} ns-ctx]
                     (Element. (to-kw tag ns-ctx)
                               (when attrs
                                 (persistent!
                                  (reduce-kv (fn [ta name val]
                                               (assoc! ta (to-kw name ns-ctx) val))
                                             (transient {}) attrs)))
                               content)))))
