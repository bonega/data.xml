(ns clojure.data.xml.node)

; Parsed data format
;; Represents a node of an XML tree
(defrecord Element [tag attrs content])
(defrecord CData [content])
(defrecord Comment [content])

(defn element [tag & [attrs & content]]
  (->Element tag (or attrs {}) (remove nil? content)))

(defn cdata [content]
  (->CData content))

(defn xml-comment [content]
  (->Comment content))
