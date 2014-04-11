(ns clojure.data.xml.node)

; Parsed data format
;; Represents a node of an XML tree
(defrecord Element [tag attrs content])
(defrecord CData [content])
(defrecord Comment [content])

(defn element*
  ([tag] (->Element tag {} nil))
  ([tag attrs] (->Element tag (or attrs {}) ()))
  ([tag attrs content] (->Element tag (or attrs {}) (remove nil? content))))

(defn element
  ([tag] (element* tag))
  ([tag attrs] (element* tag attrs))
  ([tag attrs & content] (element* tag attrs content)))

(defn cdata [content]
  (->CData content))

(defn xml-comment [content]
  (->Comment content))
