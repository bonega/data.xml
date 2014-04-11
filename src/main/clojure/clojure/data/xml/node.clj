(ns clojure.data.xml.node)

; Parsed data format
;; Represents a node of an XML tree
(defrecord Element [tag attrs content])
(defrecord CData [content])
(defrecord Comment [content])

(definline element* 
  ([tag] (->Element tag {} nil))
  ([tag attrs] (->Element tag (or attrs {}) ()))
  ([tag attrs content] (->Element tag (or attrs {}) (remove nil? content))))

(definline element
  ([tag] (content* tag))
  ([tag attrs] (content* tag attrs))
  ([tag attrs & content] (content* tag attrs content)))

(definline cdata [content]
  (->CData content))

(definline xml-comment [content]
  (->Comment content))
