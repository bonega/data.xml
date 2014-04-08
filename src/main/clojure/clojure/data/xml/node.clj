(ns clojure.data.xml.node)

; Parsed data format
;; Represents a node of an XML tree
(defrecord Element [tag attrs content])
(defrecord CData [content])
(defrecord Comment [content])
