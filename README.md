# data.xml

[data.xml](https://github.com/clojure/data.xml) is a Clojure library for reading and writing XML data. This
library is the successor to
[lazy-xml](http://clojure.github.com/clojure-contrib/lazy-xml-api.html).
data.xml has the following features:

* Parses XML documents into Clojure data structures
* Emits XML from Clojure data structures
* No additional dependencies if using 1.6
* Uses StAX internally
* lazy - should allow parsing and emitting of large XML documents
* Can handle namespaced xml utilizing clojure namespaces

## JDK 1.5

This library uses the pull parser that ships with JDK 1.6.  If you running on JDK 1.6+, you do not need any 
additional dependencies.  If you are using JDK 1.5, you will need to include a dependency on StAX.  More 
information on this is available [here](https://github.com/clojure/data.xml/blob/jdk16-pull-parser/jdk_15_readme.txt)

## Bugs

Please report bugs using JIRA [here](http://dev.clojure.org/jira/browse/DXML).

## Installation

Latest stable release: 0.0.8

* [All Released Versions](http://search.maven.org/#search%7Cgav%7C1%7Cg%3A%22org.clojure%22%20AND%20a%3A%22data.xml%22)

* [Development Snapshot Versions](https://oss.sonatype.org/index.html#nexus-search;gav~org.clojure~data.xml~~~)

### Maven
For Maven projects, add the following XML in your `pom.xml`'s `<dependencies>` section:

    <dependency>
      <groupId>org.clojure</groupId>
      <artifactId>data.xml</artifactId>
      <version>0.0.8</version>
     </dependency>

### Leiningen
Add the following to the `project.clj` dependencies:

    [org.clojure/data.xml "0.0.8"]

## Examples

The examples below assume you have added a `use` for data.xml:

    > (use 'clojure.data.xml)

As well as the following `data_readers.clj` on your classpath:

    {xml/name clojure.data.xml/qname
     xml/element clojure.data.xml/element}

data.xml supports parsing and emitting XML. The parsing functions will
read XML from a
[Reader](http://docs.oracle.com/javase/6/docs/api/java/io/Reader.html)
or
[InputStream](http://docs.oracle.com/javase/6/docs/api/java/io/InputStream.html).

    > (let [input-xml (java.io.StringReader. "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
                                            <foo><bar><baz>The baz value</baz></bar></foo>")]
        (parse input-xml))

    #xml/element{:tag :foo,
                 :attrs {},
                 :content (#xml/element{:tag :bar, 
                                        :attrs {},
                                        :content (#xml/emlement{:tag :baz,
                                        :attrs {},
                                        :content ("The baz value")})})}

The data is returned as defrecords and can be manipulated using the
normal clojure data structure functions. Additional parsing options 
can be passed via key pairs:

    > (parse-str "<a><![CDATA[\nfoo bar\n]]><![CDATA[\nbaz\n]]></a>" :coalescing false)
    #xml/element{:tag :a, :attrs {}, :content ("\nfoo bar\n" "\nbaz\n")}
    
XML elements can be created using the typical defrecord constructor
functions or the element function used below, and written using a
[java.io.Writer](http://docs.oracle.com/javase/6/docs/api/java/io/Writer.html).:

    (let [tags (element :foo {:foo-attr "foo value"}
                 (element :bar {:bar-attr "bar value"}
                   (element :baz {} "The baz value")))]
      (with-open [out-file (java.io.FileWriter. "/tmp/foo.xml")]
        (emit tags out-file)))

    ;;-> Writes XML to /tmp/foo.xml

The same can also be expressed using a more Hiccup-like style of defining the elements using sexp-as-element:

    (= (element :foo {:foo-attr "foo value"}
         (element :bar {:bar-attr "bar value"}
           (element :baz {} "The baz value")))
       (sexp-as-element
          [:foo {:foo-attr "foo value"}
           [:bar {:bar-attr "bar value"}
            [:baz {} "The baz value"]]]))
    ;;-> true

Comments and CDATA can also be emitted as an S-expression with the special tag names :-cdata and :-comment:

    (= (element :tag {:attr "value"}
         (element :body {} (cdata "not parsed <stuff")))
       (sexp-as-element [:tag {:attr "value"} [:body {} [:-cdata "not parsed <stuff"]]]
    ;;-> true       

XML can be "round tripped" through the library:

    (let [tags (element :foo {:foo-attr "foo value"}
                 (element :bar {:bar-attr "bar value"}
                   (element :baz {} "The baz value")))]
      (with-open [out-file (java.io.FileWriter. "/tmp/foo.xml")]
        (emit tags out-file))
      (with-open [input (java.io.FileInputStream. "/tmp/foo.xml")]
        (parse input)))

    #xml/element{:tag :foo, :attrs {:foo-attr "foo value"}...}

There are also some string based functions that are useful for
debugging.

    (let [tags (element :foo {:foo-attr "foo value"}
                 (element :bar {:bar-attr "bar value"}
                   (element :baz {} "The baz value")))]
      (= tags (parse-str (emit-str tags))))

    true  
    
Indentation is supported, but should be treated as a debugging feature
as it's likely to be pretty slow:

    (print (indent-str (element :foo {:foo-attr "foo value"}
                         (element :bar {:bar-attr "bar value"}
                           (element :baz {} "The baz value1")
                           (element :baz {} "The baz value2")
                           (element :baz {} "The baz value3")))))

    <?xml version="1.0" encoding="UTF-8"?>
    <foo foo-attr="foo value">
      <bar bar-attr="bar value">
        <baz>The baz value1</baz>
        <baz>The baz value2</baz>
        <baz>The baz value3</baz>
      </bar>
    </foo>

CDATA can be emitted:

    (emit-str (element :foo {}
                (cdata "<non><escaped><info><here>")))

    "<?xml version=\"1.0\" encoding=\"UTF-8\"?><foo><![CDATA[<non><escaped><info><here>]]></foo>"

But will be read as regular character data:

    (parse-str (emit-str (element :foo {}
                 (cdata "<non><escaped><info><here>"))))

    #xml/element{:tag :foo, :attrs {}, :content ("<non><escaped><info><here>")}

Comments can also be emitted:

    (emit-str (element :foo {}
                (xml-comment "Just a <comment> goes here")
                (element :bar {} "and another element")))

    "<?xml version=\"1.0\" encoding=\"UTF-8\"?><foo><!--Just a <comment> goes here--><bar>and another element</bar></foo>"

But are ignored when read:

    (emit-str
      (parse-str
        (emit-str (element :foo {}
                    (xml-comment "Just a <comment> goes here")
                    (element :bar {} "and another element")))))

    "<?xml version=\"1.0\" encoding=\"UTF-8\"?><foo><bar>and another element</bar></foo>"    

Generated API docs for data.xml are available [here](http://clojure.github.com/data.xml).

## Namespaced XML

XML can be parsed in raw or resolved mode. Their output is
incompatible, when parsing namespaced XML.

## Raw mode

In raw mode, XML names are parsed as simple keywords. When namespaced
XML is parsed in this mode, prefixed XML names are represented as
namespaced keywords and xmlns clauses are present as attributes.

    > (parse-str-raw "<P:foo xmlns:P=\"uri:\" />")
    {:tag :P/foo
     :attrs {:xmlns/P "urn:"}}

## Resolved mode

In resolved mode, XML names are only parsed as keywords, if they are
in the empty namespace, as often happens with attributes. Otherwise
they are represented as `javax.xml.namespace.QName`. Xmlns attributes
are normally stripped.

    > (parse-str "<P:foo xmlns:P=\"uri:\" />")
    {:tag #xml/name {:name "foo" :uri "uri:"}
     :attrs {}}

## Defns clauses

Even with the `#xml/name` reader tag, qualified names can be
cumbersome to type out for emitting or comparing to parsed
XML. Therefore, `defns` clauses can be used to specify mappings
between clojure namespaces and xml namespaces.

    > (ns webdav.xml (:require [data.xml :refer [defns parse-str]]))
    > (defns "DAV:" custom "CUSTOM:")
	> (parse-str "<D:props xmlns:D=\"DAV:\" xmlns:C="CUSTOM:" C:attr=\"val\" />")
    {:tag ::props
     :attrs {::custom:attr "val"}}

`alias-ns` is used to introduce clojure ns aliases. This is similar to
`clojure.core/alias` but also creates the target namespace.

    > (ns user (:require [data.xml :refer [parse-str* emit-str alias-ns]]))
    > (alias-ns dav webdav.xml)
	> (parse-str* webdav.xml "<D:props xmlns:D=\"DAV:\" xmlns:C="CUSTOM:" C:attr=\"val\" />")
    {:tag ::dav/props
     :attrs {::dav/custom:attr "val"}}
    > (emit-str *1)
    "<D:props xmlns:D=\"DAV:\" xmlns:C="CUSTOM:" C:attr=\"val\" />"

## Emitter handling of xmlns clauses and prefixes

The emitter needs to insert xmlns prefixes for namespaced xml. It does
not try to assign the prefixes itself, but requires the user to insert
appropriate xmlns attributes into the attributes.

Roundtripping in resolved mode works because the parser stores the
namespace environment in a tag's metadata. Hence when working with
untrusted XML, it's advisable to parse with `:xmlns-meta false`, such
that the emitter will serve as a last line of defense against
exposing clients to unforseen content.

## License

Licensed under the [Eclipse Public License](http://www.opensource.org/licenses/eclipse-1.0.php).

## Developer Information

* [GitHub project](https://github.com/clojure/data.xml)

* [Bug Tracker](http://dev.clojure.org/jira/browse/DXML)

* [Continuous Integration](http://build.clojure.org/job/data.xml/)

* [Compatibility Test Matrix](http://build.clojure.org/job/data.xml-test-matrix/)

## Contributing

All contributions need to be made via patches attached to tickets in
[JIRA](http://dev.clojure.org/jira/browse/DXML). Check the
[Contributing to Clojure](http://clojure.org/contributing) page for
more information.


