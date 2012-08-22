Scala BB Code Parser
====================

A simple parser for bb codes in scala.

Supported tags:
---------------

    [i]text[/i] => <i>text</i> 
    [b]text[/b] => <b>text</b> 
    [u]text[/u] => <u>text</u> 
    [s]text[/s] => <del>text</del> 
    [q]text[/q] => <blockquote>text</blockquote>

    [code]inline[/code]    => <code style="white-space: pre;">inline</code>
    [code=lang]multiline
    code[/code]            =>
    <pre><code class="lang">multiline
    code</code></pre>

    [url]http://link[/url]      => <a href="http://link">http://link</a>
    [url=http://link]text[/url] => <a href="http://link">text</a>

    [color=red]text[/color]  => <span style="color: red;">text</span>
    [color=#eee]text[/color] => <span style="color: #eee;">text</span>
    [size=15]text[/size]    => <span style="font-size: 15px;">text</span>

Basic usage
-----------

It is extremely easy to use BB-parser. Just call the `toHtml` method
with string input:

    import org.scabb.BbParser._
    // will print <b><i>Hello there</i></b>
    println( toHtml("[b][i]Hello there[/i][/b]") )

`toHtml` method returns an instance of `scala.xml.NodeSeq`, so you can
traverse and transform it's output as you like using standard scala
features.

Extensions
----------

`BbParser` object is actually an instance of trait `ExtensibleBbParser`
which is highly extensible by it's nature.

For example, you can subclass the trait and specify your own input
tokenizer (which must be an instance of trait `Tokenizer`) or provide
some additional tags mappings via scala's partial functions.

For more details please see `ExtensibleParserSpec` class source code which
contains specifications for different ways of parser extensions with
examples.

LICENSE
-------

The software is distributed under Apache 2.0 Software License.
