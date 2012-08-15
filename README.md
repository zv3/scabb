Scala BB Code Parser
====================

A simple parser for bb codes in scala.

Supported tags:

    [i]text[/i] => <i>text</i> 
    [b]text[/b] => <b>text</b> 
    [u]text[/u] => <u>text</u> 
    [s]text[/s] => <del>text</del> 
    [q]text[/q] => <blockquote>text</blockquote>

    [code]text[/code]      => <code style="white-space: pre;">text</code>
    [code=lang]text[/code] => <pre><code class="lang">text</code></pre>

    [url]http://link[/url]      => <a href="http://link">http://link</a>
    [url=http://link]text[/url] => <a href="http://link">text</a>

    [color=red]text[/color] => <span style="color: red;">text</span>
    [size=15]text[/size]    => <span style="font-size: 15px;">text</span>

