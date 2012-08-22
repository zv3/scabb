/*
 * Copyright 2012 Roman Kashitsyn
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package scabb

import org.specs2.mutable._

import scabb.BbParser._
import scala.xml._

class BbSpec extends Specification {

import BbParser._

  "The BB parser" should {

    "understand simple formatted text" in {
      toHtml("one thing\nanother thing") must ==/(
          Text("one thing") ++ <br/> ++ Text("another thing")).ordered
    }

    "deal with text in bold face" in {
      toHtml("[b]bold text[/b]") must ==/(<b>bold text</b>)
    }

    "deal with italic text" in {
      toHtml("[i]italic text[/i]") must ==/(<i>italic text</i>)
    }

    "deal with underlined text" in {
      toHtml("[u]underlined text[/u]") must ==/(<u>underlined text</u>)
    }

    "deal with striked text" in {
      toHtml("[s]striked text[/s]") must ==/(<del>striked text</del>)
    }

    "deal with different text decorations interchanged" in {
      toHtml("[b][i][u][s]overdecorated[/s][/u][/i][/b]") must ==/(
          <b><i><u><del>overdecorated</del></u></i></b>)
      toHtml("[b]some [i]italic[/i] and [s]deleted[/s] text[/b]") must ==/(
          <b>some <i>italic</i> and <del>deleted</del> text</b>)
    }

    "deal with colored blocks" in {
      toHtml("[color=red]red text[/color]") must ==/(<span style="color: red;">red text</span>)
      toHtml("[color=#eee]grey[/color]") must ==/(<span style="color: #eee;">grey</span>)
      toHtml("[color=#0000ff]blue[/color]") must ==/(<span style="color: #0000ff;">blue</span>)
    }

    "deal with size changes" in {
      toHtml("[size=20]big[/size]") must ==/(<span style="font-size: 20px;">big</span>)
    }

    "deal with code blocks" in {
      toHtml("[code]val x = 5[/code]") must ==/(
          <code style="white-space: pre;">val x = 5</code>)
      toHtml("[code=haskell]main = getLine >> putStrLn[/code]") must ==/(
            <code class="haskell" style="white-space: pre;">main = getLine >> putStrLn</code>)
      toHtml("[code]val x = 5\nval y = 6[/code]") must ==/(
          <pre><code>{"val x = 5\nval y = 6"}</code></pre>)
    }

    "deal with links" in {
      toHtml("[url]http://google.com[/url]") must ==/(
          <a href="http://google.com">http://google.com</a>)
      toHtml("[url=http://google.com]Google[/url]") must ==/(
          <a href="http://google.com">Google</a>)
      toHtml("[url=http://google.com][b]inner markup[/b][/url]") must ==/(
          <a href="http://google.com"><b>inner markup</b></a>)
    }

    "deal with quotes" in {
      toHtml("[q]a quote[/q]") must ==/(<blockquote>a quote</blockquote>)
    }

    "not pass raw html" in {
      val contents = "<script>alert('xss');</script>"
      toHtml(contents) must ==/(Text(contents))
    }

    "not pass javascript links" in {
      val link = "javascript://alerts('xss')"
      toHtml("[url]" + link + "[/url]") must ==/(Text("[url]") ++ Text(link) ++ Text("[/url]"))
      toHtml("[url=" + link + "]xss[/url]") must ==/(
          Text("[url=" + link + "]") ++ Text("xss") ++ Text("[/url]"))
    }

    "escape urls in links" in {
      toHtml("[url=http://test?x=0&y=1&z=2]test[/url]") must ==/(
          <a href="http://test?x=0&amp;y=1&amp;z=2">test</a>)
      toHtml("[url=http://test?x=0&y=1][b]link[/b][/url]") must ==/(
          <a href="http://test?x=0&amp;y=1"><b>link</b></a>)
    }

    "not pass invalid text sizes" in {
      toHtml("[size=none]text[/size]") must ==/(
          Text("[size=none]") ++ Text("text") ++ Text("[/size]"))
      toHtml("[size=-3]text[/size]") must ==/(
          Text("[size=-3]") ++ Text("text") ++ Text("[/size]"))
    }

    "not pass with invalid colors" in {
      toHtml("[color]none[/color]") must ==/(Text("[color]") ++ Text("none") ++ Text("[/color]"))
      toHtml("[color=#4444]four[/color]") must ==/(Text("[color=#4444]") ++ Text("four") ++ Text("[/color]"))
      toHtml("[color=#hhh]h[/color]") must ==/(Text("[color=#hhh]") ++ Text("h") ++ Text("[/color]"))
      toHtml("[color][i]inner[/i][/color]") must ==/(Text("[color]") ++ <i>inner</i> ++ Text("[/color]"))
    }

    "work greacefully with unclosed tags" in {
      toHtml("[b]closed [/b] or not [i]") must ==/(<b>closed </b> ++ Text(" or not ") ++ Text("[i]"))
      toHtml("[i] not [b]closed [/b]") must ==/(Text("[i]") ++ Text(" not ") ++ <b>closed </b>)
    }
  }
}