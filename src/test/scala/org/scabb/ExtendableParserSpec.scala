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
package org.scabb

import org.specs2.mutable._

class ExtendableParserSpec extends Specification {

  "Bb parser" should {

    "be extendable through new simple tag addition" in {
      object BbParserWithHeader extends ExtendableBbParser {
        override def simpleTags = super.simpleTags ++ Set("h1")
      }
      BbParserWithHeader.toHtml("[h1][i]hello[/i][/h1]") must ==/(
        <h1><i>hello</i></h1>)
    }

    "be extendable throgh new transformations" in {
      object BbParserWithMoreCodeBlocks extends ExtendableBbParser {
        val langs = Set("java", "php")
        override def astExtensions = {
          case TagNode(lang, None, contents) if langs contains lang =>
            CodeTag(Some(lang), RawText(contents.mkString("")))
        }
      }

      import BbParserWithMoreCodeBlocks._
      toHtml("[php]$x = 5;[/php]") must ==/(
          <pre><code class="php">$x = 5;</code></pre>)
      toHtml("[java]int x = 5;[/java]") must ==/(
          <pre><code class="java">int x = 5;</code></pre>)
    }

    "be extensible through trait composition" in {
      trait Headers extends ExtendableBbParser {
        override def astExtensions: Extension = ({
          case TagNode(h, None, contents) if Set("h1", "h2", "h3") contains h =>
            SimpleTag(h, None, contents.map(toAst _))
        }: Extension) orElse super.astExtensions
      }

      trait SupAndSub extends ExtendableBbParser {
        override def astExtensions = ({
          case TagNode(s, None, contents) if Set("sub", "sup") contains s =>
            SimpleTag(s, None, contents.map(toAst))
        }: Extension) orElse super.astExtensions
      }

      object MyBbParser extends ExtendableBbParser with Headers with SupAndSub
      import MyBbParser._

      toHtml("[h1]E = mc[sup]2[/sup][/h1]") must ==/(
          <h1>E = mc<sup>2</sup></h1>)
      toHtml("[h3][b]let [/b]x[sub]i[/sub] = 5[/h3]") must ==/(
          <h3><b>let </b>x<sub>i</sub> = 5</h3>)
    }
  }

}