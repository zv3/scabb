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

class TokenizerSpec extends Specification {

import Tokenizer._

  "Tokenizer" should {
    "work with simple tag sequence" in {
      tokenize("[b]bold[/b]") must_== TagNode("b", None, TextNode("bold") :: Nil) :: Nil
    }

    "work with nested tag sequence" in {
      tokenize("[u][b]bold[/b] and [i]italic[/i][/u]") must_==
        List(TagNode("u", None, List(TagNode("b", None, TextNode("bold") :: Nil),
            TextNode(" and "), TagNode("i", None, TextNode("italic") :: Nil))))
    }

    "work with attributed tags" in {
      tokenize("[code=php]$x = 5;[/code]") must_==
        List(TagNode("code", Some("php"), TextNode("$x = 5;") :: Nil))
    }

    "work with URI as attribute" in {
      tokenize("[url=http://www.google.com]Google[/url]") must_==
        List(TagNode("url", Some("http://www.google.com"), TextNode("Google") :: Nil))
    }

    "work gracefully with unclosed tags" in {
      tokenize("[i] text [b]some text[/b]") must_== List(OpenedTag("i", None), TextNode(" text "),
          TagNode("b", None, TextNode("some text") :: Nil))
      tokenize("[b]x=y[i][/b]") must_== List(TagNode("b", None, TextNode("x=y") :: OpenedTag("i", None) :: Nil))
    }
  }

}