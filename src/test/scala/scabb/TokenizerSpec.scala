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