package scabb

import org.specs2.mutable._

class BbNodeSpec extends Specification {

  import BbLexer._

  "BbAstParser" should {
    "work with simple tag sequence" in {
      toNodes("[b]bold[/b]") must_== List(OpenTag("b", None), RawText("bold"), CloseTag("b"))
    }

    "work with nested tag sequence" in {
      toNodes("[u][b]bold[/b] and [i]italic[/i][/u]") must_==
        List(OpenTag("u", None), OpenTag("b", None), RawText("bold"), CloseTag("b"),
            RawText(" and "), OpenTag("i", None), RawText("italic"), CloseTag("i"), CloseTag("u"))
    }

    "work with attributed tags" in {
      toNodes("[code=php]$x = 5;[/code]") must_==
        List(OpenTag("code", Some("php")), RawText("$x = 5;"), CloseTag("code"))
    }

    "work with URI as attribute" in {
      toNodes("[url=http://www.google.com]Google[/url]") must_==
        List(OpenTag("url", Some("http://www.google.com")), RawText("Google"), CloseTag("url"))
    }
  }

}