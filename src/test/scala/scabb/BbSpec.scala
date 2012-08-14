package scabb

import org.specs2.mutable._

class BbSpec extends Specification {

  import BbParser._

  "The BB parser" should {

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
      toHtml("[b]all bold [i]italic[/i] and [u]underlined[/u][/b]") must ==/(
          <b>all bold <i>italic</i> and <u>underlined</u></b>)
    }

    "deal with quotes" in {
      toHtml("[q]Your phrase[/q]") must ==/(<blockquote>Your phrase</blockquote>)
      toHtml("[q][b]bold[/b] and [i]italic[/i][/q]") must ==/(
          <blockquote><b>bold</b> and <i>italic</i></blockquote>)
    }

    "deal with code blocks" in {
      toHtml("[code]val x = 5[/code]") must ==/(<code>val x = 5</code>)
      toHtml("[code=haskell]main = getLine >> putStrLn[/code]") must ==/(
            <pre class="haskell"><code class="haskell">main = getLine >> putStrLn</code></pre>)
    }
  }


}