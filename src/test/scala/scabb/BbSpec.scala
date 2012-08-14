package scabb

import org.specs2.mutable._

class BbSpec extends Specification {
  "The BB parser" should {
    
    "deal with text in bold face" in {
      BbParser.toHtml("[b]bold text[/b]") must be_==(<b>bold text</b>)
    }
    
    "deal with italic text" in {
      BbParser.toHtml("[i]italic text[/i]") must be_==(<i>italic text</i>)
    }
    
    "deal with underlined text" in {
      BbParser.toHtml("[u]underlined text[/u]") must be_==(<u>underlined text</u>)
    }
    
    "deal with striked text" in {
      BbParser.toHtml("[s]striked text[/s]") must be_==(<del>striked text</del>)
    }
    
    "deal with different text decorations interchanged" in {
      BbParser.toHtml("[b][i][u][s]overdecorated[/s][/u][/i][/b]") must be_==(
          <b><i><u><del>overdecorated</del></u></i></b>)
    }
    
    "deal with code blocks" in {
      BbParser.toHtml("[code]val x = 5[/code]") must be_==(
          <pre><code>val x = 5</code></pre>)
      BbParser.toHtml("[code=haskell]main = getLine >> putStrLn[/code]") must be_==(
            <pre class="haskell"><code class="haskell">main = getLine >> putStrLn</code></pre>)
    }
  }


}