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

import scala.xml._
import scala.util.parsing.combinator._

object StrictBbParser extends BbParser {

  def toHtml(input: String): NodeSeq = new BbParsers().parseString(input).toHtml

  class BbParsers extends RegexParsers with ImplicitConversions {

    def parseString(input: String): BbAst =
      parseAll(bbCode, input).getOrElse(FormattedText(input))

    lazy val attrChars: Parser[String] = rep1(notClosedBracket) ^^
      { case chars => chars.mkString("") }

    def startTag(name: String) = '[' ~> name <~ ']'
    def startAttributedTag(name: String) = '[' ~> (name ~ opt('=' ~> attrChars)) <~ ']'
    def startAttributedTag_!(name: String) = '[' ~> name ~ ('=' ~> attrChars) <~ ']'
    def closeTag(name: String) = '[' ~ '/' ~> name <~ ']'

    def simpleTag(name: String, htmlName: Option[String]): Parser[BbAst] =
      startTag(name) ~> rep1(bbCode) <~ closeTag(name) ^^
        { case innerCode => SimpleTag(name, htmlName, innerCode) }

    lazy val bbCode: Parser[BbAst] = italic | bold | underline | striked | url |
      code | color | size | formatted

    // Simple tags
    lazy val italic = simpleTag("i", None)
    lazy val bold = simpleTag("b", None)
    lazy val underline = simpleTag("u", None)
    lazy val striked = simpleTag("s", Some("del"))

    // Text
    lazy val formatted = rep1(notOpenBracket) ^^ { case chars => FormattedText(chars.mkString) }
    lazy val raw = rep1(notCode) ^^ { case chars => RawText(chars.mkString("")) }

    // Attributed tags
    lazy val code = (startAttributedTag("code") ~ raw) <~ closeTag("code") ^^
      { case name ~ maybeAttr ~ rawCode => CodeTag(maybeAttr, rawCode) }
    lazy val url = (startAttributedTag("url") ~ formatted) <~ closeTag("url") ^^
      { case name ~ maybeAttr ~ contents => LinkTag(maybeAttr, contents :: Nil) }
    lazy val color = (startAttributedTag_!("color") ~ rep1(bbCode)) <~ closeTag("color") ^^
      { case name ~ attr ~ inner => ColorTag(Some(attr), inner) }
    lazy val size = (startAttributedTag_!("size") ~ rep1(bbCode) <~ closeTag("size")) ^^
      { case name ~ attr ~ inner => SizeTag(attr, inner) }

    // Chars flavors
    lazy val notOpenBracket: Parser[Char] = elem("simple char", c => c != '[' && !Character.isISOControl(c))
    lazy val notClosedBracket: Parser[Char] = elem("simple char", c => c != ']' && !Character.isISOControl(c))
    lazy val printableChar: Parser[Char] = elem("printable char", !Character.isISOControl(_))
    lazy val notCode: Parser[Char] = not("[/code]") ~> printableChar
  }
}