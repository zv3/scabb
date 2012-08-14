package scabb

import scala.xml._
import scala.util.parsing.combinator._

trait BbParser {
  def toHtml(input: String): NodeSeq
}

object BbParser extends BbParser {

  abstract class BbTag {
    def toHtml: NodeSeq
  }

  case class SimpleTag(name: String, htmlName: Option[String], contents: List[BbTag]) extends BbTag {
    override def toHtml = {
      Elem(null, htmlName.getOrElse(name), Null, xml.TopScope, contents.flatMap(_.toHtml): _*)
    }
  }

  case class AttributedTag(name: String, htmlName: Option[String], contents: List[BbTag])

  case class RawText(contents: String) extends BbTag {
    override def toHtml = Text(contents)
  }

  def toHtml(input: String): NodeSeq = new BbParsers().parseString(input).toHtml

  class BbParsers extends RegexParsers with ImplicitConversions {

    lazy val attrChars: Parser[String] = "[^]]+".r
    lazy val tagChars: Parser[String]  = "[a-z]+".r

    def parseString(input: String): BbTag =
      parseAll(bbCode, input).getOrElse(RawText(input))

    lazy val openedTag: Parser[BbNode] = '[' ~> (tagChars ~ opt('=' ~> attrChars)) <~ ']' ^^
    { case tagName ~ maybeVal => OpenTag(tagName, maybeVal) }

    lazy val closedTag: Parser[BbNode] = '[' ~> '/' ~> tagChars <~ ']' ^^ CloseTag

    def startTag(name: String) = '[' ~> name <~ ']'
    def closeTag(name: String) = '[' ~> '/' ~> name <~ ']'

    def simpleTag(name: String, htmlName: Option[String]): Parser[BbTag] =
      startTag(name) ~> rep1(bbCode) <~ closeTag(name) ^^
        { case innerCode => SimpleTag(name, htmlName, innerCode) }

    lazy val bbCode: Parser[BbTag] = italic | bold | underline | striked | quote | raw

    lazy val italic = simpleTag("i", None)
    lazy val bold = simpleTag("b", None)
    lazy val underline = simpleTag("u", None)
    lazy val striked = simpleTag("s", Some("del"))
    lazy val quote = simpleTag("q", Some("blockquote"))
    lazy val raw = rep1(justChar) ^^
      { case chars => RawText(chars.mkString("")) }
    lazy val justChar: Parser[Char] = elem("", c => c != '['  && !Character.isISOControl(c) )
  }
}