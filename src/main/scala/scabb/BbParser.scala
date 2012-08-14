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
    override def toHtml =
      Elem(null, htmlName.getOrElse(name), Null, xml.TopScope, contents.flatMap(_.toHtml): _*)
  }

  case class RawText(contents: String) extends BbTag {
    override def toHtml = Text(contents)
  }

  def toHtml(input: String): NodeSeq = new BbParsers().parseString(input).toHtml

  class BbParsers extends RegexParsers with ImplicitConversions {

    def parseString(input: String): BbTag =
      parseAll(bbCode, input).getOrElse(RawText(input))

    def openTag(name: String) = "[" ~> name <~ "]"
    def closedTag(name: String) = "[/" ~> name <~ "]"
    def simpleTag(name: String, htmlName: Option[String]): Parser[BbTag] =
      openTag(name) ~> bbCode <~ closedTag(name) ^^
        { case innerCode => SimpleTag(name, htmlName, innerCode :: Nil) }

    lazy val bbCode: Parser[BbTag] = italic | bold | underline | striked | raw

    lazy val italic = simpleTag("i", None)
    lazy val bold = simpleTag("b", None)
    lazy val underline = simpleTag("u", None)
    lazy val striked = simpleTag("s", Some("del"))
    lazy val raw = rep(not("[/") ~> justChar) ^^
      { case chars => RawText(chars.mkString("")) }
    lazy val justChar: Parser[Char] = elem("", c => true)

  }
}