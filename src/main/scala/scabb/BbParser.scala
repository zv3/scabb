package scabb

import scala.xml._

trait BbParser {
  def toHtml(input: String): NodeSeq
}

object BbParser extends BbParser {

  def toHtml(input: String): NodeSeq = {
    val tokens = Tokenizer.tokenize(input)
    tokens.map(toAst).flatMap(_.toHtml)
  }

  private[scabb] def mkAttr(key: String, value: String, other: MetaData) =
    Attribute(key, new Atom(value), other)

  def simpleTags = Set("i", "b", "u")

  private def toAst(node: BbNode): BbAst = node match {
    case TextNode(txt) => FormattedText(txt)
    case TagNode("code", attr, children) => CodeTag(attr, RawText(children.mkString("")))
    case TagNode("url", attr, children) => LinkTag(attr, FormattedText(children.mkString("")))
    case TagNode(t, None, children) if simpleTags.contains(t) => SimpleTag(t, None, children.map(toAst))
    case TagNode("s", None, children) => SimpleTag("s", Some("del"), children.map(toAst))
    case TagNode("q", None, children) => SimpleTag("q", Some("blockquote"), children.map(toAst))
    case TagNode("size", Some(attr), children) => SizeTag(attr, children.map(toAst))
    case TagNode("color", Some(attr), children) => ColorTag(attr, children.map(toAst))
    case _ => FormattedText(node.toString)
  }
}