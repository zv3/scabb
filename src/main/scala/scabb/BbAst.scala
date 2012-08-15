package scabb

import scala.xml._

abstract class BbAst {
  def toHtml: NodeSeq

  private[scabb] def mkAttr(key: String, value: String) =
    Attribute(key, new Atom(value), Null)
}

case class SimpleTag(name: String, htmlName: Option[String], contents: List[BbAst]) extends BbAst {
  override val toHtml =
    Elem(null, htmlName.getOrElse(name), Null, xml.TopScope, contents.flatMap(_.toHtml): _*)
}

case class CodeTag(value: Option[String], contents: RawText) extends BbAst {
  override val toHtml = {
    val classVal = value.map(mkAttr("class", _)).getOrElse(Null)
    Elem(null, "code", classVal, xml.TopScope, contents.toHtml)
  }
}

case class LinkTag(value: Option[String], inner: FormattedText) extends BbAst {
  override val toHtml = {
    val href = mkAttr("href", value.getOrElse(inner.contents))
    Elem(null, "a", href, xml.TopScope, inner.toHtml: _*)
  }
}

case class FormattedText(contents: String) extends BbAst {
  override val toHtml = {
    val buffer = new collection.mutable.ListBuffer[Node]

    def appendNode(node: Node) = {
      if (!buffer.isEmpty) buffer += <br/>
      buffer += node
    }

    def newLineToLineBreak(offset: Int): NodeSeq = {
      val newLineIndex = contents.indexOf('\n', offset)
      if (newLineIndex > 0) {
        appendNode(Text(contents.substring(offset, newLineIndex - offset)))
        newLineToLineBreak(newLineIndex + 1)
      } else {
        appendNode(Text(contents.substring(offset)))
        buffer.toSeq
      }
    }
    newLineToLineBreak(0)
  }
}

case class RawText(contents: String) extends BbAst {
  override def toHtml = Text(contents)
}