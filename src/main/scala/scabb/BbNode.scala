package scabb

sealed abstract class BbNode

case class OpenedTag(name: String, value: Option[String]) extends BbNode {
  override def toString = "[" + name + value.map("=" + _).getOrElse("") + "]"
}
case class ClosedTag(name: String) extends BbNode {
  override def toString = "[/" + name + "]"
}
case class TextNode(text: String) extends BbNode {
  override def toString = text
}
case class TagNode(name: String, value: Option[String], contents: List[BbNode]) extends BbNode {
  override def toString = "[" + name + value.map("=" + _).getOrElse("") + "]" +
    contents.mkString("") + "[/" + name + "]"
}