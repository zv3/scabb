package scabb

sealed abstract class BbNode
case class OpenTag(name: String, value: Option[String]) extends BbNode
case class CloseTag(name: String) extends BbNode
case class RawText(text: String) extends BbNode