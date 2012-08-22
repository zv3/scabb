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

abstract class BbAst {
  /**
   * Generates xml nodes which correspond to the AST node.
   */
  def toHtml: NodeSeq

  private[scabb] def mkAttr(key: String, value: String) =
    Attribute(key, new Atom(value), Null)
  private[scabb] def mkAttr(key: String, value: String, other: MetaData) =
    Attribute(key, new Atom(value), other)

}

case class SimpleTag(name: String, htmlName: Option[String], contents: List[BbAst]) extends BbAst {
  override val toHtml =
    Elem(null, htmlName.getOrElse(name), Null, TopScope, contents.flatMap(_.toHtml): _*)
}

case class CodeTag(value: Option[String], contents: RawText) extends BbAst {
  override val toHtml = {
    val multiline = contents.contents.contains('\n')
    val classAttr = value.map(mkAttr("class", _)).getOrElse(Null)
    val meta = if (multiline) classAttr else mkAttr("style", "white-space: pre;", classAttr)
    val code = Elem(null, "code", meta, TopScope, contents.toHtml)
    if (multiline) Elem(null, "pre", Null, TopScope, code: _*) else code
  }
}

case class ColorTag(value: Option[String], contents: List[BbAst]) extends BbAst {
  val ColorRegex = """^([a-z]+|#[0-9a-f]{3}|#[0-9a-f]{6})$""".r

  override val toHtml = {
    value match {
      case Some(ColorRegex(color)) => {
        val style = mkAttr("style", "color: " + color + ";")
        Elem(null, "span", style, TopScope, contents.flatMap(_.toHtml): _*)
      }
      case _ => Text("[color" + value.map("=" + _).getOrElse("") + "]") ++
        contents.flatMap(_.toHtml) ++ Text("[/color]")
    }
  }
}

case class SizeTag(value: String, contents: List[BbAst]) extends BbAst {
  override val toHtml = {
    val size = try { value.toLong } catch { case _ => -1L }
    if (size < 5 || size > 30) {
      Text("[size=" + value + "]") ++ contents.flatMap(_.toHtml) ++ Text("[/size]")
    } else {
      val style = mkAttr("style", "font-size: " + value + "px;")
      Elem(null, "span", style, TopScope, contents.flatMap(_.toHtml): _*)
    }
  }
}

case class LinkTag(value: Option[String], inner: List[BbAst]) extends BbAst {

  val mkLinkFromContents: Option[String] = inner match {
    case FormattedText(contents) :: Nil => Some(contents)
    case RawText(contents) :: Nil => Some(contents)
    case _ => None
  }

  override val toHtml = {
    val link = value.orElse(mkLinkFromContents)
    link.map { href =>
      if (href.toLowerCase startsWith "javascript://") {
        Text("[url" + value.map("=" + _).getOrElse("") + "]") ++
          inner.flatMap(_.toHtml) ++ Text("[/url]")
      } else {
        val hrefAttr = mkAttr("href", href)
        Elem(null, "a", hrefAttr, TopScope, inner.flatMap(_.toHtml): _*)
      }
    } getOrElse {
      Text("[url]") ++ inner.flatMap(_.toHtml) ++ Text("[/url]")
    }

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
        appendNode(Text(contents.substring(offset, newLineIndex)))
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