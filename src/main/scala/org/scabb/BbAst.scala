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
package org.scabb

import scala.xml._

abstract class BbAst {
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
    val classVal = value.map(mkAttr("class", _)).getOrElse(Null)
    val style = mkAttr("style", "white-space: pre;", classVal)
    Elem(null, "code", style, TopScope, contents.toHtml)
  }
}

case class ColorTag(value: String, contents: List[BbAst]) extends BbAst {
  override val toHtml = {
    val style = mkAttr("style", "color: " + value + ";")
    Elem(null, "span", style, TopScope, contents.flatMap(_.toHtml): _*)
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

case class LinkTag(value: Option[String], inner: FormattedText) extends BbAst {
  override val toHtml = {
    val href = value.getOrElse(inner.contents)
    if (href.toLowerCase startsWith "javascript://") {
      Text("[url" + value.map("=" + _).getOrElse("") + "]") ++ inner.toHtml ++ Text("[/url]")
    } else {
      val hrefAttr = mkAttr("href", href)
      Elem(null, "a", hrefAttr, TopScope, inner.toHtml: _*)
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