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