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