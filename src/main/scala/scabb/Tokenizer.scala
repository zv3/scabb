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

import scala.annotation.tailrec

trait Tokenizer {
  def tokenize(input: String): List[BbNode]
}

object Tokenizer extends Tokenizer {

  private val OpenTagRegex = """([a-z0-9]+)(=(\S+))?""".r
  private val ClosedTagRegex = """/([a-z0-9]+)""".r

  private def nullToNone[T](obj: T) = if (obj == null) None else Option(obj)

  def tokenize(input: String): List[BbNode] = {

    @tailrec
    def tryToReduce(tokens: List[BbNode], contents: List[BbNode], tag: ClosedTag): Option[List[BbNode]] = {
      if (tokens.isEmpty) None
      else tokens.head match {
        case OpenedTag(name, attr) if (name == tag.name) => Some(TagNode(name, attr, contents) :: tokens.tail)
        case node => tryToReduce(tokens.tail, node :: contents, tag)
      }
    }

    def reduce(tokens: List[BbNode], tag: ClosedTag): List[BbNode] =
      tryToReduce(tokens, Nil, tag).getOrElse(tag :: tokens)

    @tailrec
    def moveToken(offset: Int, state: List[BbNode]): List[BbNode] = {
      input.indexOf('[', offset) match {
        case -1 => if (offset == input.length) state else TextNode(input.substring(offset)) :: state
        case open if open > offset => moveToken(open, TextNode(input.substring(offset, open)) :: state)
        case open => input.indexOf(']', open) match {
          case -1 => TextNode(input.substring(offset)) :: state
          case closed => { input.substring(open + 1, closed) match {
            case OpenTagRegex(name, _, attr) => moveToken(closed + 1, OpenedTag(name, nullToNone(attr)) :: state)
            case ClosedTagRegex(name) => moveToken(closed + 1, reduce(state, ClosedTag(name)))
            case text => moveToken(closed + 1, TextNode(input.substring(offset, closed + 1)) :: state)}
          }
        }
      }
    }

    moveToken(0, Nil).reverse
  }
}