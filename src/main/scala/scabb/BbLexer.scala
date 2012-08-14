package scabb

import scala.util.parsing.combinator._

trait BbLexer {
  def toNodes(input: String): List[BbNode]
}

object BbLexer extends BbLexer {

  override def toNodes(input: String): List[BbNode] = foldTextBlocks(new BbAstParsers().run(input))

  private class BbAstParsers extends RegexParsers with ImplicitConversions {

    def mkString(chars: List[Char]) = chars mkString ""

    lazy val attrChars: Parser[String] = rep1(elem("attribute character", _ != ']')) ^^ mkString
    lazy val tagChars: Parser[String]  = rep1(elem("letter ", Character.isLetter(_))) ^^ mkString

    lazy val openedTag: Parser[BbNode] = '[' ~> (tagChars ~ opt('=' ~> attrChars)) <~ ']' ^^
    { case tagName ~ maybeVal => OpenTag(tagName, maybeVal) }

    lazy val closedTag: Parser[BbNode] = '[' ~> '/' ~> tagChars <~ ']' ^^ CloseTag

    lazy val char: Parser[BbNode] = elem("printable char", !Character.isISOControl(_)) ^^
    { case c => RawText(c.toString) }

    def astNode: Parser[List[BbNode]] = rep(openedTag | closedTag | char)

    def run(input: String): List[BbNode] = parseAll(astNode, input).getOrElse(RawText(input) :: Nil)
  }

  private def foldTextBlocks(ast: List[BbNode]): List[BbNode] = {
    val buffer = new scala.collection.mutable.ListBuffer[BbNode]

      def dw[A](in: List[A])(f: A => Boolean): List[A] = in match {
        case Nil => Nil
        case x :: xs if !f(x) => in
        case x :: xs => dw(xs)(f)
      }

      def rcb(in: List[BbNode]) {
        (in: @unchecked) match {
          case Nil =>
          case RawText(s1) :: RawText(s2) :: _ =>
            val sb = new StringBuilder
            val rest = dw(in) {
              case RawText(s) =>
                sb.append(s)
                true
              case _ => false
            }
            buffer += RawText(sb.toString)
            rcb(rest)
          case x :: xs =>
            buffer += x; rcb(xs)
        }
      }

      rcb(ast)

      buffer.toList
  }

}