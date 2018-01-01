package org.miniML.parser

import scala.language.implicitConversions
import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers


class ExpressionParser extends RegexParsers {
  def keywords = Set("let", "in", "fun", "ifz", "then", "else","fix")
  def identifier = regex2("""[_\p{L}][_\p{L}\p{Nd}]*""".r) ^^ { case s => Identifier(s) }
  def integer = """(0|[1-9]\d*)""".r ^^ { case s => Integer(s.toInt) }
  def pterm = "(" ~ expression ~ ")" ^^ { case _ ~ e ~ _ => e }
  def term: Parser[Expression] = fix | fun | ifz | let | identifier | integer | pterm
  def product = term ~ "*" ~ term ^^ { case e1 ~ o ~ e2 => Product(e1, e2) }
  def factor = product | term
  def sum = factor ~ "+" ~ factor ^^ { case e1 ~ o ~ e2 => Sum(e1, e2) }
  def minus = factor ~ "-" ~ factor ^^ { case e1 ~ o ~ e2 => Minus(e1, e2) }
  def let = "let" ~ identifier ~ "=" ~ expression ~ "in" ~ expression ^^ { case _ ~ id ~ _ ~ e1 ~ _ ~ e2 => Let(id, e1, e2) }
  def ifz ="ifz" ~ expression ~ "then" ~ expression ~ "else" ~ expression ^^ { case _ ~ cExp ~ _ ~ zExp ~ _ ~ nZExp => Ifz(cExp, zExp, nZExp) }
  def fun = "fun" ~ identifier ~ "->" ~ expression ^^ { case _ ~ variable ~ _ ~ e => Fun(variable,e) }
  def fix = "fix" ~ identifier ~ expression ^^ { case _ ~ variable ~ e => Fix(variable,e) }
  // add fterm for 'functional term' (does not include integer operation ?) + vterm for value terms
  def funApp = term ~ rep1(expression) ^^ { case funExp ~ e => e.foldLeft(funExp)(FunApp(_,_)) }

  def expression: Parser[Expression] =  funApp | fix | fun | ifz | let | sum | minus | product | identifier | integer | pterm

 
  def regex2(r: Regex): Parser[String] = new Parser[String] {
    def apply(in: Input) = {
      val source = in.source
      val offset = in.offset
      val start = handleWhiteSpace(source, offset)
      (r findPrefixMatchOf (source.subSequence(start,source.length()))) match {
        case Some(matched) => {
          val s = source.subSequence(start, start + matched.end).toString
          if (keywords(s)) Failure("string matching keyword "+s,in.drop(start - offset))
          else Success(s,in.drop(start + matched.end - offset)) }
        case None =>
          val found = if (start == source.length()) "end of source" else "`"+source.charAt(start)+"'"
          Failure("string matching regex `"+r+"' expected but "+found+" found", in.drop(start - offset))
      }
    }
  }
}

abstract trait Expression {
//  implicit def int2Expression(x: Int): Expression = int2Integer(x: Int)
}
case class Product(e1: Expression, e2: Expression) extends Expression {
  override def toString() = e1 + " * " + e2
}
case class Sum(e1: Expression, e2: Expression) extends Expression {
  override def toString() = e1 + " + " + e2  
}
case class Minus(e1: Expression, e2: Expression) extends Expression {
  override def toString() = e1 + " - " + e2  
}
case class Identifier(s: String) extends Expression {
  override def toString() = s  
}
object Identifier {
  implicit def int2Identifier(s: String): Identifier = new Identifier(s)
}
case class Integer(i: Int) extends Expression {
  override def toString() = i.toString()
}
object Integer {
  implicit def int2Integer(x: Int): Integer = new Integer(x)
}
case class Let(id: Identifier, e1: Expression, e2: Expression) extends Expression {
  override def toString() = "let " + id + " = " + e1 + " in " + e2
}
case class Ifz(cExp:Expression, zExp:Expression, nZExp:Expression) extends Expression {
  override def toString() = "ifz " + cExp + " then " + zExp + " else " + nZExp
}
case class Fun(variable: Identifier, e: Expression) extends Expression {
  override def toString() = "fun " + variable + " -> " + e
}

case class Fix(variable: Identifier, e: Expression) extends Expression {
  override def toString() = "fix " + variable + " " + e
}

case class FunApp(funExp: Expression, exp: Expression) extends Expression {
//  override def toString() = "(" + funExp + " " + exp + ")"
}


