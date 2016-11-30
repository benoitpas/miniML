package org.miniML.parser

import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

class ExpressionParser extends RegexParsers {
  def keywords = Set("let", "in", "fun", "ifz", "then", "else")
  def identifier = regex2("""[_\p{L}][_\p{L}\p{Nd}]*""".r) ^^ { case s => Identifier(s) }
  def integer = """(0|[1-9]\d*)""".r ^^ { case s => Integer(s.toInt) }
  def pterm = "(" ~ expression ~ ")" ^^ { case _ ~ e ~ _ => e }
  def term: Parser[Expression] = fun | ifz | let | identifier | integer | pterm
  def product = term ~ "*" ~ term ^^ { case e1 ~ o ~ e2 => Product(e1, e2) }
  def factor = product | term
  def sum = factor ~ "+" ~ factor ^^ { case e1 ~ o ~ e2 => Sum(e1, e2) }
  def minus = factor ~ "-" ~ factor ^^ { case e1 ~ o ~ e2 => Minus(e1, e2) }
  def let = "let" ~ identifier ~ "=" ~ expression ~ "in" ~ expression ^^ { case _ ~ id ~ _ ~ e1 ~ _ ~ e2 => Let(id, e1, e2) }
  def ifz ="ifz" ~ expression ~ "then" ~ expression ~ "else" ~ expression ^^ { case _ ~ cExp ~ _ ~ zExp ~ _ ~ nZExp => Ifz(cExp, zExp, nZExp) }
  def fun = "fun" ~ identifier ~ "->" ~ expression ^^ { case _ ~ variable ~ _ ~ e => Fun(variable,e) }
  def funApp = term ~ expression ^^ { case funExp ~ e => FunApp(funExp,e) }

  def expression: Parser[Expression] =  funApp | fun | ifz | let | sum | minus | product | term

 
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

abstract trait Expression
case class Product(e1: Expression, e2: Expression) extends Expression
case class Sum(e1: Expression, e2: Expression) extends Expression
case class Minus(e1: Expression, e2: Expression) extends Expression
case class Identifier(s: String) extends Expression
case class Integer(i: Int) extends Expression
case class Let(id: Identifier, e1: Expression, e2: Expression) extends Expression
case class Ifz(cExp:Expression, zExp:Expression, nZExp:Expression) extends Expression
case class Fun(variable: Identifier, e: Expression) extends Expression
case class FunApp(funExp: Expression, exp: Expression) extends Expression
