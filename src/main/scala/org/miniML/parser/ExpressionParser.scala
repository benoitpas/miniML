package org.miniML.parser
import scala.util.parsing.combinator.RegexParsers

class ExpressionParser extends RegexParsers {
  def identifier = """[_\p{L}][_\p{L}\p{Nd}]*""".r ^^ { case s => Identifier(s) }
  def integer = """(0|[1-9]\d*)""".r ^^ { case s => Integer(s.toInt) }
  def pterm = "(" ~ expression ~ ")" ^^ { case _ ~ e ~ _ => e }
  def term: Parser[Expression] = identifier | integer | pterm
  def product = term ~ "*" ~ term ^^ { case e1 ~ o ~ e2 => Product(e1, e2) }
  def factor = product | term
  def sum = factor ~ "+" ~ factor ^^ { case e1 ~ o ~ e2 => Sum(e1, e2) }
  def let = "let" ~ identifier ~ "=" ~ expression ~ "in" ~ expression ^^ { case _ ~ id ~ _ ~ e1 ~ _ ~ e2 => Let(id, e1, e2) }
  def expression: Parser[Expression] = let | sum | product | term
}

abstract trait Expression
case class Product(e1: Expression, e2:Expression) extends Expression
case class Sum(e1: Expression, e2:Expression) extends Expression
case class Identifier(s:String) extends Expression
case class Integer(i:Int) extends Expression
case class Let(id:Identifier,e1:Expression,e2:Expression) extends Expression
