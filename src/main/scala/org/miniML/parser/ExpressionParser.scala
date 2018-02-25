package org.miniML.parser

import scala.language.implicitConversions
import scala.util.parsing.combinator.syntactical.StandardTokenParsers


class ExpressionParser extends StandardTokenParsers {
  lexical.delimiters ++= List("(",")","=","->","+","-","*","/")
  lexical.reserved ++= Set("let", "in", "fun", "ifz", "then", "else","fix")

  def identifier : Parser[Identifier] = ident ^^ { case s => Identifier(s) }
  def integer = numericLit ^^ { case s => Integer(s.toInt) }
  def pterm = "(" ~> expression <~ ")" ^^ { case e => e }
  def iPTerm = "(" ~> iExpression <~ ")" ^^ { case e => e }
  def fPTerm = "(" ~> fExpression <~ ")" ^^ { case e => e }
  def fTerm: Parser[Expression] = fix | fun | fIfz | fLet | identifier | fPTerm
  def iTerm: Parser[Expression] = iIfz | iLet | identifier | integer | iPTerm
  def iProduct = iTerm ~ rep1(("*"|"/") ~ iTerm) ^^ { case e1 ~ e2 => e2.foldLeft(e1) {
    case (a,b) if (b._1 == "*") => Product(a,b._2)
    case (a,b) if (b._1 == "/") => Division(a,b._2)
  } }
  def iFactor = iProduct | iTerm
  def iSum = iFactor ~ rep1(("+"|"-") ~ iFactor) ^^ { case e1 ~ e2 => e2.foldLeft(e1) {
    case (a,b) if (b._1 == "+") => Sum(a,b._2)
    case (a,b) if (b._1 == "-") => Minus(a,b._2)
  } }
  def iLet = "let" ~ identifier ~ "=" ~ expression ~ "in" ~ iExpression ^^ { case _ ~ id ~ _ ~ e1 ~ _ ~ e2 => Let(id, e1, e2) }
  def fLet = "let" ~ identifier ~ "=" ~ expression ~ "in" ~ fExpression ^^ { case _ ~ id ~ _ ~ e1 ~ _ ~ e2 => Let(id, e1, e2) }
  def let = "let" ~ identifier ~ "=" ~ expression ~ "in" ~ expression ^^ { case _ ~ id ~ _ ~ e1 ~ _ ~ e2 => Let(id, e1, e2) }
  def iIfz ="ifz" ~ iExpression ~ "then" ~ iExpression ~ "else" ~ iExpression ^^ { case _ ~ cExp ~ _ ~ zExp ~ _ ~ nZExp => Ifz(cExp, zExp, nZExp) }
  def fIfz ="ifz" ~ expression ~ "then" ~ expression ~ "else" ~ fExpression ^^ { case _ ~ cExp ~ _ ~ zExp ~ _ ~ nZExp => Ifz(cExp, zExp, nZExp) }
  def ifz ="ifz" ~ expression ~ "then" ~ expression ~ "else" ~ expression ^^ { case _ ~ cExp ~ _ ~ zExp ~ _ ~ nZExp => Ifz(cExp, zExp, nZExp) }
  def fun = "fun" ~ identifier ~ "->" ~ expression ^^ { case _ ~ variable ~ _ ~ e => Fun(variable,e) }
  def fix = "fix" ~ identifier ~ expression ^^ { case _ ~ variable ~ e => Fix(variable,e) }
  def funApp = fTerm ~ rep1(lExpression) ^^ { case funExp ~ e => e.foldLeft(funExp)(FunApp(_,_)) }

  // Only for integer
  def iExpression: Parser[Expression] =  funApp | iIfz | iLet | iSum | iProduct | identifier | integer | iPTerm
  // Only for functions
  def fExpression: Parser[Expression] =  funApp | fix | fun | fIfz | fLet | identifier | fPTerm
  // limited expressions (not function applications)
  def lExpression: Parser[Expression] =  fix | fun | ifz | let | iSum | iProduct | identifier | integer | pterm
  def expression: Parser[Expression] =  funApp | lExpression

  def parse(s:String) : ParseResult[Expression] = expression(new lexical.Scanner(s))
}

sealed trait Expression {
//  implicit def int2Expression(x: Int): Expression = int2Integer(x: Int)
}

case class Product(e1: Expression, e2: Expression) extends Expression {
  override def toString : String = e1 + " * " + e2
}

case class Division(e1: Expression, e2: Expression) extends Expression {
  override def toString : String = e1 + " / " + e2
}

case class Sum(e1: Expression, e2: Expression) extends Expression {
  override def toString : String = e1 + " + " + e2
}

case class Minus(e1: Expression, e2: Expression) extends Expression {
  override def toString : String = e1 + " - " + e2
}

case class Identifier(s: String) extends Expression {
  override def toString : String = s
}

object Identifier {
  implicit def int2Identifier(s: String): Identifier = new Identifier(s)
}

case class Integer(i: Int) extends Expression {
  override def toString : String = i.toString
}

object Integer {
  implicit def int2Integer(x: Int): Integer = new Integer(x)
}

case class Let(id: Identifier, e1: Expression, e2: Expression) extends Expression {
  override def toString : String = "let " + id + " = " + e1 + " in " + e2
}

case class Ifz(cExp:Expression, zExp:Expression, nZExp:Expression) extends Expression {
  override def toString : String = "ifz " + cExp + " then " + zExp + " else " + nZExp
}
case class Fun(variable: Identifier, e: Expression) extends Expression {
  override def toString : String = "fun " + variable + " -> " + e
}

case class Fix(variable: Identifier, e: Expression) extends Expression {
  override def toString : String = "fix " + variable + " " + e
}

case class FunApp(funExp: Expression, exp: Expression) extends Expression {
  override def toString : String = "(" + funExp + " " + exp + ")"
}


