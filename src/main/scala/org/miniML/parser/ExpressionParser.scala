package org.miniML.parser

import scala.language.implicitConversions
import scala.util.parsing.combinator.syntactical.StandardTokenParsers


/*
 * Parsing of expressions using Scala monadic parsers
 *
 */
class ExpressionParser extends StandardTokenParsers {
    lexical.delimiters ++= List("(", ")", "=", "->", "+", "-", "*", "/")
    lexical.reserved ++= Set("let", "in", "fun", "ifz", "then", "else", "fix", "nil", "cons", "head", "tail", "ifnil")

    def identifier: Parser[Identifier] = ident ^^ { Identifier(_) }

    def integer : Parser[Integer] = numericLit ^^ { s => Integer(s.toInt) }

    def pTerm : Parser[Expression] = "(" ~> expression <~ ")" ^^ { e => e }

    def term: Parser[Expression] = fix | fun | ifz | let | identifier | integer | pTerm

    def product : Parser[Expression] = term ~ rep1(("*" | "/") ~ term) ^^ { case e1 ~ e2 => e2.foldLeft(e1) {
        case (a, b) if b._1 == "*" => Product(a, b._2)
        case (a, b) if b._1 == "/" => Division(a, b._2)
    }
    }

    def factor : Parser[Expression] = product | term

    def iSum : Parser[Expression] = factor ~ rep1(("+" | "-") ~ factor) ^^ {
        case e1 ~ e2 => e2.foldLeft(e1) {
            case (a, b) if b._1 == "+" => Sum(a, b._2)
            case (a, b) if b._1 == "-" => Minus(a, b._2)
        }
    }

    def nil : Parser[NilList] = rep1("nil") ^^ { _ => NilList() }

    def head : Parser[Head] = "head" ~> expression  ^^ { Head }

    def tail : Parser[Tail] = "tail" ~> expression  ^^ { Tail }

    def cons : Parser[Cons] = "cons" ~> limitedExpression ~ expression  ^^ { case head ~ tail => Cons(head, tail) }

    def ifnil : Parser[Ifnil] = "ifnil" ~> expression ~ "then" ~ expression ~ "else" ~ expression ^^ {
        case cExp ~ _ ~ zExp ~ _ ~ nZExp => Ifnil(cExp, zExp, nZExp)
    }

    def listExpression : Parser[Expression] = nil | head | tail | cons | ifnil

    def let : Parser[Let]  = "let" ~> identifier ~ "=" ~ expression ~ "in" ~ expression ^^ {
        case id ~ _ ~ e1 ~ _ ~ e2 => Let(id, e1, e2)
    }

    def ifz : Parser[Ifz]  = "ifz" ~> expression ~ "then" ~ expression ~ "else" ~ expression ^^ {
        case cExp ~ _ ~ zExp ~ _ ~ nZExp => Ifz(cExp, zExp, nZExp)
    }

    def fun  : Parser[Fun] = "fun" ~> identifier ~ "->" ~ expression ^^ { case variable ~ _ ~ e => Fun(variable, e) }

    def fix  : Parser[Fix] = "fix" ~> identifier ~ expression ^^ { case variable ~ e => Fix(variable, e) }

    def funApp  : Parser[Expression] = term ~ rep1(limitedExpression) ^^ {
        case funExp ~ e => e.foldLeft(funExp)(FunApp)
    }

    // limited expressions (not including applications)
    def limitedExpression: Parser[Expression] = listExpression | fix | fun | ifz | let | iSum | product | identifier | integer | term

    def expression: Parser[Expression] = funApp | limitedExpression

    def parse(s: String): ParseResult[Expression] = expression(new lexical.Scanner(s))
}

sealed trait Expression {
    //  implicit def int2Expression(x: Int): Expression = int2Integer(x: Int)
}

case class Product(e1: Expression, e2: Expression) extends Expression {
    override def toString: String = e1 + " * " + e2
}

case class Division(e1: Expression, e2: Expression) extends Expression {
    override def toString: String = e1 + " / " + e2
}

case class Sum(e1: Expression, e2: Expression) extends Expression {
    override def toString: String = e1 + " + " + e2
}

case class Minus(e1: Expression, e2: Expression) extends Expression {
    override def toString: String = e1 + " - " + e2
}

case class Identifier(s: String) extends Expression {
    override def toString: String = s
}

object Identifier {
    implicit def int2Identifier(s: String): Identifier = new Identifier(s)
}

case class Integer(i: Int) extends Expression {
    override def toString: String = i.toString
}

object Integer {
    implicit def int2Integer(x: Int): Integer = new Integer(x)
}

case class NilList() extends Expression {
    override def toString: String = "nil"
}

case class Cons(head: Expression, tail:Expression) extends Expression {
    override def toString: String = "(cons " + head + " "+ tail + ")"
}

case class Head(l: Expression) extends Expression {
    override def toString: String = "(head " + l + ")"
}

case class Tail(l: Expression) extends Expression {
    override def toString: String = "(tail " + l + ")"
}


case class Ifnil(cExp: Expression, zExp: Expression, nZExp: Expression) extends Expression {
    override def toString: String = "ifnil " + cExp + " then " + zExp + " else " + nZExp
}

case class Let(id: Identifier, e1: Expression, e2: Expression) extends Expression {
    override def toString: String = "let " + id + " = " + e1 + " in " + e2
}

case class Ifz(cExp: Expression, zExp: Expression, nZExp: Expression) extends Expression {
    override def toString: String = "ifz " + cExp + " then " + zExp + " else " + nZExp
}

case class Fun(variable: Identifier, e: Expression) extends Expression {
    override def toString: String = "fun " + variable + " -> " + e
}

case class Fix(variable: Identifier, e: Expression) extends Expression {
    override def toString: String = "fix " + variable + " " + e
}

case class FunApp(funExp: Expression, exp: Expression) extends Expression {
    override def toString: String = "(" + funExp + " " + exp + ")"
}


