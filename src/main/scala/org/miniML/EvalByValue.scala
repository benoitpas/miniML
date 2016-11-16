package org.miniML

import org.miniML.parser._

object EvalByValue {
  val parser = new ExpressionParser()
  
  def apply(s: String)= {
    val r = parser.parse(parser.expression,s)
    if (r.successful ) Some(eval(r.get)) else None
  }

  def eval(e: Expression): Int = {
    e match {
      case Product(e1: Expression, e2: Expression) => eval(e1) * eval(e2)
      case Sum(e1: Expression, e2: Expression)     => eval(e1) + eval(e2)
      //case class Identifier(s:String) 
      case Integer(i: Int)                         => i
      //case class Let(id:Identifier,e1:Expression,e2:Expression) extends Expression

    }
  }

}