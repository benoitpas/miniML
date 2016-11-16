package org.miniML

import org.miniML.parser._

object EvalByValue {
  val parser = new ExpressionParser()
  
  def apply(s: String)= {
    val r = parser.parse(parser.expression,s)
    if (r.successful ) Some(eval(r.get,Map())) else None
  }

  def eval(exp: Expression, env:Map[Identifier,Int]): Int = {
    exp match {
      case Product(e1: Expression, e2: Expression) => eval(e1,env) * eval(e2,env)
      case Sum(e1: Expression, e2: Expression)     => eval(e1,env) + eval(e2,env)
      case Identifier(s:String)                    => env(Identifier(s))
      case Integer(i: Int)                         => i
      case Let(id:Identifier,e1:Expression,e2:Expression) => eval(e2,env + (id -> eval(e1,env)))
    }
  }

}