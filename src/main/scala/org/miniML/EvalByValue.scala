package org.miniML

import org.miniML.parser._

object EvalByValue {
  val parser = new ExpressionParser()
  
  def apply(s: String)= {
    val r = parser.parse(parser.expression, s)
    if (r.successful) Some(eval(r.get, Map())) else None
  }

  def eval(exp: Expression, env: Map[Identifier, Expression]): Expression = {
    println("eval("+exp+", "+env+")")
    exp match {
      case Product(e1: Expression, e2: Expression) => (eval(e1, env), eval(e2, env)) match {
        case (Integer(i1), Integer(i2)) => Integer(i1 * i2)
        case (e1,e2) => Product(e1,e2)
      }
      case Sum(e1: Expression, e2: Expression) => (eval(e1, env), eval(e2, env)) match {
        case (Integer(i1), Integer(i2)) => Integer(i1 + i2)
        case (e1,e2) => Sum(e1,e2)
      }
      case Minus(e1: Expression, e2: Expression) => (eval(e1, env), eval(e2, env)) match {
        case (Integer(i1), Integer(i2)) => Integer(i1 - i2)
        case (e1,e2) => Minus(e1,e2)
      }
      case Identifier(s: String) => env.getOrElse(Identifier(s), exp)
      case Let(id: Identifier, e1: Expression, e2: Expression) => eval(e2, env + (id -> eval(e1, env)))
      case Ifz( cExp: Expression,zExp: Expression, nzExp: Expression) => eval(cExp,env) match {
        case Integer(0) => eval(zExp,env)
        case Integer(_) => eval(nzExp,env)
        case _ => exp
      }
      case FunApp(funExp: Expression, exp: Expression) => (eval(funExp, env),eval(exp,env)) match {
        case (Fun(id1,funExp1),Fun(id2,funExp2)) => 
          {
            val l = Fun(id2,Let(id1,funExp2,funExp1))
            println(l)
            l
          }
        case (Fun(id,funExp),v) => {
          println("eval("+funExp+","+env + (id -> v))
          eval(funExp, env + (id -> v))
        }
        case _ => exp
      }
      case _ => exp
    }
  }

}