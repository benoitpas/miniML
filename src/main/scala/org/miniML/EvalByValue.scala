package org.miniML

import org.miniML.parser._

object EvalByValue {
  val parser = new ExpressionParser()
  
  def apply(s: String)= {
    val r = parser.parse(parser.expression, s)
    if (r.successful) Some(eval(r.get, Map())) else {
      println(r.toString())
      None
    }
  }

  def eval(exp: Expression, env: Map[Identifier, Expression]): Expression = {
    println("eval("+exp+","+env)
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
      case FunApp(funExp: Expression, exp: Expression) => (eval(funExp, env), eval(exp, env)) match {
        case (Fun(id1, funExp1), Fun(id2, funExp2)) => replace(funExp1, id1, Fun(id2, funExp2))
        case (Fun(id, funExp), v) => eval(funExp, env + (id -> v))
        case _ => exp
      }
      case _ => exp
    }
  }

  def replace(funExp1: Expression, id1: Identifier, fun2: Expression): Expression = {
    funExp1 match {
      case Product(e1, e2)                           => Product(replace(e1, id1, fun2), replace(e2, id1, fun2))
      case Sum(e1, e2)                               => Sum(replace(e1, id1, fun2), replace(e2, id1, fun2))
      case Minus(e1, e2)                             => Minus(replace(e1, id1, fun2), replace(e2, id1, fun2))
      case Identifier(id) if (Identifier(id) == id1) => fun2
      case Let(id, idExp, exp)                       => Let(id, replace(idExp, id1, fun2), if (id != id1) replace(exp, id1, fun2) else exp)
      case Ifz(cExp, zExp, nzExp)                    => Ifz(replace(cExp, id1, fun2), replace(zExp, id1, fun2), replace(nzExp, id1, fun2))
      case FunApp(e1, e2)                            => FunApp(replace(e1, id1, fun2), replace(e2, id1, fun2))
      case Fun(id, funExp) if (id != id1)            => Fun(id, replace(funExp, id1, fun2))
      case _                                         => funExp1
    }
  }

}