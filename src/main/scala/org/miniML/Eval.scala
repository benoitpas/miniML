package org.miniML

import org.miniML.parser._

object Eval {

  sealed trait Mode
  case object ByValue extends Mode
  case object ByName extends Mode
  
  val parser = new ExpressionParser()

  def apply(s: String, mode: Mode = ByValue) = {
    val r = parser.parse(parser.expression, s)
    if (r.successful) Some(eval(r.get, Map(), mode)) else {
      println(r.toString())
      None
    }
  }

  def eval(exp: Expression, env: Map[Identifier, Expression], mode:Mode): Expression = {
    exp match {
      case Product(e1: Expression, e2: Expression) => (eval(e1, env, mode), eval(e2, env, mode)) match {
        case (Integer(i1), Integer(i2)) => Integer(i1 * i2)
        case (e1,e2) => Product(e1,e2)
      }
      case Sum(e1: Expression, e2: Expression) => (eval(e1, env, mode), eval(e2, env, mode)) match {
        case (Integer(i1), Integer(i2)) => Integer(i1 + i2)
        case (e1,e2) => Sum(e1,e2)
      }
      case Minus(e1: Expression, e2: Expression) => (eval(e1, env, mode), eval(e2, env, mode)) match {
        case (Integer(i1), Integer(i2)) => Integer(i1 - i2)
        case (e1,e2) => Minus(e1,e2)
      }
      case Identifier(s: String) => env.getOrElse(Identifier(s), exp)
      case Let(id: Identifier, e1: Expression, e2: Expression) => eval(e2, env + (id -> eval(e1, env, mode)), mode)
      case Ifz( cExp: Expression,zExp: Expression, nzExp: Expression) => eval(cExp,env,mode) match {
        case Integer(0) => eval(zExp, env, mode)
        case Integer(_) => eval(nzExp, env, mode)
        case _          => exp
      }
      case FunApp(funExp: Expression, exp: Expression) => (eval(funExp, env, mode), eval(exp, env, mode)) match {
        case (Fun(id1, funExp1), Fun(id2, funExp2)) => {
          val r = replaceAll(funExp1, id1, Fun(id2, funExp2)); println("r=" + r.toString());
          r
        }
        case (Fun(id, funExp), v) => eval(funExp, env + (id -> v), mode)
        case x                    => { println("x=" + x.toString()); exp }
      }
      case Fun(id,funExp) => Fun(id,replace(funExp,env,Set(id)))
      case _ => exp
    }
  }

  def replace(funExp1: Expression, env: Map[Identifier, Expression], tiedVars: Set[Identifier]): Expression = {
    funExp1 match {
      case Product(e1, e2) => Product(replace(e1, env, tiedVars), replace(e2, env, tiedVars))
      case Sum(e1, e2) => Sum(replace(e1, env, tiedVars), replace(e2, env, tiedVars))
      case Minus(e1, e2) => Minus(replace(e1, env, tiedVars), replace(e2, env, tiedVars))
      case Identifier(id2) if (!tiedVars.contains(Identifier(id2)) && env.contains(Identifier(id2))) => env(Identifier(id2))
      case Let(id, idExp, exp) => Let(id, replace(idExp, env, tiedVars), replace(exp, env, tiedVars + id))
      case Ifz(cExp, zExp, nzExp) => Ifz(replace(cExp, env, tiedVars), replace(zExp, env, tiedVars), replace(nzExp, env, tiedVars))
      case FunApp(e1, e2) => FunApp(replace(e1, env, tiedVars), replace(e2, env, tiedVars))
      case Fun(id, funExp) => Fun(id, replace(funExp, env, tiedVars + id))
      case _ => funExp1
    }
  }

  def replace(funExp1: Expression, id1: Identifier, exp2: Expression): Expression = {
    funExp1 match {
      case Product(e1, e2)                           => Product(replace(e1, id1, exp2), replace(e2, id1, exp2))
      case Sum(e1, e2)                               => Sum(replace(e1, id1, exp2), replace(e2, id1, exp2))
      case Minus(e1, e2)                             => Minus(replace(e1, id1, exp2), replace(e2, id1, exp2))
      case Identifier(id) if (Identifier(id) == id1) => exp2
      case Let(id, idExp, exp)                       => Let(id, replace(idExp, id1, exp2), if (id != id1) replace(exp, id1, exp2) else exp)
      case Ifz(cExp, zExp, nzExp)                    => Ifz(replace(cExp, id1, exp2), replace(zExp, id1, exp2), replace(nzExp, id1, exp2))
      case FunApp(e1, e2)                            => FunApp(replace(e1, id1, exp2), replace(e2, id1, exp2))
      case Fun(id, funExp) if (id != id1)            => Fun(id, replace(funExp, id1, exp2))
      case _                                         => funExp1
    }
  }

  def replaceAll(funExp1: Expression, id1: Identifier, exp2: Expression): Expression = {
    replace(funExp1, id1, exp2) match {
      case FunApp(Fun(id1, funExp1), e2) => replaceAll(funExp1, id1, e2)
      case x                             => x
    }
  }

}
