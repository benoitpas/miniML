package org.miniML

import org.miniML.parser._

object Eval extends EvalCommon {

  type Env = Map[Identifier, Expression]

  def newEnv() = Map()

  def identifier(s: String, env: Env, mode: Mode) = env.getOrElse(Identifier(s), Identifier(s))

  def let(id: Identifier, e1: Expression, e2: Expression, env: Env, mode: Mode) = if (mode == Eval.ByValue)
    eval(e2, env + (id -> eval(e1, env, mode)), mode)
  else
    eval(replaceAll(e2, id, e1), env, mode)
    
  def funApp(funExp: Expression, exp: Expression, env: Env, mode: Mode) = if (mode == Eval.ByValue )
    (eval(funExp, env, mode), eval(exp, env, mode)) match {
        case (Fun(id1, funExp1), Fun(id2, funExp2)) => {
          val r = replaceAll(funExp1, id1, Fun(id2, funExp2));// println("r=" + r.toString());
          r
        }
        case (Fun(id, funExp), v) => eval(funExp, env + (id -> v), mode)
        case x                    => { println("x=" + x.toString()); exp }
      }
  else
    (eval(funExp, env, mode), exp) match {
        case (Fun(id1, funExp1), exp2) => {
          val r = replaceAll(funExp1, id1, exp2); println("r=" + r.toString());
          eval(r, env, mode)
        }
        //case x => { println("x=" + x.toString()); exp }
      }

  def fun(id: Identifier, funExp: Expression, env: Env, mode: Mode) = Fun(id, replace(funExp, env, Set(id)))
  
  def fix(fid: Identifier, id: Identifier, funExp: Expression, env: Env, mode: Mode) = Fun(id,replaceAll(funExp,fid,Fix(fid,Fun(id,funExp))))
  
  def replace(funExp1: Expression, env: Map[Identifier, Expression], tiedVars: Set[Identifier]): Expression = {
    funExp1 match {
      case Product(e1, e2) => Product(replace(e1, env, tiedVars), replace(e2, env, tiedVars))
      case Division(e1, e2) => Division(replace(e1, env, tiedVars), replace(e2, env, tiedVars))
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
      case Division(e1, e2)                           => Division(replace(e1, id1, exp2), replace(e2, id1, exp2))
      case Sum(e1, e2)                               => Sum(replace(e1, id1, exp2), replace(e2, id1, exp2))
      case Minus(e1, e2)                             => Minus(replace(e1, id1, exp2), replace(e2, id1, exp2))
      case Identifier(id) if (Identifier(id) == id1) => exp2
      case Let(id, idExp, exp)                       => Let(id, replace(idExp, id1, exp2), if (id != id1) replace(exp, id1, exp2) else exp)
      case Ifz(cExp, zExp, nzExp)                    => Ifz(replace(cExp, id1, exp2), replace(zExp, id1, exp2), replace(nzExp, id1, exp2))
      case FunApp(e1, e2)                            => FunApp(replace(e1, id1, exp2), replace(e2, id1, exp2))
      case Fun(id, funExp) if (id != id1)            => Fun(id, replace(funExp, id1, exp2))
      case Fix(id, fixExp) if (id != id1)            => Fix(id, replace(fixExp, id1, exp2))
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
