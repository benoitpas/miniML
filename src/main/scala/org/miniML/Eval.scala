package org.miniML

import org.miniML.parser._

object Eval extends EvalCommon {

  // Should never be called, identifiers should have been replaced by their values
  // TODO: raise error
  def identifier(s: String, mode: Mode) = Identifier(s)

  def let(id: Identifier, e1: Expression, e2: Expression, mode: Mode) : Expression = {
    val v = if (mode == Eval.ByValue) eval(e1,mode) else e1
    eval(replaceAll(e2, id, v), mode)
  }
    
  def funApp(funExp: Expression, exp: Expression, mode: Mode) : Expression  = {
    val v = if (mode == Eval.ByValue ) eval(exp,mode) else exp
    val replaced = eval(funExp, mode) match {
      case Fun(id1, funExp1) =>
        val r = replaceAll(funExp1, id1, v);// println("r=" + r.toString());
        r
      case x                    => println("x=" + x.toString); exp  // TODO:raise an error
    }
    eval(replaced, mode)
  }

  def fun(id: Identifier, funExp: Expression, mode: Mode) = Fun(id, funExp)
  
  def fix(fid: Identifier, id: Identifier, funExp: Expression, mode: Mode) = Fun(id, replaceAll(funExp,fid,Fix(fid,Fun(id,funExp))))

  // Replaces id1 in exp2 by funExp1
  def replace(funExp1: Expression, id1: Identifier, exp2: Expression): Expression = {
    funExp1 match {
      case Product(e1, e2)                           => Product(replace(e1, id1, exp2), replace(e2, id1, exp2))
      case Division(e1, e2)                          => Division(replace(e1, id1, exp2), replace(e2, id1, exp2))
      case Sum(e1, e2)                               => Sum(replace(e1, id1, exp2), replace(e2, id1, exp2))
      case Minus(e1, e2)                             => Minus(replace(e1, id1, exp2), replace(e2, id1, exp2))
      case Identifier(id) if Identifier(id) == id1   => exp2
      case Let(id, idExp, exp)                       => Let(id, replace(idExp, id1, exp2), if (id != id1) replace(exp, id1, exp2) else exp)
      case Ifz(cExp, zExp, nzExp)                    => Ifz(replace(cExp, id1, exp2), replace(zExp, id1, exp2), replace(nzExp, id1, exp2))
      case FunApp(e1, e2)                            => FunApp(replace(e1, id1, exp2), replace(e2, id1, exp2))
      case Fun(id, funExp) if id != id1              => Fun(id, replace(funExp, id1, exp2))
      case Fix(id, fixExp) if id != id1              => Fix(id, replace(fixExp, id1, exp2))
      case _                                         => funExp1
    }
  }

  def replaceAll(funExp1: Expression, id1: Identifier, exp2: Expression): Expression = {
    replace(funExp1, id1, exp2) match {
      case FunApp(Fun(id, funExp), exp) => replaceAll(funExp, id, exp)
      case x                             => x
    }
  }
}
