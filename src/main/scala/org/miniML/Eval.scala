package org.miniML

import org.miniML.parser._

/*
 * Evaluate expression by 'replacing' functions and lambdas
 */
object Eval extends EvalCommon {

  type Context = Null

  def emptyContext = null

  // TODO: add implicit conversion from Expression to CExpression instead
  def eval(e:Expression, mode: Mode) : EExpression = eval(CExpression(e, null), mode)

  // Should never be called, identifiers should have been replaced by their values
  def identifier(s: String, c:Context, mode: Mode) = Left(s + ": Unknown identifier")

  def let(id: Identifier, e1: Expression, e2: Expression, c: Context, mode: Mode): EExpression = {
    if (mode == Eval.ByValue)
      eval(e1, mode).flatMap((v: CExpression) => eval(replaceAll(e2, id, v.e), mode))
    else
      eval(CExpression(replaceAll(e2, id, e1), c), mode)
  }

  def funApp(funExp: Expression, exp: Expression, c: Context, mode: Mode): EExpression = {

    def application(v: CExpression): EExpression = {
      eval(CExpression(funExp, c), mode).flatMap {
        case CExpression(Fun(id1, funExp1), _) =>
          val r = replaceAll(funExp1, id1, v.e); // println("r=" + r.toString());
          eval(r, mode)
        case _ => Left(funExp + ": Did not evaluate as a function")
      }
    }

    if (mode == Eval.ByValue ) eval(CExpression(exp,c), mode).flatMap(application) else application(CExpression(exp, c))
  }

  def fun(id: Identifier, funExp: Expression, c: Context, mode: Mode) = Right(CExpression(Fun(id, funExp), c))
  
  def fix(fid: Identifier, id: Identifier, funExp: Expression, c:Context, mode: Mode) =
    Right(CExpression(Fun(id, replaceAll(funExp,fid,Fix(fid,Fun(id,funExp)))), c))

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
      case x => x
    }
  }
}
