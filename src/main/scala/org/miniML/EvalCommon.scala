package org.miniML
import org.miniML.parser._

trait EvalCommon {
  sealed trait Mode
  case object ByValue extends Mode
  case object ByName extends Mode
  
  type Env
  
  val parser = new ExpressionParser()

  def newEnv(): Env

  def apply(s: String, mode: Mode = ByValue) = {
    val r = parser.parse(parser.expression, s)
    if (r.successful) Some(eval(r.get, newEnv(), mode)) else {
      println(r.toString())
      None
    }
  }

  def identifier(s: String, env: Env, mode: Mode): Expression
  def let(id: Identifier, e1: Expression, e2: Expression, env: Env, mode: Mode): Expression
  def funApp(funExp: Expression, exp: Expression, env: Env, mode: Mode): Expression
  def fun(id: Identifier, funExp: Expression, env: Env, mode: Mode): Expression
  def fix(fid: Identifier, id: Identifier, funExp: Expression, env: Env, mode: Mode): Expression

  def eval(exp: Expression, env: Env, mode: Mode): Expression = {
    exp match {
      case Product(e1: Expression, e2: Expression) => (eval(e1, env, mode), eval(e2, env, mode)) match {
        case (Integer(i1), Integer(i2)) => Integer(i1 * i2)
        case (e1, e2)                   => Product(e1, e2)
      }
      case Sum(e1: Expression, e2: Expression) => (eval(e1, env, mode), eval(e2, env, mode)) match {
        case (Integer(i1), Integer(i2)) => Integer(i1 + i2)
        case (e1, e2)                   => Sum(e1, e2)
      }
      case Minus(e1: Expression, e2: Expression) => (eval(e1, env, mode), eval(e2, env, mode)) match {
        case (Integer(i1), Integer(i2)) => Integer(i1 - i2)
        case (e1, e2)                   => Minus(e1, e2)
      }
      case Identifier(s: String)                               => identifier(s, env, mode)
      case Let(id: Identifier, e1: Expression, e2: Expression) => let(id, e1, e2, env, mode)
      case Ifz(cExp: Expression, zExp: Expression, nzExp: Expression) => eval(cExp, env, mode) match {
        case Integer(0) => eval(zExp, env, mode)
        case Integer(_) => eval(nzExp, env, mode)
        case _          => exp
      }
      case FunApp(funExp: Expression, exp: Expression) => funApp(funExp, exp, env, mode)
      case Fun(id, funExp) => fun(id, funExp, env, mode)
      case Fix(fId, Fun(Identifier(id),funExp)) => fix(fId, id, funExp, env, mode)
      case _ => exp
    }
  }

}