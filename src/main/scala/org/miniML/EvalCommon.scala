package org.miniML
import org.miniML.parser._

trait EvalCommon {
  sealed trait Mode
  case object ByValue extends Mode
  case object ByName extends Mode
  
  val parser = new ExpressionParser()

  def apply(s: String, mode: Mode = ByValue): Either[String,Expression] = {
    val r = parser.parse(s)
    if (r.successful) Right(eval(r.get, mode)) else Left("Parsing Failed: " + r.toString)
  }

  def identifier(s: String, mode: Mode): Expression
  def let(id: Identifier, e1: Expression, e2: Expression, mode: Mode): Expression
  def funApp(funExp: Expression, exp: Expression, mode: Mode): Expression
  def fun(id: Identifier, funExp: Expression, mode: Mode): Expression
  def fix(fid: Identifier, id: Identifier, funExp: Expression, mode: Mode): Expression

  def eval(exp: Expression, mode: Mode): Expression = {
    exp match {
      case Product(exp1: Expression, exp2: Expression) => (eval(exp1, mode), eval(exp2, mode)) match {
        case (Integer(i1), Integer(i2)) => Integer(i1 * i2)
        case (e1, e2)                   => Product(e1, e2)
      }
      case Division(exp1: Expression, exp2: Expression) => (eval(exp1, mode), eval(exp2, mode)) match {
        case (Integer(i1), Integer(i2)) => Integer(i1 / i2)
        case (e1, e2)                   => Product(e1, e2)
      }
      case Sum(exp1: Expression, exp2: Expression) => (eval(exp1, mode), eval(exp2, mode)) match {
        case (Integer(i1), Integer(i2)) => Integer(i1 + i2)
        case (e1, e2)                   => Sum(e1, e2)
      }
      case Minus(exp1: Expression, exp2: Expression) => (eval(exp1, mode), eval(exp2, mode)) match {
        case (Integer(i1), Integer(i2)) => Integer(i1 - i2)
        case (e1, e2)                   => Minus(e1, e2)
      }
      case Identifier(s: String)                               => identifier(s, mode)
      case Let(id: Identifier, e1: Expression, e2: Expression) => let(id, e1, e2, mode)
      case Ifz(cExp: Expression, zExp: Expression, nzExp: Expression) => eval(cExp, mode) match {
        case Integer(v) if v > 0 => eval(nzExp, mode)
        case _ => eval(zExp, mode)
      }
      case FunApp(funExp: Expression, exp: Expression) => funApp(funExp, exp, mode)
      case Fun(id, funExp) => fun(id, funExp, mode)
      case Fix(fId, Fun(Identifier(id),funExp)) => fix(fId, id, funExp, mode)
      case _ => exp
    }
  }

}