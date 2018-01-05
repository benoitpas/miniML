package org.miniML
import org.miniML.parser._

trait EvalCommon {
  sealed trait Mode
  case object ByValue extends Mode
  case object ByName extends Mode

  val parser = new ExpressionParser()

  type EExpression = Either[String,Expression]

  def apply(s: String, mode: Mode = ByValue): EExpression = {
    val r = parser.parse(s)
    if (r.successful) eval(r.get, mode) else Left("Parsing Failed: " + r.toString)
  }

  def identifier(s: String, mode: Mode): EExpression
  def let(id: Identifier, e1: Expression, e2: Expression, mode: Mode): EExpression
  def funApp(funExp: Expression, exp: Expression, mode: Mode): EExpression
  def fun(id: Identifier, funExp: Expression, mode: Mode): EExpression
  def fix(fid: Identifier, id: Identifier, funExp: Expression, mode: Mode): EExpression

  def eval(exp: Expression, mode: Mode): EExpression = {

    val notInteger = ": Cannot be evaluated as an integer"
    def combine(exp1: Expression, exp2: Expression, op: (Int,Int) => Int) : EExpression =
      (eval(exp1, mode), eval(exp2, mode)) match {
        case (Right(Integer(i1)), Right(Integer(i2))) => Right(Integer(op(i1,i2)))
        case (Right(Integer(i1)), _) => Left(exp2 + notInteger)
        case (_,Right(Integer(i1))) => Left(exp1 + notInteger)
        case (Left(s), _) => Left(s)
        case (_, Left(s)) => Left(s)
      }

    def intEval(exp: Expression) = eval(exp, mode) match {
      case Right(Integer(v)) => Right(Integer(v))
      case Right(_) => Left(exp + notInteger)
      case x => x
    }

    exp match {
      case Product(exp1: Expression, exp2: Expression)         => combine(exp1,exp2, _ * _)
      case Division(exp1: Expression, exp2: Expression)        => combine(exp1,exp2, _ / _)
      case Sum(exp1: Expression, exp2: Expression)             => combine(exp1,exp2, _ + _)
      case Minus(exp1: Expression, exp2: Expression)           => combine(exp1,exp2, _ - _)
      case Identifier(s: String)                               => identifier(s, mode)
      case Let(id: Identifier, e1: Expression, e2: Expression) => let(id, e1, e2, mode)
      case Ifz(cExp: Expression, zExp: Expression, nzExp: Expression) => intEval(cExp) match  {
        case Right(Integer(v)) => if (v > 0) intEval(nzExp) else intEval(zExp)
        case x => x
      }
      case FunApp(funExp: Expression, exp: Expression) => funApp(funExp, exp, mode)
      case Fun(id, funExp) => fun(id, funExp, mode)
      case Fix(fId, Fun(Identifier(id),funExp)) => fix(fId, id, funExp, mode)
      case Integer(i) => Right(Integer(i))
      case _ => Left(exp + ": Unable to evaluate")
    }
  }

}