package org.miniML
import org.miniML.parser._

trait EvalInterpretCommon {
  // Expression and context
  type Context
  // Error or Expression/Context
  type EExpression = Either[String,CExpression]
  val parser = new ExpressionParser()

  //implicit def expression2CExpression(e: Expression): CExpression =
  //  CExpression(e, emptyContext)
  def emptyContext: Context

  def apply(s: String, mode: Mode = ByValue): EExpression = {
    val r = parser.parse(s)
    if (r.successful) eval(CExpression(r.get, emptyContext), mode) else Left("Parsing Failed: " + r.toString)
  }

  def identifier(id: String, c: Context, mode: Mode): EExpression

  def let(id: Identifier, e1: Expression, e2: Expression, c: Context, mode: Mode): EExpression

  def funApp(funExp: Expression, exp: Expression, c: Context, mode: Mode): EExpression

  def fun(id: Identifier, funExp: Expression, c: Context, mode: Mode): EExpression

  def fix(fid: Identifier, id: Identifier, funExp: Expression, c: Context, mode: Mode): EExpression

  def cons(head: Expression, tail: Expression, c: Context, mode: Mode): EExpression =
    for {
      headValue <- eval(CExpression(head, c), mode)
      tailValue <- eval(CExpression(tail, c), mode)
    } yield CExpression(Cons(headValue.e, tailValue.e), c)

  def head(list: Expression, c: Context, mode: Mode): EExpression =
    eval(CExpression(list,c), mode) flatMap {
       listValue => listValue.e match {
        case Cons(head, _) => eval(CExpression(head,c), mode)
        case `listValue` => Left(listValue.toString + " has no 'head'")
        case _ => Left(listValue.toString + " is not a list")
      }
    }

    def tail(list: Expression, c: Context, mode: Mode): EvalInterpretCommon.this.EExpression = eval(CExpression(list,c), mode) flatMap {
        listValue => listValue.e match {
            case Cons(_, tail) => eval(CExpression(tail,c), mode)
            case `listValue` => Left(listValue.toString + " empty list")
            case _ => Left(listValue.toString + " is not a list")
        }
    }

    def eval(e:Expression, mode: Mode) : EExpression = eval(CExpression(e, emptyContext), mode)


    def eval(exp: CExpression, mode: Mode): EExpression = {

    val notInteger = ": Cannot be evaluated as an integer"
    val notList = ": Cannot be evaluated as a list"

    def combine(exp1: Expression, exp2: Expression, op: (Int,Int) => Int, c:Context) : EExpression =
      (eval(CExpression(exp1,c), mode), eval(CExpression(exp2,c), mode)) match {
        case (Right(CExpression(Integer(i1), _)), Right(CExpression(Integer(i2), _))) => Right(CExpression(Integer(op(i1, i2)), emptyContext))
        case (Left(s), _) => Left(s)
        case (_, Left(s)) => Left(s)
        case (Right(CExpression(Integer(_),_)), _) => Left(exp2.toString + notInteger)
        case (_,_) => Left(exp1.toString + notInteger)
      }

    def intEval(exp: Expression, c:Context) = eval(CExpression(exp,c), mode) flatMap {
      case CExpression(Integer(v),_) => Right(CExpression(Integer(v),c))
      case _ => Left(exp.toString + notInteger)
    }

      def listEval(exp: Expression, c:Context) = eval(CExpression(exp,c),mode) flatMap {
          case CExpression(NilList(),_) => Right(CExpression(NilList(),c))
          case CExpression(Cons(h,t),_) => Right(CExpression(Cons(h,t),c))
          case _  => Left(exp.toString + notList)
      }

    exp.e match {
      case Product(exp1: Expression, exp2: Expression)         => combine(exp1,exp2, _ * _, exp.c)
      case Division(exp1: Expression, exp2: Expression)        => combine(exp1,exp2, _ / _, exp.c)
      case Sum(exp1: Expression, exp2: Expression)             => combine(exp1,exp2, _ + _, exp.c)
      case Minus(exp1: Expression, exp2: Expression)           => combine(exp1,exp2, _ - _, exp.c)
      case Identifier(s: String)                               => identifier(s, exp.c, mode)
      case Let(id: Identifier, e1: Expression, e2: Expression) => let(id, e1, e2, exp.c, mode)
      case Ifz(cExp: Expression, zExp: Expression, nzExp: Expression) => intEval(cExp, exp.c) match  {
              // TODO: replace intEval by eval
        case Right(CExpression(Integer(v),_)) => if (v > 0) intEval(nzExp, exp.c) else intEval(zExp, exp.c)
        case x => x
      }
      case FunApp(funExp: Expression, valExp: Expression) => funApp(funExp, valExp, exp.c, mode)
      case Fun(id, funExp) => fun(id, funExp, exp.c, mode)
      case Fix(fId, Fun(Identifier(id),funExp)) => fix(fId, id, funExp, exp.c, mode)
      case Integer(i) => Right(CExpression(Integer(i), emptyContext))
      case NilList() => Right(CExpression(NilList(), emptyContext))
      case Cons(head, tail) => cons(head, tail, exp.c, mode)
      case Head(list) => head(list, exp.c, mode)
      case Tail(list) => tail(list, exp.c, mode)
      case Ifnil(cExp, nilExp, nNilExp) => listEval(cExp, exp.c) flatMap {
          case CExpression(NilList(), _)    => eval(CExpression(nilExp, exp.c), mode)
          case CExpression(Cons(_, _), _)   => eval(CExpression(nNilExp, exp.c), mode)
          case CExpression(_,_)             => Left(cExp.toString + notList)
      }
      case _ => Left(exp.toString + ": Unable to evaluate")
    }
  }

  sealed trait Mode

  case class CExpression(e:Expression, c:Context)

  case object ByValue extends Mode

  case object ByName extends Mode
}