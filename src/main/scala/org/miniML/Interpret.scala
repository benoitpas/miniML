package org.miniML

import org.miniML.parser.{Expression, Fix, Fun, Identifier}

object Interpret extends EvalInterpretCommon {

    override type Context = Map[String, CExpression]

    override def emptyContext: Map[String, CExpression] = Map()

    def identifier(id: String, c: Context, mode: Mode): EExpression = c.get(id) match {
        case Some(ce) => eval(ce, mode)
        case None => Left(id + " not found.")
    }

    def let(id: Identifier, e1: Expression, e2: Expression, c: Context, mode: Mode): EExpression = mode match {
        case ByValue => eval(CExpression(e1,c), mode).flatMap(ce => eval(CExpression(e2,c + (id.s -> ce)), mode))
        case _ => eval(CExpression(e2, c + (id.s -> CExpression(e1,c))), mode)
    }

    def funApp(funExp: Expression, exp: Expression, c: Context, mode: Mode): EExpression = {

        def application(v: CExpression): EExpression = {
            eval(CExpression(funExp, c), mode).flatMap {
                case CExpression(Fun(id1, funExp1), c1) =>
                    val r = CExpression(funExp1, c1 + (id1.toString()-> v))
                    eval(r, mode)
                case _ => Left(funExp + ": Did not evaluate as a function")
            }
        }

        if (mode == Eval.ByValue) eval(CExpression(exp, c), mode).flatMap(application) else application(CExpression(exp, c))
    }

    def fun(id: Identifier, funExp: Expression, c: Context, mode: Mode): EExpression = Right(CExpression(Fun(id, funExp), c))

    def fix(fid: Identifier, id: Identifier, funExp: Expression, c: Context, mode: Mode): EExpression =
        Right(CExpression(Fun(id, funExp), c + (fid.toString() -> CExpression(Fix(fid, Fun(id, funExp)), c))))


}
