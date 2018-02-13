package org.miniML

import org.miniML.parser.{Expression, Identifier}

/**
  * Created by arundhathi on 03/02/18.
  */
object Interpret extends EvalCommon {

    override type Context = Map[String, CExpression]

    override def emptyContext: Map[String, CExpression] = Map()

    def identifier(id: String, c: Context, mode: Mode): EExpression = c.get(id) match {
        case Some(ce) => Right(ce)
        case None => Left(id + " not found.")
    }

    def let(id: Identifier, e1: Expression, e2: Expression, c: Context, mode: Mode): EExpression = mode match {
        case ByValue => eval(CExpression(e1,c), mode).flatMap(ce => Right(CExpression(e2,c + (id.s -> ce))))
        case _ => Right(CExpression(e2, c + (id.s -> CExpression(e1,c))))
    }

    def funApp(funExp: Expression, exp: Expression, c: Context, mode: Mode): EExpression = ???

    def fun(id: Identifier, funExp: Expression, c: Context, mode: Mode): EExpression = ???

    def fix(fid: Identifier, id: Identifier, funExp: Expression, c: Context, mode: Mode): EExpression = ???

}
