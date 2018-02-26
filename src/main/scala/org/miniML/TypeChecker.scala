package org.miniML

import org.miniML.parser._

object TypeChecker {

    def apply(e: Expression): Either[String, EType] = apply(e, Map[Identifier, EType](), Set[(EType, EType)]())

    def apply(e: Expression, env: Map[Identifier, EType], equations: Set[(EType, EType)]): Either[String, EType] = {

        def operation(e1: Expression, e2: Expression): Either[String, EType] =
            (TypeChecker(e1, env, equations), TypeChecker(e2, env, equations)) match {
                case (Right(Nat()), Right(Nat())) => Right(Nat())
                case (Right(Nat()), Right(e)) => Left(e + " should be 'integer'.")
                case (Right(e), Right(_)) => Left(e + " should be 'integer'.")
                case (Left(s1), Left(s2)) => Left(s1 + s2)
                case (Left(s1), _) => Left(s1)
                case (_, Left(s2)) => Left(s2)
            }

        e match {
            case Integer(i) => Right(Nat())
            case Division(e1, e2) => operation(e1, e2)
            case Product(e1, e2) => operation(e1, e2)
            case Sum(e1, e2) => operation(e1, e2)
            case Minus(e1, e2) => operation(e1, e2)
            case Let(id, e1, e2) => TypeChecker(e1, env, equations).flatMap {
                case t => TypeChecker(e2, env + (id -> t), equations)
            }
            case Identifier(s) => env.get(Identifier(s)) match {
                case Some(t) => Right(t)
                case None => Left(s+" not defined. ")
            }
        }
    }

    sealed trait EType

    case class Nat() extends EType

    case class F(from: EType, to: EType) extends EType

    case class X(i:Int) extends EType
}
