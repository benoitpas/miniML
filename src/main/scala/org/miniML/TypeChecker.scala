package org.miniML

import org.miniML.parser._

object TypeChecker {


    def apply(e: Expression): Either[String, EType] = e match {
        case Integer(i) => Right(Nat())
        case Division(e1, e2) => operation(e1, e2)
        case Product(e1, e2) => operation(e1, e2)
        case Sum(e1, e2) => operation(e1, e2)
        case Minus(e1, e2) => operation(e1, e2)
    }

    def operation(e1: Expression, e2: Expression): Either[String, EType] = (TypeChecker(e1), TypeChecker(e2)) match {
        case (Right(Nat()), Right(Nat())) => Right(Nat())
        case (Right(Nat()), Right(e)) => Left(e +" should be 'integer'.")
        case (Right(e), Right(_)) => Left(e +" should be 'integer'.")
        case (Left(s1), Left(s2)) => Left(s1 + s2)
        case (Left(s1), _) => Left(s1)
        case (_, Left(s2)) => Left(s2)
    }

    trait EType

    sealed case class Nat() extends EType

    sealed case class F(from: EType, to: EType) extends EType
}
