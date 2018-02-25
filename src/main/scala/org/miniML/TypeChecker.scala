package org.miniML

import org.miniML.parser.Expression
import org.miniML.parser.Integer

object TypeChecker {

    def apply(e: Expression): Either[String, EType] = e match {
        case Integer(i) => Right(Nat())
    }

    trait EType

    sealed case class Nat() extends EType

    sealed case class F(from:EType,to:EType) extends EType
}
