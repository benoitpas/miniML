package org.miniML

import org.miniML.parser._

object TypeChecker {

    type Equations = Set[(EType, EType)]

    def apply(e: Expression): Either[String, (EType,Equations)] = apply(e, Map[Identifier, EType](), Set[(EType, EType)]())

    def apply(e: Expression, env: Map[Identifier, EType], equations: Equations): Either[String, (EType, Equations)] = {

        def newVariable(eq: Equations): EType = {
            def maxIndex(t: EType): Int = t match {
                case Nat() => 0
                case F(t1, t2) => Math.max(maxIndex(t1), maxIndex(t2))
                case X() => 0
                case V(i) => i
            }

            val n = eq.foldLeft(0)((a, eq) => Math.max(a, Math.max(maxIndex(eq._1), maxIndex(eq._2))))
            V(n + 1)
        }

        def operation(e1: Expression, e2: Expression): Either[String, (EType, Equations)] = {
            def addEquations(eq: Equations, t1: EType, t2: EType) = {
                val v1 = newVariable(eq)
                val eq2 = eq ++ Set((v1, Nat()))
                val v2 = newVariable(eq2)
                eq2 ++ Set((v2, Nat()))
            }

            (TypeChecker(e1, env, equations), TypeChecker(e2, env, equations)) match {
                case (Right((t1, eq1)), Right((t2, eq2))) => Right((Nat(), addEquations(eq1 ++ eq2, t1, t2)))
                case (Right((Nat(), _)), Right((t, _))) => Left(t + " should be 'integer'.")
                case (Right((t, _)), Right(_)) => Left(t + " should be 'integer'.")
                case (Left(s1), Left(s2)) => Left(s1 + s2)
                case (Left(s1), _) => Left(s1)
                case (_, Left(s2)) => Left(s2)
            }
        }

        def findVariableFromIndex(i:Int, equations: Equations) = equations.find(eq => eq._1 match {
            case V(i2) if i == i2 => true
            case _ => false
        }) flatMap( eq => Some(eq._2))

        def findVariable(t: EType, equations: Equations) : EType = t match {
            case V(i) => findVariableFromIndex(i, equations) match {
                case Some(t2) => t2
                case _ => t
            }
            case _ => t
        }

        e match {
            case Integer(i) => Right(Nat(), Set())
            case Division(e1, e2) => operation(e1, e2)
            case Product(e1, e2) => operation(e1, e2)
            case Sum(e1, e2) => operation(e1, e2)
            case Minus(e1, e2) => operation(e1, e2)
            case Let(id, e1, e2) =>
                TypeChecker(e1, env, equations).flatMap {
                    case (t1, eq1) => TypeChecker(e2, env + (id -> t1), equations ++ eq1)
                }

            case Identifier(s) => env.get(Identifier(s)) match {
                case Some(t) => Right((t, Set()))
                case None => Left(s + " not defined. ")
            }
            case Fun(idf, ef) =>
                val nv = newVariable(equations)
                TypeChecker(ef, env + (idf -> nv), equations).flatMap {
                    case (t, eq) => Right(F(findVariable(nv,eq), t), eq)
                }

        }
    }

    sealed trait EType

    case class Nat() extends EType

    case class F(from: EType, to: EType) extends EType {
        override def toString: String = "(" + from + " -> " + to + ")"
    }

    case class X() extends EType

    case class V(i:Int) extends EType
}
