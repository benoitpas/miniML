package org.miniML

import org.miniML.parser._

object TypeChecker {

    type Equations = Set[(EType, EType)]
    type Environment = Map[Identifier, EType]

    def apply(e: Expression): Either[String, (EType, Equations)] = apply(e, Map[Identifier, EType](), Set[(EType, EType)]())

    def apply(e: Expression, env: Environment, equations: Equations): Either[String, (EType, Equations)] = {

        def newVariable(eq: Equations, env: Environment): EType = {
            def maxIndex(t: EType): Int = t match {
                case Nat() => 0
                case F(t1, t2) => Math.max(maxIndex(t1), maxIndex(t2))
                case X() => 0
                case V(i) => i
            }

            val nEquations = eq.foldLeft(0)((a, eq) => Math.max(a, Math.max(maxIndex(eq._1), maxIndex(eq._2))))
            val nEnvironment = env.foldLeft(0)((n, typedExp) => typedExp._2 match {
                case V(i) => Math.max(i, n)
                case _ => n
            })
            val n = Math.max(nEquations, nEnvironment)
            V(n + 1)
        }

        def addNatEquation(eq: Equations, e: Expression, t: EType) = addEquation(eq, e, t, Integer(0), Nat()) match {
            case Right(eq2) => Right(eq2)
            case _ => Left(e + " type should be integer. ")
        }

        def checkCompatibility(t1: EType, t2: EType) = (t1, t2) match {
            case (Nat(), F(_, _)) => false
            case (F(_, _), Nat()) => false
            case _ => true
        }

        def addEquation(eq: Equations, e1: Expression, t1: EType, e2: Expression, t2: EType): Either[String, Equations] =
            if (checkCompatibility(t1, t2))
                Right(if (t1 == t2) eq else eq + (t1 -> t2))
            else
                Left(e1 + " and " + e2 + " have incompatible types (" + t1 + " and " + t2 + "). ")

        def operation(e1: Expression, e2: Expression): Either[String, (EType, Equations)] = {

            (TypeChecker(e1, env, equations), TypeChecker(e2, env, equations)) match {
                case (Right((t1, eq1)), Right((t2, eq2))) => for {
                    eq3 <- addNatEquation(eq1 ++ eq2, e1, t1)
                    eq4 <- addNatEquation(eq3, e2, t2)
                } yield (Nat(), eq4)
                case (Left(s1), Left(s2)) => Left(s1 + s2)
                case (Left(s1), _) => Left(s1)
                case (_, Left(s2)) => Left(s2)
            }
        }

        def findVariableFromIndex(i: Int, equations: Equations) = equations.find(eq => eq._1 match {
            case V(i2) if i == i2 => true
            case _ => false
        }) flatMap (eq => Some(eq._2))

        def replaceVariable(t: EType, equations: Equations): EType = t match {
            case V(i) => findVariableFromIndex(i, equations) match {
                case Some(t2) => t2
                case _ => t
            }
            case F(t1,t2) => F(replaceVariable(t1,equations), replaceVariable(t2,equations))
            case _ => t
        }

        e match {
            case Integer(i) => Right(Nat(), Set())

            case Division(e1, e2) => operation(e1, e2)
            case Product(e1, e2) => operation(e1, e2)
            case Sum(e1, e2) => operation(e1, e2)
            case Minus(e1, e2) => operation(e1, e2)

            case Let(id, e1, e2) => TypeChecker(e1, env, equations).flatMap {
                case (t1, eq1) => TypeChecker(e2, env + (id -> t1), equations ++ eq1)
            }

            case Identifier(s) => env.get(Identifier(s)) match {
                case Some(t) => Right((t, Set()))
                case None => Left(s + " not defined. ")
            }

            case Ifz(cExp, zExp, nzExp) =>
                TypeChecker(cExp, env, equations) flatMap {
                    case (cType, cEquations) => TypeChecker(zExp, env, cEquations) flatMap {
                        case (zType, zEquations) => TypeChecker(nzExp, env, zEquations) flatMap {
                            case (nzType, nzEquations) =>  for {
                                eq2 <- addNatEquation(nzEquations, cExp, cType)
                                eq3 <- addEquation(eq2, zExp, zType, nzExp, nzType)
                            } yield (zType, eq3)
                        }
                    }
                }

            case Fun(idf, ef) =>
                val nv = newVariable(equations, env)
                TypeChecker(ef, env + (idf -> nv), equations).flatMap {
                    case (t, eq) => Right(replaceVariable(F(nv, t), eq), eq)
                }

            case FunApp(funExp, exp) => {
                def FunAppCheck(funExp: Expression, fType:EType, exp: Expression, eType: EType, equations: Equations) : Either[String, (EType, Equations)]
                = fType match {
                    case F(t1,t2) => addEquation(equations, funExp, t1, exp, eType) match {
                        case Right(rEquations) => Right(t2, rEquations)
                        case Left(s) => Left(s)
                    }
                    case V(i) =>
                        val newVar = newVariable(equations, env)
                        addEquation(equations, Integer(0),V(i),Integer(0),F(eType,newVar)) match {
                            case Right(rEquations) => Right(newVar,rEquations)
                            case Left(s) => Left(s)
                        }
                    case _ => Left(funExp +" should be am function. ")
                }

                TypeChecker(funExp, env, equations) flatMap {
                    case (fType, fEquations) => TypeChecker(exp, env, fEquations) flatMap {
                        case (eType, eEquations) => FunAppCheck(funExp, fType, exp, eType, eEquations)
                    }
                }
            }
        }
    }

    sealed trait EType

    case class Nat() extends EType

    case class F(from: EType, to: EType) extends EType {
        override def toString: String = "(" + from + " -> " + to + ")"
    }

    case class X() extends EType

    case class V(i: Int) extends EType

}
