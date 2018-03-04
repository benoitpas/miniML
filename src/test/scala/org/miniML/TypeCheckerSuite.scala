package org.miniML

import org.junit.runner.RunWith
import org.miniML.TypeChecker.{EType, F, Nat, V}
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.miniML.parser.{ExpressionParser, Integer}

@RunWith(classOf[JUnitRunner])
class TypeCheckerSuite extends FunSuite {


    test("integer") {
        val t = TypeChecker(Integer(0))
        assert( t == Right((TypeChecker.Nat(), Set())))
    }

    val ep = new ExpressionParser()

    def check(s: String, et: EType): Unit = {
        val t = TypeChecker(ep.parse(s).get)
        println(s)
        assert(t.isRight)
        println(t.right.get)
        assert(t.right.get._1 == et)
    }

    def checkFailure(s: String, error: String) {
        val t = TypeChecker(ep.parse(s).get)
        assert(t.swap.contains(error))
    }

    test("sum") {
        check("1+2", TypeChecker.Nat())
    }

    test("let with integers") {
        check("let a = 2 in a * 3", TypeChecker.Nat())
    }

    test("unknown identifier") {
        checkFailure("a * 2", "a not defined. ")
    }

    test("function with integers") {
        check("fun a -> a * 3", F(Nat(),Nat()))
    }

    test("identify function") {
        check("fun a -> a", F(V(1), V(1)))
    }

    test("identify function 2") {
        check("fun b -> fun a -> a", F(V(1),  F(V(2), V(2))))
    }

    test("identify function varible shadowing") {
        check("fun a -> fun a -> a", F(V(1),  F(V(2), V(2))))
    }

    test("addition function") {
        check("fun a -> fun b -> a+b",F(Nat(),F(Nat(),Nat())))
    }

    /*
    // TODO: Need to add function application to support that
    test("Y combinator") {
        check("fun f -> (fun x -> f (x x)) fun x -> f (x x)",Nat())
    }
    */
}
