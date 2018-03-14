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
        assert(t == Right((TypeChecker.Nat(), Set())))
    }

    val ep = new ExpressionParser()

    def check(s: String, et: EType): Unit = {
        val t = TypeChecker(ep.parse(s).get)
        println(s)
        if (t.isLeft) {
            println(t.left.get)
        }
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
        check("fun a -> a * 3", F(Nat(), Nat()))
    }

    test("identify function") {
        check("fun a -> a", F(V(1), V(1)))
    }

    test("identify function 2") {
        check("fun b -> fun a -> a", F(V(1), F(V(2), V(2))))
    }

    test("identify function varible shadowing") {
        check("fun a -> fun a -> a", F(V(1), F(V(2), V(2))))
    }

    test("addition function") {
        check("fun a -> fun b -> a+b", F(Nat(), F(Nat(), Nat())))
    }

    test("simple ifz") {
        check("ifz 0 then 1 else 5", Nat())
    }

    test("ifz with fun") {
        check("fun x -> ifz x then x else x", F(Nat(), Nat()))
    }

    test("incorrect ifz") {
        checkFailure("ifz 0 then 1 else fun x -> 1","1 and fun x -> 1 have incompatible types (Nat() and (V(1) -> Nat())). ")
    }

    test("simple function application") {
        check("let inc = fun x -> x + 1 in inc 2", Nat())
    }

    test("id function application") {
        check("(fun x -> x) 1", Nat())
    }

    // TODO: Should be 'Nat() ? or doesn't work with polymorphic functions ?'
    test("combine functions") {
        check("let f = fun x1 -> 0 in let g = fun x2 -> 0 in let combine = fun f1 -> fun f2 -> fun x -> (f1 (f2 x)) in ((combine g) f) 1", V(5))
    }

    test("combine functions 2") {
        check("let f = fun x1 -> x1 + 1 in let g = fun x2 -> x2 * 2 in let combine = fun f1 -> fun f2 -> fun x -> (f1 (f2 x)) in ((combine g) f)", V(5))
    }


    test("Y combinator") {
        checkFailure("fun f -> (fun x -> f (x x)) fun x -> f (x x)", "(x x) is not typable. ")
    }

}
