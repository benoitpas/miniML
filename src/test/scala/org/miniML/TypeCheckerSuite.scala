package org.miniML

import org.junit.runner.RunWith
import org.miniML.TypeChecker.EType
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.miniML.parser.{ExpressionParser, Integer}

@RunWith(classOf[JUnitRunner])
class TypeCheckerSuite extends FunSuite {


    test("integer") {
        val t = TypeChecker(Integer(0))
        assert( t == Right(TypeChecker.Nat()))
    }

    val ep = new ExpressionParser()

    def check(s: String, et: EType): Unit = {
        val t = TypeChecker(ep.parse(s).get)
        assert(t.contains(et))
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

}
