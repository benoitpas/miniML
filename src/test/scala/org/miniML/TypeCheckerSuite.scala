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

    def check(s:String, et:EType) : Unit = {
        val t = TypeChecker(ep.parse(s).get)
        assert(t.contains(et))
    }

    test("sum") {
        check("2+2", TypeChecker.Nat())
    }
}
