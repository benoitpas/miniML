package org.miniML

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

import org.miniML.parser.Integer

@RunWith(classOf[JUnitRunner])
class TypeCheckerSuite extends FunSuite {

    test("integer") {
        val t = TypeChecker(Integer(0))
        assert( t == Right(TypeChecker.Nat()))
    }
}
