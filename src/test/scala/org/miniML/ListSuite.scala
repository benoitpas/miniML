package org.miniML

import org.miniML.parser._
import org.scalatest.FunSuite
import EvalInterpretSuite.check

class ListSuite extends FunSuite{

    test("Empty list") {
        check("nil", NilList(), eType = None)
    }

    test("One element list") {
        check("cons 1 nil", Cons(Integer(1), NilList()), eType = None)
    }

    test("Simple head test") {
        check("head cons 1 nil", Integer(1), eType = None)
    }

    test("Lazy list evaluation") {
        check("head cons (1 + 1) cons ((fun x->x x) 1) nil", Integer(2), mode = Some(Eval.ByName), eType = None)
    }

}
