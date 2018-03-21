package org.miniML

import org.miniML.parser._
import org.scalatest.FunSuite
import EvalInterpretSuite.check

class ListSuite extends FunSuite{

    // TODO: need to fix type checker to support lists
    test("Empty list") {
        check("nil", NilList(), eType = None)
    }

    test("One element list") {
        check("cons 1 nil", Cons(Integer(1), NilList()), eType = None)
    }

    test("Simple head test") {
        check("head cons 1 nil", Integer(1), eType = None)
    }

    test("Simple tail test") {
        check("tail cons 1 cons 2 nil", Cons(Integer(2), NilList()), eType = None)
    }

    test("Heterogeneous list evaluation") {
        check("head cons (1 + 1) cons (fun x->x x) nil", Integer(2), mode = Some(Eval.ByName), eType = None)
    }

    test("ifnil with empty nil") {
        check("ifnil nil then 0 else 1", Integer(0), eType = None)
    }

    test("ifnil with non-empty nil") {
        check("ifnil cons 1 nil then 0 else 1", Integer(1), eType = None)
    }

    test("add last") {
        val addLast = "let addLast = fix f fun e -> fun l -> (ifnil l then (cons e l) else (cons (head l) (f e (tail l)))) in "
        check(addLast + " addLast 1 nil", Cons(Integer(1),NilList()), eType = None)
        check(addLast + "let l = cons 3 (cons 2 nil) in addLast 1 l",
            Cons(Integer(3),Cons(Integer(2),Cons(Integer(1),NilList()))), eType = None)
    }

    //TODO: implement reverse
    test("reverse function") {
      val reverse = "fun l -> let "
    }

    //TODO: add sorting of integer list
    //TODO: add map function

}
