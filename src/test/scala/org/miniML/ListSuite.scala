package org.miniML

import org.miniML.parser._
import org.scalatest.funsuite.AnyFunSuite
import EvalInterpretSuite.check

class ListSuite extends AnyFunSuite {

    implicit def list2MLList(l: List[Int]) : Expression = l match {
        case Nil => NilList()
        case head::tail => Cons(Integer(head),list2MLList(tail))
    }

    // TODO: need to fix type checker to support lists
    test("Empty list") {
        check("nil", List(), eType = None)
    }

    test("One element list") {
        check("cons 1 nil", List(1), eType = None)
    }

    test("Simple head test") {
        check("head cons 1 nil", Integer(1), eType = None)
    }

    test("Simple tail test") {
        check("tail cons 1 cons 2 nil", List(2), eType = None)
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
            List(3,2,1), eType = None)
    }

    test("reverse (tail terminating)") {
        val reverse = "let reverse = fun l -> (fix f fun l -> fun a -> ifnil l then a else f (tail l) cons (head l) a) l nil in "
        check(reverse + " reverse nil", List(), eType = None)
        check(reverse + " reverse (cons 1 nil)", List(1), eType = None)
        check(reverse + " reverse (cons 1 (cons 2 nil))", List(2,1), eType = None)
        check(reverse + " reverse (cons 1 (cons 2 (cons 3 nil)))", List(3,2,1), eType = None)
    }

    test("reverse function (not efficient implementation)") {
        val addLast = "let addLast = fix f fun e -> fun l -> (ifnil l then (cons e l) else (cons (head l) (f e (tail l)))) in "
        val reverse = "let reverse = fix f fun l2 -> ifnil l2 then nil else addLast (head l2) (f tail l2) in "
        check(addLast + reverse + " reverse nil", List(), eType = None)
        check(addLast + reverse + " reverse (cons 1 nil)", List(1), eType = None)
        check(addLast + reverse + " reverse (cons 1 (cons 2 nil))", List(2,1), eType = None)
        check(addLast + reverse + " reverse (cons 1 (cons 2 (cons 3 nil)))", List(3,2,1), eType = None)
    }

    //TODO: add sorting of integer list
    //TODO: add map function

}
