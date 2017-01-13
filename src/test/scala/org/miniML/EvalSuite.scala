

package org.miniML

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.scalatest.Finders

import org.miniML.parser._
import org.miniML.parser.Integer._
import org.miniML.parser.Identifier._

@RunWith(classOf[JUnitRunner])
class EvalSuite extends FunSuite {

  def check(e: String, exp: Expression) = {
    val r1 = Eval(e, Eval.ByName)
    val r2 = Eval(e, Eval.ByValue)
    assert(r1 == Some(exp))
    assert(r2 == Some(exp))
  }
  
  test("addition") {
    check("5+4", 9)
  }

  test("addition and free variable") {
    check("5+coco", Sum(5, "coco"))
  }

  test("addition and multiplication") {
    check("5+4*2", 13)
  }

  test("multiplication and addition") {
    check("5*4+2", 22)
  }

  test("simple let test") {
    check("let y=5+(4*0) in y+1", 6)
  }

  test("simple nested let test") {
    check("let v= 1 in let y=5+4*v in y+1", 10)
  }

  test("let test : free variable") {
    check("let y=5+(4*v) in y+1", Sum(Sum(5, Product(4, "v")), 1))
  }

  test("simple ifz test") {
    check("ifz 1 then (let f = fun x-> (x x) in (f f)) else 0", 0)
  }

  test("ifz test with expressions") {
    check("ifz 1 +1 then 10*2+5 else 10+5", 15)
  }
  
  test("ifz test with functions") {
    check("ifz 0 then fun x -> x + 1 else fun x->x*x", Fun("x", Sum("x", 1)))
  }
 
  test("simple function test") {
    check("fun a -> a + 1", Fun("a", Sum("a", 1)))
  }

  test("double function test") {
    check("fun a -> fun b -> b + a", Fun("a", Fun("b", Sum("b", "a"))))
  }

  test("function application 1") {
    check("fun a -> a + 1 2", 3)
  }

  test("function application 2") {
    check("let coco = fun x -> 2 * x in coco 10+0",20)
  }
  
  test("combining simple functions 1") {
    check("let f = fun x -> x * 2 in let g = fun x -> x + 1 in (f g 1) + (g f 1)", 4 + 3)
  }

  test("combining simple functions 2") {
    check("let f = fun x -> x * 2 in let g = fun x -> x + 1 in f g 1", 4)
  }

  test("combine functions") {
    check("let f = fun x -> x * 2 in let g = fun x -> x + 1 in let combine = fun f1 -> fun f2 -> fun x -> (f1 (f2 x)) in ((combine g) f) 1",3)
  }

  test("iFactorial test") {
    check("let id = fun x -> x in let iFact = fun f -> fun n -> ifz n then 1 else (n * (f (n-1))) in ((iFact id) 0)", 1)

    check("let id = fun x -> x in let iFact = fun f -> fun n -> ifz n then 1 else (n * (f (n-1))) in (iFact (iFact id)) 1", 1)

    check("let id = fun x -> x in let iFact = fun f -> fun n -> ifz n then 1 else (n * (f (n-1))) in (iFact (iFact (iFact id))) 2", 2)

    check("let id = fun x -> x in let iFact = fun f -> fun n -> ifz n then 1 else (n * (f (n-1))) in (iFact (iFact (iFact (iFact id)))) 3", 6)

  }

  def check(e1: Expression, e2: Expression) = {
    val byValue = Eval.eval(e1, Map(), Eval.ByValue)
    val byName = Eval.eval(e1, Map(), Eval.ByValue)
    assert(byValue == e2)
    assert(byName == e2)
  }
  
  test("Y combinator") {
    val yCombinator = Eval("fun f -> (fun x -> f (x x)) fun x -> f (x x)")
    val e1 = Fun(Identifier("f"), FunApp(Fun(Identifier("x"), FunApp(Identifier("f"), FunApp(Identifier("x"), Identifier("x")))), Fun(Identifier("x"), FunApp(Identifier("f"), FunApp(Identifier("x"), Identifier("x"))))))
    assert(yCombinator.get == e1, s"ExpressionParser")
    val iFactorial = Eval("fun f2 -> fun n -> ifz n then 1 else (n * (f2 (n-1)))")
    val e2 = Fun(Identifier("f2"),Fun(Identifier("n"),Ifz(Identifier("n"),Integer(1),Product(Identifier("n"),FunApp(Identifier("f2"),Minus(Identifier("n"),Integer(1)))))))
    assert(iFactorial == Some(e2), s"ExpressionParser")
    val fact = FunApp(yCombinator.get, iFactorial.get)
    println

    check(FunApp(fact, Integer(0)), Integer(1))
    check(FunApp(fact, Integer(1)), Integer(1))
    check(FunApp(fact, Integer(5)), Integer(1 * 2 * 3 * 4 * 5))
  }

  test("power function") {
//   val ep = new ExpressionParser()
//    ep.parse(ep.expression,"fun f -> (fun x -> f (x x)) fun x -> f (x x)")
//    ep.parse(ep.expression,"fun n-> fun f2 -> (fun p -> ifz p then 1 else n * (f2 (p-1)))")
//    ep.parse(ep.expression,"fun n -> (fun f2 -> (fun p -> ifz p then 1 else n * (f2 (p-1))))")
    val yCombinator = Eval("fun f -> (fun x -> f (x x)) fun x -> f (x x)")
    val iPower = Eval("fun n -> (fun f2 -> (fun p -> ifz p then 1 else n * (f2 (p-1))))")
    //val p23 = EvalByValue("let yCombinator = fun f -> ((fun x -> f (x x)) (fun x -> f (x x))) in "
    //     + "let iPower = fun n -> (fun f2 -> (fun p -> ifz p then 1 else (n * (f2 (p-1))))) in "
    //     + "let power = fun n -> fun p -> (yCombinator (iPower n) p) in "
    //     + "((power 2 ) 3)").get
    check(
      Let(Identifier("power"), Fun(Identifier("n"), Fun(Identifier("p"), FunApp(FunApp(yCombinator.get, FunApp(iPower.get, Identifier("n"))), Identifier("p")))),
        FunApp(FunApp(Identifier("power"), Integer(2)), Integer(3))), Integer(2 * 2 * 2))
  }
}
