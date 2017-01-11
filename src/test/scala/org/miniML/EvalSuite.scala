

package org.miniML

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.scalatest.Finders

import org.miniML.parser._

@RunWith(classOf[JUnitRunner])
class EvalSuite extends FunSuite {
  test("addition") {
    val r = Eval("5+4")
    assert(r == Some(Integer(9)))
  }

  test("addition and free variable") {
    val r = Eval("5+coco")
    assert(r == Some(Sum(Integer(5), Identifier("coco"))))
  }

  test("addition and multiplication") {
    val r = Eval("5+4*2")
    assert(r == Some(Integer(13)))
  }

  test("multiplication and addition") {
    val r = Eval("5*4+2")
    assert(r == Some(Integer(22)))
  }
  
  test("simple let test") {
    val r = Eval("let y=5+(4*0) in y+1")
    assert(r == Some(Integer(6)))
  }

  test("simple nested let test") {
    val r = Eval("let v= 1 in let y=5+4*v in y+1")
    assert(r == Some(Integer(10)))
  }

  test("let test : free variable") {
    val r = Eval("let y=5+(4*v) in y+1")
    assert(r == Some(Sum(Sum(Integer(5),Product(Integer(4),Identifier("v"))),Integer(1))))
  }

  test("simple ifz test") {
    val r = Eval("ifz 1 then (let f = fun x-> (x x) in (f f)) else 0")
    assert(r.get == Integer(0))
  }

  test("ifz test with expressions") {
    val r = Eval("ifz 1 +1 then 10*2+5 else 10+5")
    assert(r.get == Integer(15))
  }
  
  test("ifz test with functions") {
    val r = Eval("ifz 0 then fun x -> x + 1 else fun x->x*x")
    assert(r.get == Fun(Identifier("x"),Sum(Identifier("x"),Integer(1))))
  }
 
  test("simple function test") {
      val r = Eval( "fun a -> a + 1")
    val e = Fun(Identifier("a"),Sum(Identifier("a"),Integer(1)))
    assert(r.get == e, s"ExpressionParser")    
  }

  test("double function test") {
    val r = Eval("fun a -> fun b -> b + a")
    val e = Fun(Identifier("a"), Fun(Identifier("b"), Sum(Identifier("b"), Identifier("a"))))
    assert(r.get == e, s"ExpressionParser")
  }

  test("function application 1") {
    val r = Eval( "fun a -> a + 1 2")
    val e = Integer(3)
    assert(r.get == e, s"ExpressionParser")
  }

  test("function application 2") {
    val r = Eval("let coco = fun x -> 2 * x in coco 10+0")
    val e = Integer(20)
    assert(r.get == e, s"ExpressionParser")
  }
  
  test("combining simple functions 1") {
    val r = Eval("let f = fun x -> x * 2 in let g = fun x -> x + 1 in (f g 1) + (g f 1)") 
    val e = Integer(4+3)
    assert(r.get == e, s"ExpressionParser")
  }

  test("combining simple functions 2") {
    val r = Eval("let f = fun x -> x * 2 in let g = fun x -> x + 1 in f g 1")
    val e = Integer(4)
    assert(r.get == e, s"ExpressionParser")
  }

  test("combine functions") {
    val r = Eval("let f = fun x -> x * 2 in let g = fun x -> x + 1 in let combine = fun f1 -> fun f2 -> fun x -> (f1 (f2 x)) in ((combine g) f) 1")
    val e:Integer = 3
    assert(r.get == e, s"ExpressionParser")
  }

  test("iFactorial test") {
    val fact0 = Eval(
      "let id = fun x -> x in let iFact = fun f -> fun n -> ifz n then 1 else (n * (f (n-1))) in ((iFact id) 0)")
    assert(fact0.get == Integer(1))
 
   val fact1 = Eval(
      "let id = fun x -> x in let iFact = fun f -> fun n -> ifz n then 1 else (n * (f (n-1))) in (iFact (iFact id)) 1")
    assert(fact1.get == Integer(1))

   val fact2 = Eval(
      "let id = fun x -> x in let iFact = fun f -> fun n -> ifz n then 1 else (n * (f (n-1))) in (iFact (iFact (iFact id))) 2")
    assert(fact2.get == Integer(2))
    
   val fact3 = Eval(
      "let id = fun x -> x in let iFact = fun f -> fun n -> ifz n then 1 else (n * (f (n-1))) in (iFact (iFact (iFact (iFact id)))) 3")
    assert(fact3.get == Integer(6))

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
    val fact0 = Eval.eval(FunApp(fact, Integer(0)), Map(), Eval.ByValue)
    assert(fact0 == Integer(1))
    val fact1 = Eval.eval(FunApp(fact, Integer(1)), Map(), Eval.ByValue)
    assert(fact1 == Integer(1))
    val fact5 = Eval.eval(FunApp(fact, Integer(5)), Map(), Eval.ByValue)
    assert(fact5 == Integer(1 * 2 * 3 * 4 * 5))
  }

  test("power function") {
    val yCombinator = Eval("fun f -> (fun x -> f (x x)) fun x -> f (x x)")
    val iPower = Eval("fun n -> (fun f2 -> (fun p -> ifz p then 1 else n * (f2 (p-1))))")
    //val p23 = EvalByValue("let yCombinator = fun f -> ((fun x -> f (x x)) (fun x -> f (x x))) in "
    //     + "let iPower = fun n -> (fun f2 -> (fun p -> ifz p then 1 else (n * (f2 (p-1))))) in "
    //     + "let power = fun n -> (fun p -> (yCombinator (iPower n) p)) in "
    //     + "((power 2 ) 3)").get
    val p23 = Eval.eval(
      Let(Identifier("power"), Fun(Identifier("n"), Fun(Identifier("p"), FunApp(FunApp(yCombinator.get, FunApp(iPower.get, Identifier("n"))), Identifier("p")))),
        FunApp(FunApp(Identifier("power"), Integer(2)), Integer(3))), Map(), Eval.ByValue)
    assert(p23 == Integer(2 * 2 * 2))
  }
}
