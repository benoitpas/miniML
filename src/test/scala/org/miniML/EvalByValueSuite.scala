

package org.miniML

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.Finders

import org.miniML.parser._

@RunWith(classOf[JUnitRunner])
class EvalByValueSuite extends FunSuite {
  test("addition") {
    val r = EvalByValue("5+4")
    assert(r == Some(Integer(9)))
  }

  test("addition and free variable") {
    val r = EvalByValue("5+coco")
    assert(r == Some(Sum(Integer(5), Identifier("coco"))))
  }

  test("addition and multiplication") {
    val r = EvalByValue("5+4*2")
    assert(r == Some(Integer(13)))
  }

  test("multiplication and addition") {
    val r = EvalByValue("5*4+2")
    assert(r == Some(Integer(22)))
  }
  
  test("simple let test") {
    val r = EvalByValue("let y=5+(4*0) in y+1")
    assert(r == Some(Integer(6)))
  }

  test("simple nested let test") {
    val r = EvalByValue("let v= 1 in let y=5+4*v in y+1")
    assert(r == Some(Integer(10)))
  }

  test("let test : free variable") {
    val r = EvalByValue("let y=5+(4*v) in y+1")
    assert(r == Some(Sum(Sum(Integer(5),Product(Integer(4),Identifier("v"))),Integer(1))))
  }

  test("simple ifz test") {
    val r = EvalByValue("ifz 0 then 10 else 0")
    assert(r.get == Integer(10))
  }

  test("ifz test with expressions") {
    val r = EvalByValue("ifz 1 +1 then 10*2+5 else 10+5")
    assert(r.get == Integer(15))
  }
  
  test("ifz test with functions") {
    val r = EvalByValue("ifz 0 then fun x -> x + 1 else fun x->x*x")
    assert(r.get == Fun(Identifier("x"),Sum(Identifier("x"),Integer(1))))
  }
 
  test("simple function test") {
      val r = EvalByValue( "fun a -> a + 1")
    val e = Fun(Identifier("a"),Sum(Identifier("a"),Integer(1)))
    assert(r.get == e, s"ExpressionParser")    
  }

  test("double function test") {
    val r = EvalByValue("fun a -> fun b -> b + a")
    val e = Fun(Identifier("a"), Fun(Identifier("b"), Sum(Identifier("b"), Identifier("a"))))
    assert(r.get == e, s"ExpressionParser")
  }

  test("function application 1") {
    val r = EvalByValue( "fun a -> a + 1 2")
    val e = Integer(3)
    assert(r.get == e, s"ExpressionParser")
  }

  test("function application 2") {
    val r = EvalByValue("let coco = fun x -> 2 * x in coco 10+0")
    val e = Integer(20)
    assert(r.get == e, s"ExpressionParser")
  }
  
  
  test("Y combinator") {
    val yCombinator = EvalByValue("fun f -> fun x -> f (x x) fun x -> f (x x)")
    val e1 = Fun(Identifier("f"), FunApp1(Fun(Identifier("x"), FunApp2(Identifier("f"), FunApp2(Identifier("x"), Identifier("x")))), Fun(Identifier("x"), FunApp2(Identifier("f"), FunApp2(Identifier("x"), Identifier("x"))))))
    assert(yCombinator.get == e1, s"ExpressionParser")
    //val iFactorial = EvalByValue("fun f ->fun n->ifz n then 1 else (n * (f (n-1)))")
    //val iFactorial = Fun(Identifier("f"),Fun(Identifier("n"),Ifz(Identifier("n"),Integer(0),Product(Identifier("n"),FunApp2(Identifier("f"),Minus(Identifier("n"),Integer(1)))))))
    //val f0 = FunApp1(FunApp1(e1,iFactorial),0)
    //assert(iFactorial == Integer(0), s"ExpressionParser")
  }
}