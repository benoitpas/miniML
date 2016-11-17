

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
    val r = EvalByValue("let v= 1 in let y=5+(4*v) in y+1")
    assert(r == Some(Integer(10)))
  }

  test("let test : free variable") {
    val r = EvalByValue("let y=5+(4*v) in y+1")
    assert(r == Some(Sum(Sum(Integer(5),Product(Integer(4),Identifier("v"))),Integer(1))))
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
    val r = EvalByValue("let coco = (fun x -> 2 * x) in coco 10+0")
    val e = Integer(20)
    assert(r.get == e, s"ExpressionParser")
  }
}