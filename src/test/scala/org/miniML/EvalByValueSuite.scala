

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
}