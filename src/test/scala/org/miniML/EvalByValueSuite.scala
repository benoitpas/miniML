

package org.miniML

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.Finders

@RunWith(classOf[JUnitRunner])
class EvalByValueSuite extends FunSuite {
  test("addition") {
    val r = EvalByValue("5+4")
    assert(r == Some(9))
  }

  test("addition and multiplication") {
    val r = EvalByValue("5+4*2")
    assert(r == Some(13))
  }

  test("multiplication and addition") {
    val r = EvalByValue("5*4+2")
    assert(r == Some(22))
  }
  
  test("simple let test") {
    val r = EvalByValue("let y=5+(4*0) in y+1")
    assert(r == Some(6))
  }

  test("simple nested let test") {
    val r = EvalByValue("let v= 1 in let y=5+(4*v) in y+1")
    assert(r == Some(10))
  }

  test("let test : undefined identifier") {
    assertThrows[NoSuchElementException] {
    val r = EvalByValue("let y=5+(4*v) in y+1")
  }
  }
}