

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
}