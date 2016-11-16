

package org.miniML.parser
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ExpressionParserSuite extends FunSuite {

  test("simple  test  1") {
    val ep = new ExpressionParser()
    val r = ep.parse(ep.expression,"5+4 *v")
    val e = Sum(Integer(5),Product(Integer(4),Identifier("v")))
    assert(r.get==e, s"ExpressionParser")
  }

  test("simple  test  2") {
    val ep = new ExpressionParser()
    val r = ep.parse(ep.expression, "4 * v + 0")
    val e = Sum( Product(Integer(4), Identifier("v")),Integer(0))
    assert(r.get == e, s"ExpressionParser")
  }

  test("simple  test (with parenthesis)") {
    val ep = new ExpressionParser()
    val r = ep.parse(ep.expression, "5+( 4* v)")
    val e = Sum(Integer(5), Product(Integer(4), Identifier("v")))
    assert(r.get == e, s"ExpressionParser")
  }

  test("let test") {
    val ep = new ExpressionParser()
    val r = ep.parse(ep.expression, "let y=5+(4*v) in y+1")
    val e = Let(Identifier("y"),Sum(Integer(5), Product(Integer(4), Identifier("v"))),Sum(Identifier("y"),Integer(1)))
    assert(r.get == e, s"ExpressionParser")
  }
}