

package org.miniML.parser
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ExpressionParserSuite extends FunSuite {

    val ep = new ExpressionParser()

    test("simple  test  1") {
    val r = ep.parse(ep.expression,"5+4 *v")
    val e = Sum(Integer(5),Product(Integer(4),Identifier("v")))
    assert(r.get==e, s"ExpressionParser")
  }

  test("simple  test  2") {
    val r = ep.parse(ep.expression, "4 * v + 0")
    val e = Sum( Product(Integer(4), Identifier("v")),Integer(0))
    assert(r.get == e, s"ExpressionParser")
  }
  
  test("tripple sum") {
    val r = ep.parse(ep.expression, "1 + 2 + 3")
    val e = Sum(Integer(1), Integer(2)) // 3 is missing
    assert(r.get == e, s"ExpressionParser")
    
  }

  test("simple  test (with parenthesis)") {
    val r = ep.parse(ep.expression, "5+( 4* v)")
    val e = Sum(Integer(5), Product(Integer(4), Identifier("v")))
    assert(r.get == e, s"ExpressionParser")
  }

  test("let test") {
    val r = ep.parse(ep.expression, "let y=5+(4*v) in y+1")
    val e = Let(Identifier("y"),Sum(Integer(5), Product(Integer(4), Identifier("v"))),Sum(Identifier("y"),Integer(1)))
    assert(r.get == e, s"ExpressionParser")
  }
  
  test("function test") {
    val r = ep.parse(ep.expression, "fun a -> a + 1")
    val e = Fun(Identifier("a"),Sum(Identifier("a"),Integer(1)))
    assert(r.get == e, s"ExpressionParser")    
  }

  test("double function test") {
    val r = ep.parse(ep.expression, "fun a -> fun b -> b + a")
    val e = Fun(Identifier("a"), Fun(Identifier("b"), Sum(Identifier("b"), Identifier("a"))))
    assert(r.get == e, s"ExpressionParser")
  }

  test("function application 1") {
    val r = ep.parse(ep.expression, "fun a -> a + 1 2")
    val e = FunApp1(Fun(Identifier("a"), Sum(Identifier("a"), Integer(1))), Integer(2))
    assert(r.get == e, s"ExpressionParser")
  }
  test("function application 2") {
    val r = ep.parse(ep.expression, "coco 10+o")
    val e = FunApp2(Identifier("coco"), Sum(Integer(10),Identifier("o")))
    assert(r.get == e, s"ExpressionParser")
  }

}