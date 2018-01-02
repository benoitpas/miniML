

package org.miniML.parser
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import org.miniML.parser.Integer._
import org.miniML.parser.Identifier._

@RunWith(classOf[JUnitRunner])
class ExpressionParserSuite extends FunSuite {

  val ep = new ExpressionParser()

  test("simple  test  1") {
    val r = ep.parse("5+4 *v")
    val e = Sum(Integer(5),Product(Integer(4),Identifier("v")))
    assert(r.get==e, s"ExpressionParser")
  }

  test("simple  test  2") {
    val r = ep.parse("4 * v + 0")
    val e = Sum( Product(Integer(4), Identifier("v")),Integer(0))
    assert(r.get == e, s"ExpressionParser")
  }
  
  test("tripple sum") {
    val r = ep.parse("1 + 2 + 3")
    val e = Sum(Sum(1, 2), 3)
    assert(r.get == e, s"ExpressionParser")
  }

  test("tripple product") {
    val r = ep.parse("1*2*3")
    val e = Product(Product(1, 2), 3)
    assert(r.get == e, s"ExpressionParser")
  }

  test("simple  test (with parenthesis)") {
    val r = ep.parse("5+( 4* v)")
    val e = Sum(Integer(5), Product(Integer(4), Identifier("v")))
    assert(r.get == e, s"ExpressionParser")
  }

  // Side effect of using 'StantardTokenParsers' from the standard lib
  test("test float number") {
    val r = ep.parse("3.14")
    val e = Integer(3)
    assert(r.get == e, s"ExpressionParser")
  }
  test("let test") {
    val r = ep.parse("let y=5+(4*v) in y+1")
    val e = Let(Identifier("y"),Sum(Integer(5), Product(Integer(4), Identifier("v"))),Sum(Identifier("y"),Integer(1)))
    assert(r.get == e, s"ExpressionParser")
  }
  
  test ("let in expression") {
    val r = ep.parse("5 * let y=5 in y+1")
    val e = Product(Integer(5),Let(Identifier("y"),Integer(5), Sum(Identifier("y"),Integer(1))))
    assert(r.get == e, s"ExpressionParser")    
  }
 
  test("simple ifz test") {
    val r = ep.parse("ifz 1 then 10 else 20")
    val e = Ifz(Integer(1), Integer(10), Integer(20))
    assert(r.get == e, s"ExpressionParser")    
  }

  test("ifz test with expressions") {
    val r = ep.parse("ifz 1 +1 then 10*2+5 else 10+5")
    val e = Ifz(Sum(Integer(1),Integer(1)), Sum(Product(Integer(10),Integer(2)),Integer(5)), Sum(Integer(10),Integer(5)))
    assert(r.get == e, s"ExpressionParser")    
  }

  test("ifz in expression") {
    val r = ep.parse("5 * ifz 0 then 1 else 2")
    val e = Product(Integer(5),Ifz(Integer(0),Integer(1),Integer(2)))
    assert(r.get == e, s"ExpressionParser")
  }
 
  test("function test") {
    val r = ep.parse("fun a -> a + 1")
    val e = Fun(Identifier("a"),Sum(Identifier("a"),Integer(1)))
    assert(r.get == e, s"ExpressionParser")    
  }

  test("double function test") {
    val r = ep.parse("fun a -> fun b -> b + a")
    val e = Fun(Identifier("a"), Fun(Identifier("b"), Sum(Identifier("b"), Identifier("a"))))
    assert(r.get == e, s"ExpressionParser")
  }

  test("function application 1") {
    val r = ep.parse("fun a -> a + 1 2")
    val e = FunApp(Fun(Identifier("a"), Sum(Identifier("a"), Integer(1))), Integer(2))
    assert(r.get == e, s"ExpressionParser")
  }

  test("function application 2") {
    val r = ep.parse("coco 10+o")
    val e = FunApp(Identifier("coco"), Sum(Integer(10),Identifier("o")))
    assert(r.get == e, s"ExpressionParser")
  }

  test("function application 3") {
    val r = ep.parse("f 1 2")
    val e = FunApp(FunApp("f", 1),2)
    assert(r.get == e, s"ExpressionParser")
  }

  test("function application 4") {
    val r = ep.parse("(f 1) 2")
    val e = FunApp(FunApp("f", 1), 2)
    assert(r.get == e, s"ExpressionParser")
  }

  test("function application 5") {
    val r = ep.parse("let f = fun w -> fun x -> fun y -> fun z -> w + x + y + z in f a b c d")
    val e = Let("f", Fun("w",Fun("x",Fun("y",Fun("z", Sum(Sum(Sum("w","x"),"y"),"z"))))),FunApp(FunApp(FunApp(FunApp("f","a"),"b"),"c"),"d"))

    assert(r.get == e, s"ExpressionParser")
  }

  test("function application associativity") {
    val r = ep.parse("let f = fun x -> x + 1 in f 1 + 2")
    val e = Let("f",Fun("x",Sum("x",1)), FunApp("f", Sum(1, 2)))
    assert(r.get == e, s"ExpressionParser")
  }

  test("Y combinator (no lazy parsing)") {
    val r = ep.parse("fun f -> (fun x -> f (x x)) fun x -> f (x x)")
    val e = Fun(Identifier("f"),FunApp(Fun(Identifier("x"),FunApp(Identifier("f"),FunApp(Identifier("x"),Identifier("x")))),Fun(Identifier("x"),FunApp(Identifier("f"),FunApp(Identifier("x"),Identifier("x"))))))
    assert(r.get == e, s"ExpressionParser")
  }

  test("Y combinator (for lazy parsing)") {
    val r = ep.parse("fun f -> f f")
    val e = Fun(Identifier("f"),FunApp(Identifier("f"),Identifier("f")))
    assert(r.get == e, s"ExpressionParser")
  }

  test("iFactorial (to be used with Y combinator") {
    val r = ep.parse("fun n -> (ifz n then 1 else (n * (f (n - 1))))")
    val e = Fun(Identifier("n"),Ifz(Identifier("n"),Integer(1),Product(Identifier("n"),FunApp(Identifier("f"),Minus(Identifier("n"),Integer(1))))))
    assert(r.get == e, s"ExpressionParser")
  }

  test("fix function") {
    val r = ep.parse("fix f fun n -> ifz n then 1 else n * (f n -1)")
    val e = Fix("f", Fun("n", Ifz("n", 1, Product("n", FunApp("f",Minus("n", 1))))))
    assert(r.get == e)
  }
}