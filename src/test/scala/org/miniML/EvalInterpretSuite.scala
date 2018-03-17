

package org.miniML

import org.junit.runner.RunWith
import org.miniML.TypeChecker.{EType, F, Nat, V}
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.miniML.parser._
import org.miniML.parser.Integer._
import org.miniML.parser.Identifier._

@RunWith(classOf[JUnitRunner])
class EvalInterpretSuite extends FunSuite {

  val ep = new ExpressionParser()

  def check(e: String, exp: Expression, mode: Option[Eval.Mode] = None, eType: Option[EType] = Some(Nat())) {

    val t = TypeChecker(ep.parse(e).get)
    eType match {
      case Some(et) => assert(t.isRight && t.right.get._1 == et)
      case None => assert(t.isLeft)
    }

    if (mode.isEmpty || mode.contains(Eval.ByName)) {
      val r1 = Eval(e, Eval.ByName)
      eAssert(r1, exp)
      val r2 = Interpret(e, Interpret.ByName)
      iAssert(r2, exp)
    }
    if (mode.isEmpty || mode.contains(Eval.ByValue)) {
      val r1 = Eval(e, Eval.ByValue)
      eAssert(r1, exp)
      val r2 = Interpret(e, Interpret.ByValue)
      iAssert(r2, exp)
    }
  }

  def eAssert(value:Either[String,Eval.CExpression], expected:Expression): Unit = value match {
      case Right(Eval.CExpression(vExp,_)) => assert(vExp == expected)
      case Left(s) => assert(false, s)
    }

  def iAssert(value:Either[String, Interpret.CExpression], expected:Expression): Unit =
    value match {
      case Right(Interpret.CExpression(vExp,_)) =>assert(  vExp == expected)
      case Left(s) => assert(false, s)
    }

  def checkFailure(e:String, error:String, mode:Option[Eval.Mode] = None) {
    if (mode.isEmpty || mode.contains(Eval.ByName)) {
      val r1 = Eval(e, Eval.ByName)
      assert(r1 == Left(error))
    }
    if (mode.isEmpty || mode.contains(Eval.ByValue)) {
      val r2 = Eval(e, Eval.ByValue)
      assert(r2 == Left(error))
    }
  }


  test("failed parsing") {
    checkFailure(")", "Parsing Failed: [1.1] failure: '`('' expected but `)' found\n\n)\n^")
  }

  test("addition") {
    check("5+4", 9)
  }

  test("addition and free variable") {
    checkFailure("5+coco", "coco: Unknown identifier")
  }

  test("addition and multiplication") {
    check("5+4*2", 13)
  }

  test("multiplication and addition") {
    check("5*4+2", 22)
  }

  test("substraction and addition") {
    check("2-1+1", 2)
    check("2-(1+1)", 0)
  }

  test("division") {
    check("13/3", 4)
  }

  test("simple let test") {
    check("let y=5+(4*0) in y+1", 6)
  }

  test("simple let test (call by name)") {
    // (x x) is not typable
    check("let f = fun x-> (x x) in let v = f f in 1", 1, Some(Eval.ByName), None)
  }

  test("simple nested let test") {
    check("let v= 1 in let y=5+4*v in y+1", 10)
  }

  test("let test : free variable") {
    checkFailure("let y=5+(4*v) in y+1", "v: Unknown identifier")
  }

  test("simple ifz test") {
    check("ifz 1 then (let f = fun x-> (x x) in (f f)) else 0", 0, eType = None)
  }

  test("ifz test with expressions") {
    check("ifz 1 +1 then 10*2+5 else 10+5", 15)
  }
  
  test("ifz test with functions") {
    checkFailure("ifz 0 then fun x -> x + 1 else fun x->x*x", "fun x -> x + 1: Cannot be evaluated as an integer")
  }
 
  test("simple function test") {
    check("fun a -> a + 1", Fun("a", Sum("a", 1)), eType = Some(F(Nat(), Nat())))
  }

  test("double function test") {
    check("fun a -> fun b -> b + a", Fun("a", Fun("b", Sum("b", "a"))), eType=Some(F(Nat(), F(Nat(), Nat()))))
  }

  test("function application 1") {
    check("fun a -> a + 1 2", 3)
  }

  test("variable shadowing 1") {
    check("(fun x -> fun x -> x) 2 3", 3)
  }

  test("variable shadowing 2") {
    check("(fun x -> fun y -> fun x-> x+y x*2) 5 4", 14)
  }

  test("variable shadowing 3") {
    check("(fun x -> x + 10 * ((fun x -> x + 10 * ((fun x -> x) 1)) 2)) 3", 123)
  }

  test("variable shadowing 4") {
    check("let x = 3 in x + 10 * let x = 2 in x + 10 * let x = 1 in x", 123)
  }

  test("variable shadowing 5") {
    check("let x = 5 in x + 10 * let y = 4 in y + 10 * let x = 3 in x + 10 * let y = 2 in y + 10 * let x = 1 in x", 12345)
  }

  test("variable shadowing 6") {
    check("let f = fun x -> x*x*x in let f = fun x -> x*x in let f = fun x -> x in f 2", 2)
  }

  test("function application 2") {
    check("let coco = fun x -> 2 * x in coco 10+0", 20)
  }

  test("function application 3") {
    check("let f = fun x -> fun y -> x - y in (f 1) 2", -1)
  }

  test("function application 4") {
    check("let f = fun x -> fun y -> x + y in f 1 2", 3)
  }

  test("function application 5") {
    check("let f = fun x -> fun y -> fun z -> x + y * z in f 1 2 3", 7)
  }

  test("function application 6") {
    check("let f = fun w -> fun x -> fun y -> fun z -> w * x + y * z in f 1 2 3 4", 14)
  }

  // This would not finish when evaluated by value
  test("function for call by name") {
    check("let f1 = fun a -> 1 in let f2 = fun x-> (x x) in 2 + (f1 (f2 f2))", 3, Some(Eval.ByName), None)
  }

  test("combining simple functions 1") {
    check("let f = fun x -> x * 2 in let g = fun x -> x + 1 in (f (g 1)) + (g (f 1))", 4 + 3)
  }

  test("combining simple functions 2") {
    check("let f = fun x -> x * 2 in let g = fun x -> x + 1 in f (g 1)", 4)
  }

  test("combine functions") {
      // TODO: Type should be 'Nat()'
    check("let f = fun x -> x * 2 in let g = fun x -> x + 1 in let combine = fun f1 -> fun f2 -> fun x -> (f1 (f2 x)) in combine g f 1",
        3, eType = Some(V(6)))
  }

  test("iFactorial test") {
    check("let id = fun x -> x in let iFact = fun f -> fun n -> ifz n then 1 else (n * (f n-1)) in iFact id 0", 1)

    check("let id = fun x -> x in let iFact = fun f -> fun n -> ifz n then 1 else (n * (f n-1)) in iFact (iFact id) 1", 1)

    check("let id = fun x -> x in let iFact = fun f -> fun n -> ifz n then 1 else (n * (f n-1)) in iFact (iFact (iFact id)) 2", 2)

    check("let id = fun x -> x in let iFact = fun f -> fun n -> ifz n then 1 else (n * (f n-1)) in iFact (iFact (iFact (iFact id))) 3", 6)

  }
  
  test("factorial (with Y combinator)") {
    val fact2 =
        "let yCombinator = fun f -> (fun x -> f (x x)) fun x -> f (x x) in " +
        "let iFactorial = fun f2 -> fun n -> ifz n then 1 else (n * (f2 (n-1))) in " +
        "let fact = yCombinator iFactorial in fact "

    check(fact2 + "0",  1, eType = None)
    check(fact2 + "1",  1, eType = None)
    check(fact2 + "5",  1*2*3*4*5, eType = None)
  }

  test("power function (With Y combinator)") {
    check("let yCombinator = fun f -> (fun x -> f (x x)) fun x -> f (x x) in "
        + "let iPower = fun n -> fun f2 -> fun p -> ifz p then 1 else n * (f2 p - 1) in "
        + "let power = fun n -> fun p -> (yCombinator (iPower n) p) in "
        + "power 2 3", Integer(2*2*2), eType = None)
  }
  
  test("factorial (with fix function)") {
    val fact = "(fix f fun n -> (ifz n then 1 else (n * (f (n -1)))))"
    check(fact + " 1", Integer(1))
    check(fact + " 5", Integer(1 * 2 * 3 * 4 * 5))
  }

  test("check if a number is prime") {
    val isPrime = "let isPrime = (fun n -> (fix f (fun i -> ifz n-i then 1 else ifz (n-n/i*i) then 0 else f (i + 1))) 2) in isPrime "
    check(isPrime + "2", 1)
    check(isPrime + "3", 1)
    check(isPrime + "4", 0)
    check(isPrime + "5", 1)
    check(isPrime + "9", 0)
    check(isPrime + "13", 1)
    check(isPrime + "34", 0)
  }

  test("Cantor's numbers") {
    val K = "(fun n -> fun p -> (n+p)*(n+p+1)/2+n) "
    val findu = "(fun q -> (fix f fun i -> ifz i*(i+1)/2 - q then i else f i - 1) q/2) "
    val Ki1 = "(fun q -> fun u -> q-u*(u+1)/2) "
    val Ki2 = "(fun q -> fun u -> u-q+u*(u+1)/2) "
    val L = "let K=" + K + "in fun n -> fun p -> (K n p) + 1"

    check(K + "1 2", 7)
    check(K + "2 1", 8)
    check(findu + "8", 3)

    // K 1 3 = 11
    check(K + "1 3", 11)
    check(findu + "11", 4)
    check(Ki1 + "11 4", 1)
    check(Ki2 + "11 4", 3)

    // K 7 11 = 178
    check(K + "7 11", 178)
    check(findu + "178", 18)
    check(Ki1 + "178 18", 7)
    check(Ki2 + "178 18", 11)

    // polynom x^2+2x+5 value:
    val pValue = "let L=" + L + "in L 5 (L 2 (L 1 0))"
    check(pValue, 282)

    // extract values from polynom
    val nextCoef = "(fun v -> let u=" + findu + " v in " + Ki1 + " (v-1) u) "
    val nextAcc = "(fun v -> let u=" + findu + " v in " + Ki2 + " (v-1) u) "
    check(nextCoef + "282", 5)
    check(nextAcc + "282", 18)
    check(nextCoef + "18", 2)
    check(nextAcc + "18", 3)
    check(nextCoef + "3", 1)
    check(nextAcc + "3", 0)

    // Evaluate polynom coded as 282 with x=13
    val evalPolygon = "(let findu=" + findu +" in let Ki1=" + Ki1 + " in let Ki2=" + Ki2 + "in " +
        "(fun pValue -> fun x -> (fix f fun pValue -> fun xn -> fun ret -> ifz pValue then ret else " +
        "let pValueDec = pValue-1 in let u = findu pValueDec in f (Ki2 pValueDec u) xn*x xn*(Ki1 pValueDec u)+ret) pValue 1 0)) "
    check(evalPolygon+" 282 13", 13*13+2*13+5)
  }

  test("Sum of squares") {
    val sumSquares = "(fun n -> (fix f fun i -> fun j -> ifz i then j else f (i-1) j+i*i) n 0) "
    check(sumSquares + "2", 1 + 2 * 2)
    check(sumSquares + "3", 1 + 2 * 2 + 3 * 3)
  }

    test("Church encoding of boolean numbers") {
        val eTrue = Fun("a",Fun("b","a"))
        val eFalse = Fun("a",Fun("b","b"))
        val e = "let true = fun a -> fun b -> a in " +
                "let false = fun a -> fun b -> b in " +
                "let not = fun p -> p false true in " +
                "let and = fun a -> fun b -> a b false in " +
                "let or = fun a -> fun b -> a true b in "

        check(e + " not true", eFalse, eType = Some(V(7)))
        check(e + " not false",eTrue, eType = Some(V(7)))
        check(e + " and true true", eTrue, eType = Some(V(11)))
        check(e + " and true false", eFalse, eType = Some(V(11)))
        check(e + " and false true", eFalse, eType = Some(V(11)))
        check(e + " and false false", eFalse, eType = Some(V(11)))
        check(e + " or true true", eTrue, eType = Some(V(15)))
        check(e + " or true false", eTrue, eType = Some(V(15)))
        check(e + " or false true", eTrue, eType = Some(V(15)))
        check(e + " or false false", eFalse, eType = Some(V(15)))
    }
}
