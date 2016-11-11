package org.miniML.parser

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.miniML.parser.ForParser
import org.miniML.parser.Block
//import org.bbwp.parser.ForLoop

@RunWith(classOf[JUnitRunner])
class ForParserSuite extends FunSuite {

  test("simple let test") {
    val fp = new ForParser()
    val r = fp.parse(fp.loop,"for i in 0 to 10 {}")
    val e = ForLoop("i",0,10,Block(List()))
    assert(r.get==e, s"LetParser")
  }
}