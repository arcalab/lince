package hprog

import org.scalatest.FlatSpec
import DSL._
import hprog.ast._
import hprog.frontend.Show

class TestShow extends FlatSpec {

  def testPrint(c:Progr,res:String) {
//    println(show(c))
    s"Program ${Show(c)}" should s"be printed as expected"
    assertResult(res)(Show(c))
  }

  testPrint(ex1,
    "x:=2.0")
  testPrint(ex2,
    "y:=3.0 & 34.0")
  testPrint(ex3,
    "x:=2.0, y:=3.0")
  testPrint(ex4,
    "x:=2.0 ; y:=3.0")
  testPrint(ex5,
    "(x:=2.0, y:=3.0) & 34.0 ; x':=2.0 & x > 2.0")
  testPrint(ex6,
    "x':=x > (y * 3.0) & x < 5.0")

}