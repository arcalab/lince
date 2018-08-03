package hprog

import org.scalatest.FlatSpec
import DSL._
import hprog.ast._
import hprog.frontend.Show

class TestShow extends FlatSpec {

  def testPrint(c:Prog,res:String) {
//    println(show(c))
    s"Program ${Show(c)}" should s"be printed as expected" in
      assertResult(res)(Show(c))
  }

  testPrint(ex1, "x := 2")
  testPrint(ex2, "y' = 3 & 34")
  testPrint(ex3, "x' = 2, y' = 3")
  testPrint(ex4, "x' = 2\ny' = 3")
  testPrint(ex5, "x := 0\nx' = 2, y' = 3 & 34\nx' = 2 & x > 2")
  testPrint(ex6, "x' = 1 & x > 3 x /\\ x < 5")
  testPrint(ex7, "p' = v, v' = g & p <= 0 /\\ v <= 0\nv := -0.5 v")
  testPrint(ex8, "x' = 1 & (x > 3 x /\\ x < 5) \\/ y >= 3")
  testPrint(ex9, "x' = 1 & x > 3 x /\\ (x < 5 \\/ y >= 3)")

}