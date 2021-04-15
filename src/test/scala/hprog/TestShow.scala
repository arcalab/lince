package hprog

import org.scalatest.flatspec.AnyFlatSpec
import DSL._
import hprog.ast._
import hprog.backend.Show

class TestShow extends AnyFlatSpec {

  def testPrint(c:Syntax, res:String): Unit = {
//    println(show(c))
    s"Program ${Show(c)}" should s"be printed as expected" in
      assertResult(res)(Show(c))
  }

  testPrint(ex1, "x:=2 for 0")
  testPrint(ex2, "y'=3 for 34")
  testPrint(ex3, "x'=2, y'=3 forever")
  //testPrint(ex4, "x' = 2\ny' = 3")
  testPrint(ex5, "y:=0, x:=0, x'=2, y'=3 for 34\nx'=2 until_None x>2")
  testPrint(ex6, "x:=0, x'=1 until_None (x>3*x) & (x<5)")
  testPrint(ex7, "p'=v, v'=g until_None (p<=0) & (v<=0)\nv:=-5/10*v for 0")
  testPrint(ex8, "x'=1 until_None ((x>3*x) & (x<5)) | (y>=3)")
  testPrint(ex9, "x'=1 until_None (x>3*x) & ((x<5) | (y>=3))")

}