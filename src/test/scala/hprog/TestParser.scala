package hprog

import org.scalatest.FlatSpec
import hprog.DSL._
import hprog.ast._
import hprog.frontend.Show
import hprog.lang.Parser


/**
  * Created by jose on 02/08/2018.
  */
class TestParser extends FlatSpec {

  testOk("x:=2.0",ex1)
  testOk("y:=3.0 & 34.0",ex2)
  testOk("x:=2.0, y:=3.0",ex3)
  testOk("x:=2.0 ; y:=3.0",ex4)
  testOk("(x:=2.0, y:=3.0) & 34.0 ; x':=2.0 & x > 2.0",ex5)
  testOk("x:=2.0, y:=3.0 & 34.0 ; x':=2.0 & x > 2.0",ex5)
  testOk("x':=x > (y * 3.0) & x < 5.0",ex6)


  private def testOk(in:String,res:Progr) =
    s"""Parsing "$in"""" should s"""produce the program "${Show(res)}"""" in {
      Parser.parse(in) match {
        case Parser.Success(result, _) =>
          assertResult(res)(result)
        //          assert(s"Wrong parsed value. Got\n  $result\nexpected\n  $res",result,res)
        case err: Parser.NoSuccess =>
          fail("Parse error: " + err)
      }
  }
}

