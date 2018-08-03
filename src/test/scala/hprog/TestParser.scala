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

  testOk("x:=2",ex1)
  testOk("y=3 & 34",ex2)
  testOk("x=2, y=3",ex3)
  testOk("x=2 ; y=3",ex4)
  testOk("x:=0; x=2, y=3 & 34 ; x=2 & x > 2",ex5)
//  testOk("x=2, y=3 & 34 ; x=2 & x > 2",ex5)
  testOk("x=1 & x > (3 * x) /\\ x < 5",ex6)


  private def testOk(in:String,res:Prog) =
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

