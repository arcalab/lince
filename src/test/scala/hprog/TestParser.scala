package hprog

import org.scalatest.flatspec.AnyFlatSpec
import hprog.DSL._
import hprog.ast._
import hprog.backend.Show
import hprog.lang.Parser
import hprog.common.ParserException


/**
  * Created by jose on 02/08/2018.
  */
class TestParser extends AnyFlatSpec {

  testOk("x:=2",ex1)
  testOk("y'=3 for 34",ex2)
  testOk("x'=2, y'=3",ex3)
  //testOk("x'=2 ; y'=3",ex4)
  testOk("y:=0; x:=0; x'=2, y ' =3 for 34 ; x' = 2 until x > 2",ex5)
  //testOk("x=2, y=3 & 34 ; x=2 & x > 2",ex5)
  testOk("x:=0; x'=1 until x > (3 * x) /\\ x < 5",ex6)
  testKO("p' = v; v' = g until_0.01 p <= 0 /\\ v <= 0;\nv := -0.5 * v")


  private def testOk(in:String,res:Syntax): Unit =
    s"""Parsing "$in"""" should s"""produce the program "${Show(res)}"""" in {
      Parser.parse(in) match {
        case Parser.Success(result, _) =>
          assertResult(res)(result)
        //          assert(s"Wrong parsed value. Got\n  $result\nexpected\n  $res",result,res)
        case err: Parser.NoSuccess =>
          fail("Parse error: " + err)
      }
  }

  private def testKO(in:String): Unit =
    s"""Parsing "$in"""" should s"""NOT parse"""" in {
      try {
        Parser.parse(in) match {
          case Parser.Success(result, _) =>
            fail(s"succeed to parse: ${Show(result)}")
          //          assert(s"Wrong parsed value. Got\n  $result\nexpected\n  $res",result,res)
          case _: Parser.NoSuccess =>
            //fail("Parse error: " + err)
        }
      } catch {
        case _:ParserException =>
      }
    }
}

