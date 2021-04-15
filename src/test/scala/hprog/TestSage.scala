package hprog

import hprog.DSL._
import hprog.backend.Show
import hprog.frontend.solver.LiveSageSolver
import hprog.lang.SageParser
import org.scalatest.flatspec.AnyFlatSpec


/**
  * Created by jose on 02/08/2018.
  */
class TestSage extends AnyFlatSpec {

  val solver = new LiveSageSolver("/Applications/SageMath-9.0.app/Contents/Resources/sage")

  // testOk("x:=2")
  // testOk("v:=0; v'=1 for 2; v'=3 for 2")
  // testOk("p:=0; v:=2; while true do { if v<=10 then p'=v,v'=5  for 1  else p'=v,v'=-2 for 1}")
  // testOk("y:=0; x:=0; x'=2, y ' =3 for 34 ; x' = 2 until_0.01 x > 2")

  // testCond("25 > -49/10*2","True")

  testExpr("-49/5*2 + 34","72/5")

  private def testExpr(in:String,res:String): Unit =
    s"""Using Sage to simplify "$in"""" should s"""produce the value "${Show(res)}"""" in {
      
      SageParser.parseExpr(in) match {
        case SageParser.Success(result, _) =>
          val solved = solver.solveSymb(result)
          assertResult(res)(solved.toString())
        //          assert(s"Wrong parsed value. Got\n  $result\nexpected\n  $res",result,res)
        case err: SageParser.NoSuccess =>
          fail("Parse error: " + err)
      }
  }

}

