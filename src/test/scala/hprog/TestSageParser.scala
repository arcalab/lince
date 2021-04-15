package hprog

import hprog.ast._
import hprog.frontend.Eval
import hprog.lang.SageParser
import org.scalatest.flatspec.AnyFlatSpec


/**
  * Created by jose on 05/04/2019.
  */
class TestSageParser extends AnyFlatSpec {

  testOk("[v(_t_) == -49/5*_t_ + v(0), p(_t_) == -49/10*_t_^2 + _t_*v(0) + p(0)]",
    Map("v"->100, "p"->500),
    List(0.0->Map("v"->100.0,"p"->500.0), 3.0 -> Map("v"-> (-49.0*3.0/5.0+100.0),"p"-> (-49.0*9.0/10.0+800.0))))

  testOk("[f1(_t_) == _t_ + f1(0), f2(_t_) == _t_ + f2(0)]",
    Map("f1"->100, "f2"->500),
    List(0.0->Map("f1"->100.0,"f2"->500.0), 3.0 -> Map("f1"-> (3.0+100.0),"f2"-> (3.0+500.0))))

  testOk("[p(_t_) == cos(_t_)*p(0) + sin(_t_)*v(0), v(_t_) == -p(0)*sin(_t_) + cos(_t_)*v(0)]",
    Map("v"->100, "p"->500),
    List(3.0 -> Map("p"-> (Math.cos(3.0)*500.0+Math.sin(3.0)*100.0),"v"-> (Math.cos(3.0)*100.0-Math.sin(3.0)*500.0))))

  testOk("_t_ + x(0)",
    Map("x"->0),
    List(0.0->Map(""->0.0), 3.0 -> Map(""-> 3.0)))


  //  testOk("[f1(_t_) == _t_ + f1(0), f2(_t_) == _t_ + f2(0)]",
//    Map("f1"->100, "f2"->500),List(0.0->0.0,1.0->0.0,2.0->0.0))
//  testOk("[p(_t_) == 5/2*_t_^2 + _t_*v(0) + p(0), v(_t_) == 5*_t_ + v(0)]",
//    Map("p"->100, "v"->500),List(0.0->0.0,1.0->0.0,2.0->0.0))
//  testOk("[p(_t_) == -_t_^2 + _t_*v(0) + p(0), v(_t_) == -2*_t_ + v(0)]",
//    Map("p"->100, "v"->500),List(0.0->0.0,1.0->0.0,2.0->0.0))


  private def testOk(in:String, ctx:frontend.CommonTypes.Point, res:List[(Double,Map[String,Double])]): Unit =
    for ((t,mp) <- res; (vv,va) <- mp) {
      s"Parsing '$in'\n for $vv, t=$t with ${ctx.mkString(";")}" should s"""be $va"""" in {
//      s"Parsing something" should s"""be $va"""" in {
        SageParser.parse(in) match {
          case SageParser.Success(result, _) =>
            assertResult(va)(Eval.apply(result(vv),t,ctx.view.mapValues(SVal).toMap))
          //          assert(s"Wrong parsed value. Got\n  $result\nexpected\n  $res",result,res)
          case err: SageParser.NoSuccess =>
            fail("Parse error: " + err)
        }
      }
    }
}

