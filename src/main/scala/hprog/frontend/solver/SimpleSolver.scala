package hprog.frontend.solver

import hprog.ast.SymbolicExpr.SyExprAll
import hprog.ast._
import hprog.frontend.Eval
import Syntax._
import hprog.frontend.CommonTypes.{Point, Solution, SySolution, Valuation}

// Numerical solver, using a naive solution for differencital equations
// based on Taylor series

class SimpleSolver extends Solver {

  type DValuation = Map[String,Double]
//  override def ++=(systems: List[List[DiffEq]]): Unit = {}
//  override def ++=(syntax: Syntax): Unit = {}
//  override def +=(eqs: List[DiffEq]): Unit = {}
  override def evalFun(eqs: List[DiffEq]): Solution = {
    val vars = Solver.getVars(eqs).filterNot(_.startsWith("_"))
    vars.map(v=> v -> ( (t:Double) => (init:Point ) =>
      callTaylorSolver(init,eqs)(t)(v)
      )).toMap
  }

  override def solveSymb(eqs:List[DiffEq]): SySolution = Map()
  override def solveSymb(expr: SyExprAll): SyExprAll = expr
  override def solveSymb(cond: Cond, v:Valuation): Boolean = Eval(Eval(v),cond)

//  override def solveSymb(eqs: List[DiffEq]): SageSolution = Map()


  private def callTaylorSolver(input:DValuation , eqs:List[DiffEq]): Double => DValuation  = {
    val (vars,mtx): (List[String],List[List[Double]]) = Solver.getMatrix(eqs)
    val sol1: (List[Double],Double) => List[Double] = Solver.solveTaylorManual(mtx)

    //    println(s"## calling solver" +
    //      s"\neqs:\n  ${eqs.mkString("\n  ")}" +
    //      s"\ninput: ${input.map(p=>s"${p._1}->${p._2}").mkString(", ")}" +
    //      s"\nvars: ${vars.mkString(",")}" +
    //      s"\nmtx:\n  ${mtx.map(_.mkString("\t")).mkString("\n  ")}")
    //println("## Sage\n"+genSage(eqs))
    //println(s"calling solver for ${input} and ${eqs.map(Show(_)).mkString(",")}")
    def sol(t:Double): DValuation  = {
      // "input" should have all variables but no "" - this should be assigned to 0
      def getDummy(v:String): Double = (vars.indexOf(v),vars.indexOf("_"+v)) match {
        case (_,-1) => 0.0
        case (_,_)  =>
          //println(s"dummy($v) = ${mtx(i)(j)}")
          //mtx(i)(j)
          1
      }
      val dummies = vars.map(v => ("_"+v) -> getDummy(v))
      val input2  = vars.map(input ++ dummies)
      //      println("input with dummies: "+input2.mkString(","))
      val list = sol1(input2, t)
      //      println(s"solving: got list ${list.size}")
//      println(s"solving: returning ${(vars zip list).toMap -- vars.map("_"+_)}")
      (vars zip list).toMap -- vars.map("_"+_)
    }
    sol
  }

}
