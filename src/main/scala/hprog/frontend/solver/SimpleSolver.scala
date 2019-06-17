package hprog.frontend.solver

import hprog.ast.{DiffEq, Syntax}
import hprog.frontend.Semantics.{SageSolution, Solution, Valuation}

// Numerical solver, using a naive solution for differencital equations
// based on Taylor series

class SimpleSolver extends Solver {

  override def ++=(systems: List[List[DiffEq]]): Unit = {}
  override def ++=(syntax: Syntax): Unit = {}
  override def +=(eqs: List[DiffEq]): Unit = {}
  override def evalFun(eqs: List[DiffEq]): Solution = {
    val vars = Solver.getVars(eqs).filterNot(_.startsWith("_"))
    vars.map(v=> v -> ( (t:Double) => (init:Valuation) =>
      callTaylorSolver(init,eqs)(t)(v)
      )).toMap
  }
  override def solveSymb(eqs: List[DiffEq]): SageSolution = Map()


  private def callTaylorSolver(input:Valuation, eqs:List[DiffEq]): Double => Valuation = {
    val (vars,mtx): (List[String],List[List[Double]]) = Solver.getMatrix(eqs)
    val sol1: (List[Double],Double) => List[Double] = Solver.solveTaylorManual(mtx)

    //    println(s"## calling solver" +
    //      s"\neqs:\n  ${eqs.mkString("\n  ")}" +
    //      s"\ninput: ${input.map(p=>s"${p._1}->${p._2}").mkString(", ")}" +
    //      s"\nvars: ${vars.mkString(",")}" +
    //      s"\nmtx:\n  ${mtx.map(_.mkString("\t")).mkString("\n  ")}")
    //println("## Sage\n"+genSage(eqs))
    //println(s"calling solver for ${input} and ${eqs.map(Show(_)).mkString(",")}")
    def sol(t:Double): Valuation = {
      // "input" should have all variables but no "" - this should be assigned to 0
      def getDummy(v:String): Double = (vars.indexOf(v),vars.indexOf("_"+v)) match {
        case (_,-1) => 0.0
        case (i,j)  =>
          //          println(s"dummy($v) = ${mtx(i)(j)}")
          //mtx(i)(j)
          1
      }
      val dummies = vars.map(v => ("_"+v) -> getDummy(v))
      val input2  = vars.map(input ++ dummies)
      //      println("input with dummies: "+input2.mkString(","))
      val list = sol1(input2, t)
      (vars zip list).toMap -- vars.map("_"+_)
    }
    sol
  }

}
