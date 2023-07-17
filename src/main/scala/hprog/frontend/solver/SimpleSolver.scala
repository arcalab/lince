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
  override def evalFun(eqs: List[DiffEq]): Solution = {
    val vars = Solver.getVars(eqs).filter(_.startsWith("_")) // NEW
    //println("vars_SimpleSolver:",vars)
    vars.map(v=> v -> ( (t:Double) => (init:Point ) =>
      callTaylorSolver(init,eqs)(t)(v)
      )).toMap
  }

  override def solveSymb(eqs:List[DiffEq]): SySolution = Map()
  override def solveSymb(expr: SyExprAll): SyExprAll = expr
  override def solveSymb(cond: Cond, v:Valuation): Boolean = Eval(Eval(v),cond)




  private def callTaylorSolver(input:DValuation , eqs:List[DiffEq]): Double => DValuation  = {
    //println("input:",input)
    //println("eqs:",eqs)

    val (vars,mtx): (List[String],List[List[Double]]) = Solver.getMatrix(eqs)
    //println("(vars,mtx):",(vars,mtx))

    val sol1: (List[Double],Double) => List[Double] = Solver.solveTaylorManual(mtx)
    //println("sol1:",sol1)

    def sol(t:Double): DValuation  = {
      // "input" should have all variables but no "" - this should be assigned to 0
      def getDummy(v:String): Double = (vars.indexOf(v),vars.indexOf("_"+v)) match {
        case (_,-1) => 0.0
        case (_,_)  =>
          1
      }
      val dummies = vars.map(v => ("_"+v) -> getDummy(v))
      //println("dummies:",dummies)

      val input2  = vars.map(input ++ dummies)
      //println("input2:",input2)

      val list = sol1(input2, t)
      //println("list:",list)

      (vars zip list).toMap -- vars.map("_"+_)
    }
    sol
  }

}
