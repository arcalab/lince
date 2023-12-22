package hprog.frontend.solver

import hprog.ast.SymbolicExpr.SyExprAll
import hprog.ast._
import hprog.frontend.Eval
import Syntax._
import hprog.frontend.CommonTypes.{Point, Solution, SySolution, Valuation}


/*
class SimpleSolver extends Solver {

  type DValuation = Map[String,Double]
  override def evalFun(eqs: List[DiffEq]): Solution = {
    val vars = Solver.getVars(eqs).filter(_.startsWith("_")) // NEW
    println("vars_SimpleSolver:",vars)
    val res= vars.map(v=> v -> ( (t:Double) => (init:Point ) =>
      rungeKutta4(init,eqs)(t)(v)
      )).toMap

    //print("outputttttttttttttttttttttttttttttttttttttttttttttttttttEvalFun:",res)
    return res
  }

  private def rungeKutta4(input:DValuation , eqs:List[DiffEq]): Double => DValuation  = {
    //input->initial conditions for each variable
    //eqs -> differential equations


  }

}

*/






// Numerical solver, using a naive solution for differencital equations
// based on Taylor series


class SimpleSolver(windows_size:Double) extends Solver {

  type DValuation = Map[String,Double]
  override def evalFun(eqs: List[DiffEq]): Solution = {
    val vars = Solver.getVars(eqs).filter(_.startsWith("_")) // NEW
    val res= vars.map(v=> v -> ( (t:Double) => (init:Point ) =>
      runge_kutta_func(init,eqs,t,v)
      )).toMap
    return res
  }

  override def solveSymb(eqs:List[DiffEq]): SySolution = Map()
  override def solveSymb(expr: SyExprAll): SyExprAll = expr
  override def solveSymb(cond: Cond, v:Valuation): Boolean = Eval(Eval(v),cond)

def runge_kutta_func(input:DValuation , eqs:List[DiffEq],time:Double, key_V:String): Double   = {
       
 val init = scala.collection.mutable.Map.empty[String, Double]
 init ++= input //Map with initial values
 var N:Int = 75 // number of steps until the 'time' 
 var h:Double=time/N //step size 
 var acum:scala.collection.mutable.Map[String,Double]=init.clone() //Map to perform the formulation of runge-kutta 
 //Map to perform the k1,k2,k3 and k4 of the formule of runge-kutta
 var k1:scala.collection.mutable.Map[String,Double] = init.clone().map{case (key,value) => key -> 0}
 var k2:scala.collection.mutable.Map[String,Double] = init.clone().map{case (key,value) => key -> 0}
 var k3:scala.collection.mutable.Map[String,Double] = init.clone().map{case (key,value) => key -> 0}
 var k4:scala.collection.mutable.Map[String,Double] = init.clone().map{case (key,value) => key -> 0}
 
    
 for (i <- 0 until N){
  
  // Determination of k1 for all differential equations
  for ((key, value) <- acum) {
  
  acum(key) = init(key)
  }
  for (deq <- eqs){
    k1(deq.v.v)=h*(Eval.applyAux(acum,deq.e))
  }







 // Determination of k2 for all differential equations
 for ((key, value) <- acum) {
  acum(key) = init(key)+k1(key)/2
  }
  for (deq <- eqs){
    k2(deq.v.v)=h*(Eval.applyAux(acum,deq.e))
  }
 



  // Determination  k3 for all differential equations
  for ((key, value) <- acum) {
  acum(key) = init(key)+k2(key)/2
  }
  for (deq <- eqs){
    k3(deq.v.v)=h*(Eval.applyAux(acum,deq.e))
  }









  // Determination of k4 for all differential equations
  for ((key, value) <- acum) {
  acum(key) = init(key)+k3(key)
  }
  for (deq <- eqs){
    k4(deq.v.v)=h*(Eval.applyAux(acum,deq.e))
  }
 

  
  //Update init
  for ((key, value) <- init) {
  init(key) = value + (k1(key) + 2*k2(key) + 2*k3(key) + k4(key))/6
  }
 }
 
 
 //val (key,value)=init.head
 //return store_old_values
 //return sol
 return init(key_V)

   }


/*
  private def callTaylorSolver(input:DValuation , eqs:List[DiffEq]): Double => DValuation  = {
    println("inputtttttttttt:",input) //Map[String,Double]
    println("eqsssssssssss:",eqs)
    

    val (vars,mtx): (List[String],List[List[Double]]) = Solver.getMatrix(eqs)
    println("(vars,mtx):",(vars,mtx))

    val sol1: (List[Double],Double) => List[Double] = Solver.solveTaylorManual(mtx)
    println("sol1:",sol1)

    def sol(t:Double): DValuation  = {
      // "input" should have all variables but no "" - this should be assigned to 0
      def getDummy(v:String): Double = (vars.indexOf(v),vars.indexOf("_"+v)) match {
        case (_,-1) => 0.0
        case (_,_)  =>
          1
      }
      val dummies = vars.map(v => ("_"+v) -> getDummy(v))
      println("dummies:",dummies)

      val input2  = vars.map(input ++ dummies)
      println("input2:",input2)

      val list = sol1(input2, t)
      println("list:",list)

      (vars zip list).toMap -- vars.map("_"+_)
    }
    sol
  }
  */

}
