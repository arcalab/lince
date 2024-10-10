package hprog.frontend.solver

import hprog.ast.SymbolicExpr.SyExprAll
import hprog.ast._
import hprog.frontend.Eval
import Syntax._
import Runge_kutta._
import hprog.frontend.CommonTypes.{Point, Solution, SySolution, Valuation}
import scala.collection.mutable.{Map => MMap}

object Runge_kutta {

  type DValuation = Map[String,Double]

  /**
    *
    * @param input Map[String,Double] = Initial values of the variables of diff.eqs
    * @param eqs List[DiffEq] = diff.eqs
    * @param time time to perform the Diff.eqs
    * @return Map[String,Double] Valuation, assigning each variable to a value
    */
  def runge_kutta_func(input:DValuation , eqs:List[DiffEq],time:Double): MMap[String,Double]  = {
    val initial_values = MMap.empty[String, Double]
    initial_values ++= input //Map with initial values
    val h:Double=0.001 //step size
    val numSteps:Int=(time/h).toInt //number of steps until the 'time'
    val iteration_values:MMap[String,Double]=initial_values.clone() //Map to perform the formulation of runge-kutta
     //var store_old_values:MMap[String,List[Double]] = initial_values.map{case (key,value) => key -> List(value)} // Map that store de old values of the variables of the diff.eqs
     //Map to perform the k1,k2,k3 and k4 of the formule of runge-kutta
//     val list_k1:MMap[String,Double] = initial_values.clone().map{case (key,value) => key -> 0}
//     val list_k2:MMap[String,Double] = initial_values.clone().map{case (key,value) => key -> 0}
//     val list_k3:MMap[String,Double] = initial_values.clone().map{case (key,value) => key -> 0}
//     val list_k4:MMap[String,Double] = initial_values.clone().map{case (key,value) => key -> 0}
    val list_k1 = MMap.empty[String,Double].withDefaultValue(0)
    val list_k2 = MMap.empty[String,Double].withDefaultValue(0)
    val list_k3 = MMap.empty[String,Double].withDefaultValue(0)
    val list_k4 = MMap.empty[String,Double].withDefaultValue(0)

    for (i <- 0 until numSteps){

      // Determination of k1 for all differential equations
      //println("i:",i)

//      for ((key, value) <- iteration_values)
//        iteration_values(key) = initial_values(key)
      for (deq <- eqs)
        list_k1(deq.v.v)=h*(Eval.applyAux(iteration_values,deq.e))

      // Determination of k2 for all differential equations
      for ((key, value) <- iteration_values)
        iteration_values(key) = initial_values(key)+list_k1(key)/2

      for (deq <- eqs)
        list_k2(deq.v.v)=h*(Eval.applyAux(iteration_values,deq.e))

      // Determination  k3 for all differential equations
      for ((key, value) <- iteration_values)
        iteration_values(key) = initial_values(key)+list_k2(key)/2
      //println("iteration_3:",iteration_values)
      for (deq <- eqs)
        list_k3(deq.v.v)=h*(Eval.applyAux(iteration_values,deq.e))

      // Determination of k4 for all differential equations
      for ((key, value) <- iteration_values)
        iteration_values(key) = initial_values(key)+list_k3(key)
      //println("iteration_4:",iteration_values)
      for (deq <- eqs)
        list_k4(deq.v.v)=h*(Eval.applyAux(iteration_values,deq.e))

      //Update initial_values
      for ((key, value) <- initial_values)
        initial_values(key) = value + (list_k1(key) + 2*list_k2(key) + 2*list_k3(key) + list_k4(key))/6
     }

     //return store_old_values
     initial_values
   }

}