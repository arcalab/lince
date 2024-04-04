package hprog.frontend.solver

import hprog.ast.SymbolicExpr.SyExprAll
import hprog.ast._
import hprog.frontend.Eval
import Syntax._
import Runge_kutta._
import hprog.frontend.CommonTypes.{Point, Solution, SySolution, Valuation}
//import scala.collection.mutable.Map


//ESTÁ A DAR MAL, TESTAR COM PRINTS E VERIFICAR O MÉTODO SE ESTÁ BEM IMPLMENTADO
object Runge_kutta {


type DValuation = Map[String,Double]
//Arguments:
   //input -> Map[String,Double] = Initial values of the variables of diff.eqs
   //eqs -> List[DiffEq] = diff.eqs
   //time -> time to perform the Diff.eqs
def runge_kutta_func(input:DValuation , eqs:List[DiffEq],time:Double): scala.collection.mutable.Map[String,Double]  = {
   val initial_values = scala.collection.mutable.Map.empty[String, Double]
   initial_values ++= input //Map with initial values
   //println("Input:",initial_values)
   var h:Double=0.001 //step size
   var numSteps:Int=(time/h).toInt //number of steps until the 'time' 
   var iteration_values:scala.collection.mutable.Map[String,Double]=initial_values.clone() //Map to perform the formulation of runge-kutta 
   //var store_old_values:scala.collection.mutable.Map[String,List[Double]] = initial_values.map{case (key,value) => key -> List(value)} // Map that store de old values of the variables of the diff.eqs
   //Map to perform the k1,k2,k3 and k4 of the formule of runge-kutta
   var list_k1:scala.collection.mutable.Map[String,Double] = initial_values.clone().map{case (key,value) => key -> 0}
   var list_k2:scala.collection.mutable.Map[String,Double] = initial_values.clone().map{case (key,value) => key -> 0}
   var list_k3:scala.collection.mutable.Map[String,Double] = initial_values.clone().map{case (key,value) => key -> 0}
   var list_k4:scala.collection.mutable.Map[String,Double] = initial_values.clone().map{case (key,value) => key -> 0}
   //println("k1:",list_k1)
   //println("k2:",list_k2)
   //println("k3:",list_k3)
   //println("k4:",list_k4)

   for (i <- 0 until numSteps){
    
    // Determination of k1 for all differential equations
    //println("i:",i)


    for ((key, value) <- iteration_values) {
    //println("key:",key)
    //println("value:",value)
    //println("it_(key):",iteration_values(key))
    //println("initial_values(key):",initial_values(key))
    iteration_values(key) = initial_values(key)
    }
    //println("iteration_1:",iteration_values)
    for (deq <- eqs){
    	list_k1(deq.v.v)=h*(Eval.applyAux(iteration_values,deq.e))
    }
    //println("k1:",list_k1)
    //println("\n\n\n")








   // Determination of k2 for all differential equations
   for ((key, value) <- iteration_values) {
    /*
    println("key:",key)
    println("value:",value)
    println("it_(key):",iteration_values(key))
    println("initial_values(key):",initial_values(key))
    println("k1(key)/2:",list_k1(key)/2)
    */
    iteration_values(key) = initial_values(key)+list_k1(key)/2
    }
    //println("iteration_2:",iteration_values)
    for (deq <- eqs){
    	list_k2(deq.v.v)=h*(Eval.applyAux(iteration_values,deq.e))
    }
    //println("k2:",list_k2)
    //println("\n\n\n")







   //println("ALERTA iteration_values:",iteration_values)
   //println("ALERTA initial_values:",initial_values)

    // Determination  k3 for all differential equations
    for ((key, value) <- iteration_values) {
    /*
    println("key:",key)
    println("value:",value)
    println("it_(key):",iteration_values(key))
    println("initial_values(key):",initial_values(key))
    println("k2(key)/2:",list_k2(key)/2)
    */
    iteration_values(key) = initial_values(key)+list_k2(key)/2
    }
    //println("iteration_3:",iteration_values)
    for (deq <- eqs){
    	list_k3(deq.v.v)=h*(Eval.applyAux(iteration_values,deq.e))
    }
    //println("k3:",list_k3)
    //println("\n\n\n")









    // Determination of k4 for all differential equations
    for ((key, value) <- iteration_values) {
    /*
    println("key:",key)
    println("value:",value)
    println("it_(key):",iteration_values(key))
    println("initial_values(key):",initial_values(key))
    println("k3(key):",list_k3(key))
    */
    iteration_values(key) = initial_values(key)+list_k3(key)
    }
    //println("iteration_4:",iteration_values)
    for (deq <- eqs){
    	list_k4(deq.v.v)=h*(Eval.applyAux(iteration_values,deq.e))
    }
    //println("k4:",list_k4)
    //println("\n\n\n")

    
    //Update initial_values
    for ((key, value) <- initial_values) {
    initial_values(key) = value + (list_k1(key) + 2*list_k2(key) + 2*list_k3(key) + list_k4(key))/6
    }
    //println("initial_values:",initial_values)
    //Update store_old_values
    //for ((key, value) <- store_old_values) {
    //store_old_values(key) = value ++ List(initial_values(key))
    //}
   }

   //return store_old_values
   return initial_values

   }

}