package hprog.frontend

import breeze.linalg._
import breeze.numerics._
import hprog.DSL
import hprog.ast._

object Solver {
  /**
    * collect list of unique variables used by a system of equations
    * @param eqs
    * @return unique variables
    */
  def getVars(eqs:DiffEqs): List[String] =
    eqs.eqs.flatMap(getVars).distinct

  private def getVars(eq:DiffEq): List[String] =
    eq.v.v :: getVars(eq.e)

  private def getVars(e:Lin): List[String] = e match {
    case Var(v) => List(v)
    case Value(v) => List("")
    case Add(l1, l2) => getVars(l1) ::: getVars(l2)
    case Mult(v, l) => getVars(l)
  }

  private def getMatrix(eqs:DiffEqs): (List[String],List[List[Double]]) = {
    val vars = getVars(eqs)
    println(s"## collected vars: ${vars.map("'"+_+"'").mkString(", ")}")
    val rows = eqs.eqs.map((x:DiffEq) => x.v.v -> getRow(vars,x.e)).toMap
    (  vars
      ,for (v<-vars) yield rows.getOrElse(v,vars.map(_ => 0.0))) // Note: set to 0 when unknown variable
  }

  private def getRow(vars:List[String],e:Lin): List[Double] = {
    val m = getRowValues(e)
    vars.map(x => m.getOrElse[Double](x,0))
  }

  private def getRowValues(e:Lin): Map[String,Double] = e match {
    case Var(v) => Map(v->1)
    case Value(v) => Map(""->v)
    case Add(l1, l2) => join(getRowValues(l1),getRowValues(l2))
    case Mult(v, l) => getRowValues(l).mapValues(_*v.v)
  }
  //  private def multt(d:Double,d2:Double): Double = d*d2
  private def join(m1:Map[String,Double],m2:Map[String,Double]): Map[String,Double] = {
    var res = m2
    for ((k, v1) <- m1) m2.get(k) match {
      case Some(v2) => val v = v1+v2; res += k -> v
      case None => res += k->v1
    }
    res
  }

  private def list2Matrix(m:List[List[Double]]): DenseMatrix[Double] = {
    val rows = m.size
    val cols = m.headOption.getOrElse(Nil).size
//    println("before toMtr: "+m)
    DenseMatrix.create(cols,rows,m.flatten.toArray).t
  }

  /**
    * Core function that solves an equation using breeze libraries
    * @param eqs differential equations to be solved (ignoring conditions for now)
    * @return trajectory (ignoring conditions)
    */
  def solve(state:Map[String,Double],eqs:DiffEqs): Trajectory = { //scala.Seq[Double] => Double => DenseMatrix[Double] = {
    val (vars, mtx) = getMatrix(eqs)
    val res = solve(vars,mtx)
    //    if ((vars contains "") && mtx.forall(_ == 0.0))
    //

//    def buildGuard(trajectory: Trajectory, init: List[Double], cond: Cond): Double => Boolean = {
//      val c: Double => Boolean = (t:Double) =>  {
//        val res = Eval(trajectory, init, cond)(t)
//        println(s"--- guard[$cond]($t) = $res")
//        res
//      }
//      c
//    }

    eqs.dur match {
      case For(t) => res until Some(t.v)
      case Until(c) =>
        val init = res.vars.map(state.getOrElse(_,0.0))
        val guard: Double => Boolean = Eval(res,init,c) //buildGuard(res,init,c)
        println(s"--- guard[$cond](${init.mkString(",")}) = $guard")
        // TODO: experiment with different parameters
        val dur = if (guard(0)) Some(0.0)
                  else logSearch(1,0,true,guard)
        res until dur
      case Forever => res until None
    }
  }

  /**
    * Solve a linear/coupled system of diferential equations
    * @param vars names of variables
    * @param mtx matrix
    * @return
    */
  def solve(vars:List[String],mtx:List[List[Double]]): Trajectory = {
    val a = list2Matrix(mtx)
    val p = eig(a).eigenvectors
    val pi = inv(p)
    val ev = eig(a).eigenvalues

//    println("## A:\n"+a)
//    println("## p:\n"+p)
//    println("## p^-1:\n"+pi)
//    println("## eigV: "+ev.toArray.mkString(", "))

    def sol(x0:List[Double],t:Double): DenseMatrix[Double] = {
//      println("#### E:\n"+diag(exp(ev *:* t)))
//      println("#### pEp-1:\n"+p * diag(exp(ev *:* t)) * pi)
//      println(s"#### x0=${x0.mkString(",")} ")
      val res = p * diag(exp(ev *:* t)) * pi * list2Matrix(x0.map(List(_)))
//      println(s"--- sol($t) = ${res.t}")
      res
    }

    val traj = buildTrajectory(vars,sol)
    traj
  }

  /**
    * Converts a function over matrices into a trajectory (over assignments)
    * @param vars List of variables in the order used by the matrices
    * @param matrixFun function over matrices
    * @return Trajectory for that function
    */
  private def buildTrajectory(vars: List[String],
                              matrixFun: (List[Double],Double) => DenseMatrix[Double]) : Trajectory = {
//    def list2map[A](l:Iterable[A]): Map[String,A] =
//      (for ((v,i) <- vars.zipWithIndex) yield (v,l.toList(i))).toMap
//    def map2list(m:Map[String,Double]): List[Double] =
//      for ( v  <-  vars) yield m.getOrElse(v,0.0)

    val res: List[Double] => Double => List[Double] =
      mapVars => t =>  matrixFun(mapVars,t).data.toList

    Trajectory.buildFromList(None,vars,res)
  }


  ////////////////////////
  /// TESTING FUNCTIONS //
  ////////////////////////


//  def solveEqs(eqs: List[DiffEq]): Trajectory = eqs match {
//    case Nil => Trajectory.empty
//    case t1::Nil => solve(t1)
//    case t1::tl =>
//  }

  def getFstDiffEq(prog:Prog): DiffEqs  = prog match {
//    case Assign(v, e) => Nil
    case d@DiffEqs(eqs, dur) => d
//    case Seq(Nil) => Nil
    case Seq(p::ps) =>
      try getFstDiffEq(p)
      catch {
        case _: Throwable => getFstDiffEq(Seq(ps))
      }
//    case Skip => Nil
    case ITE(ifP, thenP, elseP) =>
      try getFstDiffEq(thenP)
      catch {
        case _: Throwable => getFstDiffEq(elseP)
      }
    case While(c, doP) => getFstDiffEq(doP)
    case _ => throw new RuntimeException("no diff. eq. found")
  }

  def getFstMatrix(prog:Prog): List[List[Double]] =
    getMatrix(getFstDiffEq(prog))._2

  def tryM(prog:String): List[List[Double]] = getFstMatrix(DSL.parse(prog))

  //  def tryS(prog:String): scala.Seq[Double] => Double => DenseMatrix[Double] = solve(getFstDiffEq(DSL.parse(prog)))
//    def tryS(prog:String): Trajectory = {
//      val ed = getFstDiffEq(DSL.parse(prog))
//      val res = solve(ed)
//
//      val until: Option[Double] = ed.dur match {
//        case For(t) => Some(t.v)
//        case Until(c) =>
//          val init = res.vars.map(_=>1.0) // TODO: now testing with hard coded value (1,1,1,...)
//          val guard =  buildGuard(res,init,c)
//          // TODO: experiment with different parameters
//          logSearch(1,0,true,guard)
//        case Forever => None
//      }
//      println(s"until: $until")
//      res
//    }

  // import hprog.frontend.Solver._
  // ex1:
  // tryS("x1=-(x1+3*x2)-x3, x2=2*x2 & 2")
  // ex2 (from the bool):
  // tryS("x1=-x1, x2=2*x2 & 2")
  // ex3 (from th book):
  // tryS("x1=-x1-3*x2, x2=2*x2 & 2")

  // usage:
  // val sol = tryS("x1=-x1-3*x2, x2=2*x2 & 2")
  // sol(List(1,1))(4) // x(0)=[1 1] , t=4



//  val curX = 6
//  val gamma = 0.01
//  val precision = 0.00001
//  val previousStepSize = 1 / precision
//
//  def df(x: Double): Double = 4 * pow(x, 3) - 9 * pow(x, 2)

  /**
    * Performs a gradient descent, i.e., searches for an argument "t" of "function" s.t. function(t) = 0.
    * From wikipedia - not in use.
    * @param previousStepSize how closer it got since the last try
    * @param curX initial value
    * @param function to find a 0
    * @param precision goal (the new step being smaller than the precision)
    * @param gamma difference used to calculate derivative
    * @return the argument of the function that yields 0
    */
  def gradientDescent(previousStepSize: Double= 1/0.000001, curX: Double = 0,
                      function: Double => Double,
                      precision: Double = 0.000001, gamma: Double = 0.01): Double = {
    if (previousStepSize > precision) {
      val newX = curX + -gamma * function(curX)
      println(curX)
      // update previousStepSize and curX
      gradientDescent(abs(newX - curX), newX, function, precision, gamma)
    } else curX
  }
//  val ans = gradientDescent(previousStepSize, curX, precision, gamma)
//  println(s"The local minimum occurs at $ans")

  /**
    * Searches for the earliest value at which a given guard becomes true.
    * Initially it grows exponentially until it finds a "true",
    * once it does, searches for the initial value up to some precision.
    * Assumes that, once the guard it true, it will always be true.
    * @param prevStepSize
    * @param curX
    * @param dir
    * @param expanding
    * @param guard
    * @param precision
    * @return
    */
  def logSearch(prevStepSize:Double, curX: Double, expanding:Boolean,
                guard: Double => Boolean,
                precision: Double = 0.0000001, max: Int = 1000000) : Option[Double] = {
//    println(s"${if (expanding) "EXPANDING" else "DIVING "} at $curX with step $prevStepSize")
    if (max<curX)  return None

    if (expanding) { // growing steps until guard succeeds
      if (!guard(curX))
           logSearch(prevStepSize*2,curX + prevStepSize  ,expanding,guard,precision,max)
      else logSearch(prevStepSize/8,curX - prevStepSize/4,false,guard,precision,max)
    }
    else if (prevStepSize > precision) { // shrinking steps until having the right precision
      if (!guard(curX))
           logSearch(prevStepSize/2,curX + prevStepSize,expanding,guard,precision,max)
      else logSearch(prevStepSize/2,curX - prevStepSize,expanding,guard,precision,max)
    }
    else Some(curX)
  }
}
