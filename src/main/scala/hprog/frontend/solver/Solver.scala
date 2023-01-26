package hprog.frontend.solver

import breeze.linalg._
import breeze.numerics._
import hprog.ast.SymbolicExpr.{Pure, SyExpr, SyExprAll}
import hprog.ast._
import Syntax._
import hprog.backend.Show
import hprog.frontend.CommonTypes._
import hprog.frontend.Eval
import hprog.frontend.Utils.asSyExpr

import scala.collection.immutable

trait Solver {

  /** Gets a solution of a system of equations, using system calls to Sage,
    * checking first in its cache.
    * @param eqs System of equations to be retrived
    * @return Result from Sage as a function
    */
  def evalFun(eqs:List[DiffEq]): Solution =
    solveSymb(eqs).view.mapValues(evalFun).toMap
  def evalFun(expr: SyExprAll): SFunction =
    (t:Double) => (v:Point) => Eval(Eval.update(expr,SVal(t),v.view.mapValues(SVal).toMap))
  def evalVal(expr: SyExpr): Double =
    Eval(asSyExpr(solveSymb(expr)))

  /** Gets a symbolic solution of a system of equations, using system calls to Sage,
    * checking first in its cache.
    * @param eqs System of equations to be retrived
    * @return Result from Sage as a symbolic expression
    */
  def solveSymb(eqs:List[DiffEq]): SySolution
  def solveSymb(expr: SyExprAll): SyExprAll
  def solveSymb(cond: Cond, valua: Valuation): Boolean

  def solveSymb(valua:Valuation): Valuation =
    valua.view.mapValues(e => asSyExpr(solveSymb(e))).toMap
  def solveSymbExpr(expr:SyExpr): SyExpr =
    asSyExpr(solveSymb(expr))

}


object Solver {
  /**
    * collect list of unique variables used by a system of equations, including dummies
    * @param eqs system of equations
    * @return unique variables
    */
  def getVars(eqs:List[DiffEq]): List[String] =
    eqs.flatMap(getVars).distinct

  private def getVars(eq:DiffEq): List[String] =
    eq.v.v :: getVars(eq.e,eq.v.v)

  private def getVars(e:Lin,base:String): List[String] = e match {
    case Var(v) => List(v)
    case Value(v) => List("_"+base)
    case Add(l1, l2) => getVars(l1,base) ::: getVars(l2,base)
    case Mult(v, l) => getVars(l,base)
  }

  def getMatrix(eqs:List[DiffEq]): (List[String],List[List[Double]]) = {
    val vars = getVars(eqs)
    //println(s"## collected vars: ${vars.map("'"+_+"'").mkString(", ")}")
    val rows = eqs.map((x:DiffEq) => x.v.v -> getRow(vars,x.v.v,x.e)).toMap
    (  vars
      ,for (v<-vars) yield rows.getOrElse(v,vars.map(_ => 0.0))) // Note: set to 0 when unknown variable
  }

  private def getRow(vars:List[String],base:String,e:Lin): List[Double] = {
    val m = getRowValues(e,base)
    vars.map(x => m.getOrElse[Double](x,0))
  }






/////////////////////////////////////////////////////////////////
// ALTEREI!!!!!!!!!!!!!!!
  private def getRowValues(e:Lin,base:String): Map[String,Double] = e match {
    case Var(v) => Map(v->1)
    case Value(v) => Map(("_"+base)->v)
    case Add(l1, l2) => join(getRowValues(l1,base),getRowValues(l2,base))
    case Mult(l1, l2) => mult(getRowValues(l1,base),getRowValues(l2,base))
    //case Div(l1, l2) => div(getRowValues(l1,base),getRowValues(l2,base))
    //case Pow(l1, l2) => mypow(getRowValues(l1,base),getRowValues(l2,base))
    //case Sqrt(l1, l2) => mysqrt(getRowValues(l1,base),getRowValues(l2,base))
    //case Sin(l1) => mysin(getRowValues(l1,base))
    //case Cos(l1) => mycos(getRowValues(l1,base))
    //case Tan(l1) => mytan(getRowValues(l1,base))
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

  private def mult(m1:Map[String,Double],m2:Map[String,Double]): Map[String,Double] = {
    var res = m2
    for ((k, v1) <- m1) m2.get(k) match {
      case Some(v2) => val v = v1*v2; res += k -> v
      case None => res += k->v1
    }
    res
  }

////////////////////////////////////////////////////////////////












  private def list2Matrix(m:List[List[Double]]): DenseMatrix[Double] = {
    val rows = m.size
    val cols = m.headOption.getOrElse(Nil).size
//    println("before toMtr: "+m)
    DenseMatrix.create(cols,rows,m.flatten.toArray).t
  }



  def estimateDur(until:Until, eqs:List[DiffEq], x:Valuation, solver:Solver): Option[(Double,Option[Double=>String])] = {
    val sol = solver.evalFun(eqs) // maps variables to their solutions (function from t/ctx to value)
//    println(s"Estimating duration using point ${Show(x)}.")
    def guard: Double => Boolean =
      t => {
//        println(s" - guard ${c} @ $t")
//        solver.solveSymb(until.c,x)
        Eval(sol.view.mapValues(fun=>fun(t)(Eval(x))).toMap,until.c)
      }
//    println(" - going")
    val durValue = searchCond(until,guard,solver) // give value or do jumps searching for duration
    //debug(()=>s"Solving duration for ${Show(eqs)} & ${Show(diffEqs.dur)} - ${durValue}")
    //debug(()=>s"knowing ${Show(input)}")
//    println(s" - done $durValue")
    durValue
  }

  def searchCond[X](until:Until, guard:Double=>Boolean,solver:Solver): Option[(Double,Option[Double=>String])] = {
    if (guard(0.0)) Some((0.0,None))
       else
        //logSearch(1, 0, None)(guard, precision = until.eps)
        stepwiseSearch(0,until.jump,until.eps,guard,until.c,solver)
  }


  /**
    * Core function that solves a linear/coupled system of diferential equations using breeze libraries
    * @param mtx matrix
    * @return
    */
  def solveInvertable(mtx:List[List[Double]]): (List[Double],Double) => List[Double] = {
    val a = list2Matrix(mtx)
    inv(a) // throw an error if "a" is not invertable
    val b = breeze.linalg.inv
    val p = eig(a).eigenvectors
    val pi = inv(p)
    val ev = eig(a).eigenvalues

    println("## A:\n"+a)
    println("## p:\n"+p)
    println("## p^-1:\n"+pi)
    println("## eigV: "+ev.toArray.mkString(", "))

    def sol(x0:List[Double],t:Double): List[Double] = {
            println("##### E:\n"+diag(exp(ev *:* t)))
            println("##### pEp-1:\n"+p * diag(exp(ev *:* t)) * pi)
            println(s"##### x0=${x0.mkString(",")} ")
      val res = p * diag(exp(ev *:* t)) * pi * list2Matrix(x0.map(List(_)))
            println(s"--- sol($t) = ${res.t}")
      res.data.toList
    }
    sol
  }

  /**
    * Returns x(x0,t) = x0*e^At, where e^At is expended to the taylor series:
    *   e^At = I + At + At^2/2! + At^3/3! + At^4/4! + ...
    * @param mtx
    * @param precision
    * @return
    */
  def solveTaylor(mtx:List[List[Double]],precision:Double = 0.0000001):  (List[Double],Double) => List[Double] = {
    val a = list2Matrix(mtx)
    val size = a.rows // square matrix
    val id = DenseMatrix.eye[Double](size)

    def fact(n:Int):Int = if (n<=0) 1 else n * fact(n-1)
    def avg(m:DenseMatrix[Double]): Double = m.data.map(abs(_)).sum / m.size

    def sol(x0:List[Double],t:Double): List[Double] = {
      val at = a * t

      def iter(step:Int,bigA:DenseMatrix[Double],cur:DenseMatrix[Double]): DenseMatrix[Double] ={
        val nextBigA = bigA * at
        val next = nextBigA / fact(step).toDouble
        val res = cur + next
        // TODO: control precision and use error
        val av = avg(next)
//        println(s"avg: $av")
        if (av.isNaN || avg(next) < precision)
          res
        else
          iter(step+1,nextBigA,res)
      }

      val res = iter(1,id,id) * list2Matrix(x0.map(List(_)))
      res.data.toList
    }

    sol
  }


  /**
    * Returns x(x0,t) = x0*e^At, where e^At is expended to the taylor series:
    *   e^At = I + At + At^2/2! + At^3/3! + At^4/4! + ...
    * @param a matrix
    * @param precision
    * @return
    */
  def solveTaylorManual(a:List[List[Double]],precision:Double = 0.0000001, maxSteps:Int=100): (List[Double],Double) => List[Double] = {
    //println("building ODE "+a.mkString("/"))
    val size = a.size // square matrix

    type Row=List[Double]
    type Matrix=List[Row]

    def fact(n:Int):Int = if (n<=0) 1 else n * fact(n-1)
    def avg(m:Matrix): Double = m.flatten.map(myAbs).sum / (size*size)
    def myAbs(d: Double): Double = if(d<0) -d else d
    def myEye(size: Int, step: Int): Matrix = {
      if (step >= size)
        Nil
      else (for (p <- 0 until size) yield if (p == step) 1.0 else 0.0).toList :: myEye(size, step + 1)
    }

    val id = myEye(size,0)

    def mPlusm(m1: Matrix, m2: Matrix): Matrix = {
      (m1 zip m2).map(x => x._1.zip(x._2).map(y=>y._1+y._2))
    }
    def dotProd(v1:Row,v2:Row): Double =
      v1.zip(v2).
        map { t: (Double, Double) => t._1 * t._2 }.sum
    def transpose(m:Matrix):Matrix =
      if(m.head.isEmpty) Nil
      else m.map(_.head) :: transpose(m.map(_.tail))

    def mXm( m1:Matrix, m2:Matrix ): List[List[Double]] =
      for( m1row <- m1 ) yield
        for( m2col <- transpose(m2) ) yield
          dotProd( m1row, m2col )

    def sol(x0:List[Double],t:Double): List[Double] = {
      //print(s"t=$t -")
      val at = a.map(_.map(_*t))

      @scala.annotation.tailrec
      def iter(step:Int, bigA:List[List[Double]], cur:List[List[Double]]): List[List[Double]] ={
        val nextBigA: List[List[Double]] = mXm(bigA,at)
        val den = fact(step).toDouble
        val next = nextBigA.map(_.map(_/den))
        val res = mPlusm(cur,next)
        // TODO: control precision and use error
        val av = avg(next)
        //        println(s"avg: $av")
//        if (res.nonEmpty && res.head.nonEmpty && res.head.head.isNaN)
//          cur
        //else
        if (av.isNaN || avg(next) < precision || step>maxSteps)
          res
        else
          iter(step+1,nextBigA,res)
      }

      val res = mXm(iter(1,id,id) , x0.map(List(_)) )
      //println("res="+res.flatten.mkString(","))
      res.flatten
    }

    sol
  }



  ////////////////////////
  /// TESTING FUNCTIONS //
  ////////////////////////

//  def getDiffEqs(prog:Syntax): List[List[DiffEq]]  = prog match {
//    case d@DiffEqs(eqs, dur) => List(eqs)
//    //    case Seq(Nil) => Nil
//    case Seq(p::ps) =>
//      getDiffEqs(p) ::: getDiffEqs(Seq(ps))
//    //    case Skip => Nil
//    case ITE(ifP, thenP, elseP) =>
//      getDiffEqs(thenP) ++ getDiffEqs(elseP)
//    case While(c, doP) => getDiffEqs(doP)
//    case _ => Nil
//  }


//
//
//  def getFstDiffEq(prog:Syntax): DiffEqs  = prog match {
////    case Assign(v, e) => Nil
//    case d@DiffEqs(eqs, dur) => d
////    case Seq(Nil) => Nil
//    case Seq(p::ps) =>
//      try getFstDiffEq(p)
//      catch {
//        case _: Throwable => getFstDiffEq(Seq(ps))
//      }
////    case Skip => Nil
//    case ITE(ifP, thenP, elseP) =>
//      try getFstDiffEq(thenP)
//      catch {
//        case _: Throwable => getFstDiffEq(elseP)
//      }
//    case While(c, doP) => getFstDiffEq(doP)
//    case _ => throw new RuntimeException("no diff. eq. found")
//  }
//
//  def getFstMatrix(prog:Syntax): List[List[Double]] =
//    getMatrix(getFstDiffEq(prog).eqs)._2
//
//  def tryM(prog:String): List[List[Double]] = getFstMatrix(DSL.parse(prog))
//
//
//
//  /**
//    * Performs a gradient descent, i.e., searches for an argument "t" of "function" s.t. function(t) = 0.
//    * From wikipedia - not in use.
//    * @param previousStepSize how closer it got since the last try
//    * @param curX initial value
//    * @param function to find a 0
//    * @param precision goal (the new step being smaller than the precision)
//    * @param gamma difference used to calculate derivative
//    * @return the argument of the function that yields 0
//    */
//  def gradientDescent(previousStepSize: Double= 1/0.000001, curX: Double = 0,
//                      function: Double => Double,
//                      precision: Double = 0.000001, gamma: Double = 0.01): Double = {
//    if (previousStepSize > precision) {
//      val newX = curX + -gamma * function(curX)
//      println(curX)
//      // update previousStepSize and curX
//      gradientDescent(abs(newX - curX), newX, function, precision, gamma)
//    } else curX
//  }
////  val ans = gradientDescent(previousStepSize, curX, precision, gamma)
////  println(s"The local minimum occurs at $ans")

  /**
    * Searches for the earliest value at which a given guard becomes true.
    * Initially it grows exponentially until it finds a "true",
    * once it does, searches for the initial value up to some precision.
    * Assumes that, once the guard it true, it will always be true.
    *
    * @param jump The distance backwards when the guard was false.
    * @param curX Current value being evaluated
    * @param guard The function that checks if the value holds
    * @param precision The maximum error
    * @return The earliest value at which the guard is true, if it exists before "max", and the largest value traversed
    */
  @scala.annotation.tailrec
  def logSearchRec(jump:Double, curX: Double, last:Option[(Double,Double)])
                  (implicit guard: Double => Boolean,
                   precision: Double = 0.00000001, max: Int = 1000000) : Option[(Double,Double)] = {
    //println(s"[SS-log] $curX by $jump (last: $last)")
    if (max<curX)  return None

    last match {
      // no value found yet - growing search
      case None =>
        if (guard(curX)) // FOUND! define last, start shrinking
        logSearchRec(jump/8,curX - jump/4, Some((curX,curX)))
        else // need to jump more
        logSearchRec(jump*2,curX + jump  ,None)

      // growing steps until guard succeeds
      case Some((v1,v2)) =>
        if (jump*2 > precision) { // shrinking steps until having the right precision
          if (guard(curX))
            logSearchRec(jump/2,curX - jump, Some(curX,v2))
          else
            logSearchRec(jump/2,curX + jump,last)
        }
        else {
          //println(s"[SS-log] got $last (precision: ${jump*2})")
          last
        }
    }
  }

  /**
    * Searches for the earliest time `t` a predicate `guard(t)` holds by jumping and trying
    * @param curX next time value to try
    * @param bigStep   if None: smallStep is the naive step
    *                  if Some(step): just forward step until guard(t), then logSearch with precision smallStep
    * @param smallStep if None: ignore bigstep, and do log search up and down with fixed precision
    *                  if Some(step): use step as jump forward of precision, based on bigStep.
    * @param guard predicate parameterised by the time
    * @return
    */
  def stepwiseSearch(curX:      Double,
                     bigStep:   Option[Double],
                     smallStep: Option[Double],
                     guard:     Double => Boolean,
                     guardCond: Cond,
                     solver:    Solver): Option[(Double,Option[Double=>String])] = {
    //println(s"[SS-jump] $curX by $jump")
    (bigStep,smallStep) match {
      case (_,None)                  =>
        logSearch(1,curX,curX,None,guard,guardCond,100000000)
      case (None,Some(step))         =>
        stepwiseSearchSmallSteps(SVal(curX))(guard,SVal(step),100000000,solver).map((_,None))
      case (Some(bstep),Some(sstep)) =>
        stepwiseSearchBigSteps(SVal(curX))((SVal(bstep),sstep),guard,guardCond,100000000,solver)
    }
  }

  @scala.annotation.tailrec
  private def stepwiseSearchBigSteps(curX: SyExpr)
                                    (implicit bigSmallStep:(SyExpr,Double),
                                     guard: Double=>Boolean, guardCond:Cond,
                                     max:Double,
                                     solver:Solver): Option[(Double,Option[Double=>String])] = {
    val now = Eval(curX)
    if (max<now)
      None
    else if (guard(now)) // overshoot - now go slowly
    logSearch(
      Eval(bigSmallStep._1)/4,
      now - Eval(bigSmallStep._1)/2,
      now - Eval(bigSmallStep._1),
      Some(now),
      guard,guardCond,max.toInt,bigSmallStep._2)
    else
    stepwiseSearchBigSteps(solver.solveSymbExpr(asSyExpr(curX +bigSmallStep._1)))
  }

  @scala.annotation.tailrec
  private def stepwiseSearchSmallSteps(curX: SyExpr)
                                      (implicit guard: Double => Boolean,
                                       step:SyExpr,
                                       max:Int,
                                       solver:Solver): Option[Double] = {
    //println(s"[SS-basic] $curX by $step")
    val now = Eval(curX)
    if      (max<now)     None
    else if (guard(now))  Some(now)
    else                  stepwiseSearchSmallSteps(solver.solveSymbExpr(asSyExpr(curX+step)))
  }

  def logSearch(jump:Double, curX: Double, first:Double, last:Option[Double], guard: Double=>Boolean,
                guardCond: Cond,
                max: Int,
                precision: Double = 0.00000001) : Option[(Double,Option[Double=>String])] = {
    val res = logSearchRec(jump,curX,last.map(x=>(x,x)))(guard,precision,max)
    res.map(res => (res._1 , Some( t =>
      //        s"found logsearch at ${res._1} starting from ${first} until ${res._2}, curX=$curX"
      s"Assuming ${Show(guardCond)} becomes true only once.</br>" +
        s"Interval: ${t-(res._1-first)} -> ${t+(res._2-res._1)}</br>" +
        s"Precision: $precision"
    )))
  }


  /////////////////////////////////////////////////
//
//  /**
//    * Searches for the earliest value at which a given guard becomes true.
//    * Initially it grows exponentially until it finds a "true",
//    * once it does, searches for the initial value up to some precision.
//    * Assumes that, once the guard it true, it will always be true.
//    *
//    * @param jump The distance backwards when the guard was false.
//    * @param curX Current value being evaluated
//    * @param guard The function that checks if the value holds
//    * @param precision The maximum error
//    * @return The earliest value at which the guard is true, if it exists before "max", and the largest value traversed
//    */
//  @scala.annotation.tailrec
//  def logSearchRec(jump:Double, curX: Double, last:Option[(SyExpr,SyExpr)])
//                  (implicit guard: SyExpr => Boolean,
//                   precision: Double = 0.00000001, max: Int = 1000000) : Option[(SyExpr,SyExpr)] = {
//    //println(s"[SS-log] $curX by $jump (last: $last)")
//    if (max<curX)  return None
//
//    last match {
//      // no value found yet - growing search
//      case None =>
//        if (guard(SVal(curX))) // FOUND! define last, start shrinking
//        logSearchRec(jump/8,curX - jump/4, Some((SVal(curX),SVal(curX))))
//        else // need to jump more
//        logSearchRec(jump*2,curX + jump  ,None)
//
//      // growing steps until guard succeeds
//      case Some((v1,v2)) =>
//        if (jump*2 > precision) { // shrinking steps until having the right precision
//          if (guard(SVal(curX)))
//            logSearchRec(jump/2,curX - jump, Some(SVal(curX),v2))
//          else
//            logSearchRec(jump/2,curX + jump,last)
//        }
//        else {
//          //println(s"[SS-log] got $last (precision: ${jump*2})")
//          last
//        }
//    }
//  }
//
//  /**
//    * Searches for the earliest time `t` a predicate `guard(t)` holds by jumping and trying
//    * @param curX next time value to try
//    * @param bigStep   if None: smallStep is the naive step
//    *                  if Some(step): just forward step until guard(t), then logSearch with precision smallStep
//    * @param smallStep if None: ignore bigstep, and do log search up and down with fixed precision
//    *                  if Some(step): use step as jump forward of precision, based on bigStep.
//    * @param guard predicate parameterised by the time
//    * @return
//    */
//  def stepwiseSearch(curX:      SyExpr,
//                     bigStep:   Option[Double],
//                     smallStep: Option[Double],
//                     guard:     SyExpr => Boolean,
//                     guardCond: Cond,
//                     solver:    Solver): Option[(SyExpr,Option[Double=>String])] = {
//    //println(s"[SS-jump] $curX by $jump")
//    (bigStep,smallStep) match {
//      case (_,None)                  =>
//        logSearch(1,Eval(curX),curX,None,guard,guardCond,100000000)
//      case (None,Some(step))         =>
//        stepwiseSearchSmallSteps(curX)(guard,step,(Eval(curX)+1000000*step).toInt,solver).map((_,None))
//      case (Some(bstep),Some(sstep)) =>
//        stepwiseSearchBigSteps(curX)((bstep,sstep),guard,guardCond,Eval(curX)+1000000*bstep,solver)
//    }
//  }
//
//  @scala.annotation.tailrec
//  private def stepwiseSearchBigSteps(curX: SyExpr)
//                                    (implicit bigSmallStep:(Double,Double),
//                                     guard: SyExpr=>Boolean, guardCond:Cond,
//                                     max:Double,
//                                     solver:Solver): Option[(SyExpr,Option[Double=>String])] = {
//    if (max<Eval(curX))
//      None
//    else if (guard(curX)) // overshoot - now go slowly
//    logSearch(
//      bigSmallStep._1/4,
//      Eval(curX) - bigSmallStep._1/2,
//      solver.solveSymbExpr(SSub[Pure](curX,SVal(bigSmallStep._1))),
//      Some(curX),
//      guard,guardCond,max.toInt,bigSmallStep._2)
//    else
//    stepwiseSearchBigSteps(solver.solveSymbExpr(SAdd[Pure](curX , SVal(bigSmallStep._1))))
//  }
//
//  @scala.annotation.tailrec
//  private def stepwiseSearchSmallSteps(curX: SyExpr)
//                                      (implicit guard: SyExpr => Boolean,
//                                       step:Double,
//                                       max:Int,
//                                       solver:Solver): Option[SyExpr] = {
//    //println(s"[SS-basic] $curX by $step")
//    if      (max<Eval(curX))     None
//    else if (guard(curX))  Some(curX)
//    else                   stepwiseSearchSmallSteps(solver.solveSymbExpr(SAdd[Pure](curX,SVal(step))))
//  }
//
//  def logSearch(jump:Double, curX: Double, first:SyExpr, last:Option[SyExpr], guard: SyExpr=>Boolean,
//                guardCond: Cond,
//                max: Int,
//                precision: Double = 0.00000001) : Option[(SyExpr,Option[Double=>String])] = {
//    val res = logSearchRec(jump,curX,last.map(x=>(x,x)))(guard,precision,max)
//    res.map(res => (res._1 , Some( t =>
//      //        s"found logsearch at ${res._1} starting from ${first} until ${res._2}, curX=$curX"
//      s"Assuming ${Show(guardCond)} becomes true only once</br>" +
//        s"Interval: ${t-(Eval(res._1)-Eval(first))} -> ${t+(Eval(res._2)-Eval(res._1))}" +
//        s"Precision: $precision</br>"
//      //      s"on $curX-${res._2}"
//    )))
//  }

  }
