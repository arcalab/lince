package hprog.frontend

import breeze.linalg._
import breeze.numerics._
import hprog.DSL
import hprog.ast._

object Solver {
  /**
    * collect list of unique variables used by a system of equations, including dummies
    * @param eqs
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

//  def getMatrixAndConstants(eqs: DiffEqs):  (List[String],List[List[Double]],List[Double]) = {
//    val (vars,mtx) = Solver.getMatrix(eqs)
//    val i = vars.indexOf("")
//    if (i == -1)
//      (vars,mtx,vars.map(_=>0.0))
//    else
//      (vars - "" , mtx.map(_.drop(i)) , mtx.map(_(i)))
//  }

  private def getRow(vars:List[String],base:String,e:Lin): List[Double] = {
    val m = getRowValues(e,base)
    vars.map(x => m.getOrElse[Double](x,0))
  }

  private def getRowValues(e:Lin,base:String): Map[String,Double] = e match {
    case Var(v) => Map(v->1)
    case Value(v) => Map(("_"+base)->v)
    case Add(l1, l2) => join(getRowValues(l1,base),getRowValues(l2,base))
    case Mult(v, l) => getRowValues(l,base).mapValues(_*v.v)
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


  def solveDur[X](dur:Dur, input:X, guard:Cond=>Double=>Boolean) = dur match {
    case For(t) => Some(t.v)
    case Until(c) => if (guard(c)(0)) Some(0.0)
                     else logSearch(1, 0, None, guard(c))
    case Forever => None
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
    * @param mtx
    * @param precision
    * @return
    */
  def solveTaylorManual(a:List[List[Double]],precision:Double = 0.0000001):  (List[Double],Double) => List[Double] = {
    //println("building ODE "+a.mkString("/"))
    val size = a.size // square matrix

    type Row=List[Double]
    type Matrix=List[Row]

    def fact(n:Int):Int = if (n<=0) 1 else n * fact(n-1)
    def avg(m:Matrix): Double = m.flatten.map(myAbs(_)).sum / (size*size)
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
    def dotProd(v1:Row,v2:Row) =
      v1.zip(v2).
        map { t: (Double, Double) => t._1 * t._2 }.sum
    def transpose(m:Matrix):Matrix =
      if(m.head.isEmpty) Nil
      else m.map(_.head) :: transpose(m.map(_.tail))

    def mXm( m1:Matrix, m2:Matrix ) =
      for( m1row <- m1 ) yield
        for( m2col <- transpose(m2) ) yield
          dotProd( m1row, m2col )

    def sol(x0:List[Double],t:Double): List[Double] = {
      //print(s"t=$t -")
      val at = a.map(_.map(_*t))

      def iter(step:Int,bigA:List[List[Double]],cur:List[List[Double]]): List[List[Double]] ={
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
        if (av.isNaN || avg(next) < precision)
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


//  def solveEqs(eqs: List[DiffEq]): Trajectory = eqs match {
//    case Nil => Trajectory.empty
//    case t1::Nil => solve(t1)
//    case t1::tl =>
//  }

  def getFstDiffEq(prog:Syntax): DiffEqs  = prog match {
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

  def getFstMatrix(prog:Syntax): List[List[Double]] =
    getMatrix(getFstDiffEq(prog).eqs)._2

  def tryM(prog:String): List[List[Double]] = getFstMatrix(DSL.parse(prog))



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
  def logSearch(prevStepSize:Double, curX: Double, last:Option[Double],
                guard: Double => Boolean,
                precision: Double = 0.0000001, max: Int = 1000000) : Option[Double] = {
//    println(s"${if (expanding) "EXPANDING" else "DIVING "} at $curX with step $prevStepSize")
    if (max<curX)  return None

    last match {
      // no value found yet
      case None =>
        if (guard(curX))
          logSearch(prevStepSize/8,curX - prevStepSize/4,Some(curX),guard,precision,max)
        else
          logSearch(prevStepSize*2,curX + prevStepSize  ,None      ,guard,precision,max)

      // growing steps until guard succeeds
      case Some(value) =>
        if (prevStepSize > precision) { // shrinking steps until having the right precision
          if (guard(curX))
            logSearch(prevStepSize/2,curX - prevStepSize,Some(curX),guard,precision,max)
          else
            logSearch(prevStepSize/2,curX + prevStepSize,last      ,guard,precision,max)
        }
        else last
        // TODO: control precision and errors
    }
  }
}
