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
import scala.math._


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

//New
  private def getVars(e:NotLin,base:String): List[String] = e match {
    case VarNotLin(v) => List(v)
    case ValueNotLin(v) => List()
    case AddNotLin(l1, l2) => getVars(l1,base) ::: getVars(l2,base)
    case MultNotLin(l1, l2) => getVars(l1,base) ::: getVars(l2,base)
    case DivNotLin(l1,l2) => getVars(l1,base) ::: getVars(l2,base)
    case ResNotLin(l1,l2) => getVars(l1,base) ::: getVars(l2,base)
    case PowNotLin(l1,l2) => getVars(l1,base) ::: getVars(l2,base)
    case FuncNotLin(s,list) => getVarsAux(list,base)
  }

//new
  def getVarsAux(list:List[NotLin],base:String): List[String] = list match {
    case List() => List()
    case n::List() => getVars(n,base)
    case n::ns => getVars(n,base) ::: getVarsAux(ns,base)
  }  
  def getVars_Numerical(eqs:List[DiffEq]): List[String] =
    eqs.flatMap(getVars_Numerical).distinct

  private def getVars_Numerical(eq:DiffEq): List[String] =
    eq.v.v :: getVars_Numerical(eq.e,eq.v.v)

  private def getVars_Numerical(e:NotLin,base:String): List[String] = e match {
    case VarNotLin(v) => List(v)
    case ValueNotLin(v) => List("_"+base)
    case AddNotLin(l1, l2) => getVars_Numerical(l1,base) ::: getVars_Numerical(l2,base)
    case MultNotLin(l1, l2) => getVars_Numerical(l1,base) ::: getVars_Numerical(l2,base)
    case DivNotLin(l1,l2) => getVars_Numerical(l1,base) ::: getVars_Numerical(l2,base)
    case ResNotLin(l1,l2) => getVars_Numerical(l1,base) ::: getVars_Numerical(l2,base)
    case PowNotLin(l1,l2) => getVars_Numerical(l1,base) ::: getVars_Numerical(l2,base)
    case FuncNotLin(s,list) => getVarsAux_Numerical(list,base)
  }
//new
  def getVarsAux_Numerical(list:List[NotLin],base:String): List[String] = list match {
    case List() => List()
    case n::List() => getVars_Numerical(n,base)
    case n::ns => getVars_Numerical(n,base) ::: getVarsAux_Numerical(ns,base)
  }  

  def getMatrix(eqs:List[DiffEq]): (List[String],List[List[Double]]) = {
    val vars = getVars_Numerical(eqs)
    println("vars_getmatrix:",vars)
    val rows = eqs.map((x:DiffEq) => x.v.v -> getRow(vars,x.v.v,x.e)).toMap
    println("row_getmatrix:",rows)
    (  vars
      ,for (v<-vars) yield rows.getOrElse(v,vars.map(_ => 0.0))) // Note: set to 0 when unknown variable
  }


  private def getRow(vars:List[String],base:String,e:NotLin): List[Double] = {
    val m = getRowValues(e,base)
    println("m_getrow:",m)
    println("vars.map(x => m.getOrElse[Double](x,0)):",vars.map(x => m.getOrElse[Double](x,0)))
    return vars.map(x => m.getOrElse[Double](x,0))
  }







  private def getRowValues(e:NotLin,base:String): Map[String,Double] = {
    var res:Map[String,Double]=e match {
            case VarNotLin(v) => Map(v->1)
            case ValueNotLin(v) => Map(("_"+base)->v) //New 
            case AddNotLin(l1, l2) => join(getRowValues(l1,base),getRowValues(l2,base))
            case MultNotLin(l1, l2) => multjoin(getRowValues(l1,base),getRowValues(l2,base)) //new
            case DivNotLin(l1,l2) => divjoin(getRowValues(l1,base),getRowValues(l2,base)) //new
            case ResNotLin(l1,l2) => resjoin(getRowValues(l1,base),getRowValues(l2,base)) //new
            case PowNotLin(l1,l2) => powjoin(getRowValues(l1,base),getRowValues(l2,base)) //new
            case FuncNotLin(s,list) => funcjoin(s,list.map((l:NotLin) => getRowValues(l,base)),base) //new
             
          }
      println(s"getRowValues: notlin->${e} to ${res}")
      res
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

//New
    private def multjoin(m1:Map[String,Double],m2:Map[String,Double]): Map[String,Double] = {
    var res = m2
    for ((k, v1) <- m1) m2.get(k) match {
      case Some(v2) => val v = v1*v2; res += k -> v
      case None => res += k->v1
    }
    res
  }


//New
  private def divjoin(m1:Map[String,Double],m2:Map[String,Double]): Map[String,Double] = {
    var res = m2
    for ((k, v1) <- m1) m2.get(k) match {
      case Some(v2) => val v = v1/v2; res += k -> v
      case None => res += k->v1
    }
    res
  }

  //New
  private def resjoin(m1:Map[String,Double],m2:Map[String,Double]): Map[String,Double] = {
    var res = m2
    for ((k, v1) <- m1) m2.get(k) match {
      case Some(v2) => val v = v1%v2; res += k -> v
      case None => res += k->v1
    }
    res
  }


//New
  private def powjoin(m1:Map[String,Double],m2:Map[String,Double]): Map[String,Double] = {
    var res = m2
    for ((k, v1) <- m1) m2.get(k) match {
      case Some(v2) => val v = math.pow(v1,v2); res += k -> v
      case None => res += k->v1
    }
    res
  }


//New
  private def funcjoin(s:String,list:List[Map[String,Double]],base:String):Map[String,Double] ={
      if(list.length == 0 || list.length>2){
        s match {
          case ("PI") => Map(("_"+base) -> math.Pi) //new
          case ("E") => Map(("_"+base) -> math.E) // new
          case (_) => throw new RuntimeException(s"Unknown function '${s}',or the number of arguments are incorrect")
        }
        
      }
      else {
        if (list.length == 1){
          s match {
            case ("exp") => list(0).map(v => v._1 -> math.exp(v._2))
            case ("sin") => list(0).map(v => v._1 -> math.sin(v._2))
            case ("cos") => list(0).map(v => v._1 -> math.cos(v._2))
            case ("tan") => list(0).map(v => v._1 -> math.tan(v._2))
            case ("arcsin") => list(0).map(v => v._1 -> math.asin(v._2))
            case ("arccos") => list(0).map(v => v._1 -> math.acos(v._2))
            case ("arctan") => list(0).map(v => v._1 -> math.atan(v._2))
            case ("sinh") => list(0).map(v => v._1 -> math.sinh(v._2))
            case ("cosh") => list(0).map(v => v._1 -> math.cosh(v._2))
            case ("tanh") => list(0).map(v => v._1 -> math.tanh(v._2))
            case ("sqrt") => list(0).map(v => v._1 -> math.sqrt(v._2))
            case ("log") => list(0).map(v => v._1 -> math.log(v._2))
            case ("log10") => list(0).map(v => v._1 -> math.log10(v._2))
            case (_)=>throw new RuntimeException(s"Unknown function '${s}',or the number of arguments are incorrect")
          }
        }
        else {
          s match {
            case ("max") => list(0) ++ (for ((x,v) <- list(1)) yield x -> (if (list(0).contains(x))  math.max((list(0))(x),v) else v))
            case ("min") => list(0) ++ (for ((x,v) <- list(1)) yield x -> (if (list(0).contains(x))  math.min((list(0))(x),v) else v))
            case (_)=>throw new RuntimeException(s"Unknown function '${s}',or the number of arguments are incorrect")
          }  
        }

      }
    }



  @deprecated
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
    DenseMatrix.create(cols,rows,m.flatten.toArray).t
  }



  def estimateDur(until:Until, eqs:List[DiffEq], x:Valuation, solver:Solver): Option[(Double,Option[Double=>String])] = {
    val sol = solver.evalFun(eqs) // maps variables to their solutions (function from t/ctx to value)
    def guard: Double => Boolean =
      t => {
        Eval(sol.view.mapValues(fun=>fun(t)(Eval(x))).toMap,until.c)
      }
    val durValue = searchCond(until,guard,solver) // give value or do jumps searching for duration
    durValue
  }

  def searchCond[X](until:Until, guard:Double=>Boolean,solver:Solver): Option[(Double,Option[Double=>String])] = {
    if (guard(0.0)) Some((0.0,None))
       else
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
            println("##### E:\n"+diag(breeze.numerics.exp(ev *:* t)))
            println("##### pEp-1:\n"+p * diag(breeze.numerics.exp(ev *:* t)) * pi)
            println(s"##### x0=${x0.mkString(",")} ")
      val res = p * diag(breeze.numerics.exp(ev *:* t)) * pi * list2Matrix(x0.map(List(_)))
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
    def avg(m:DenseMatrix[Double]): Double = m.data.map(breeze.numerics.abs(_)).sum / m.size

    def sol(x0:List[Double],t:Double): List[Double] = {
      val at = a * t

      def iter(step:Int,bigA:DenseMatrix[Double],cur:DenseMatrix[Double]): DenseMatrix[Double] ={
        val nextBigA = bigA * at
        val next = nextBigA / fact(step).toDouble
        val res = cur + next
        // TODO: control precision and use error
        val av = avg(next)
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
    println("id_STM:",id)

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
      val at = a.map(_.map(_*t))

      @scala.annotation.tailrec
      def iter(step:Int, bigA:List[List[Double]], cur:List[List[Double]]): List[List[Double]] ={
        val nextBigA: List[List[Double]] = mXm(bigA,at)
        val den = fact(step).toDouble
        val next = nextBigA.map(_.map(_/den))
        val res = mPlusm(cur,next)
        // TODO: control precision and use error
        val av = avg(next)
        if (av.isNaN || avg(next) < precision || step>maxSteps)
          res
        else
          iter(step+1,nextBigA,res)
      }

      val res = mXm(iter(1,id,id) , x0.map(List(_)) )
      res.flatten
    }

    sol
  }



 



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




  }
