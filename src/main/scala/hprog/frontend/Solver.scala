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

  def getRowValues(e:Lin): Map[String,Double] = e match {
    case Var(v) => Map(v->1)
    case Value(v) => Map(""->v)
    case Add(l1, l2) => join(getRowValues(l1),getRowValues(l2))
    case Mult(v, l) => getRowValues(l).mapValues(_*v.v)
  }
  private def multt(d:Double,d2:Double): Double = d*d2
  private def join(m1:Map[String,Double],m2:Map[String,Double]): Map[String,Double] = {
    var res = m2
    for ((k, v1) <- m1) m2.get(k) match {
      case Some(v2) => val v = v1+v2; res += k -> v
      case None => res += k->v1
    }
    res
  }

  def getRow(vars:List[String],e:Lin): List[Double] = {
    val m = getRowValues(e)
    vars.map(x => m.getOrElse[Double](x,0))
  }

  def getMatrix(eqs:DiffEqs): List[List[Double]] = {
    val vars = getVars(eqs)
    println(s"## vars: ${vars.map("'"+_+"'").mkString(", ")}")
    val rows = eqs.eqs.map((x:DiffEq) => x.v.v -> getRow(vars,x.e)).toMap
    for (v<-vars) yield rows.getOrElse(v,vars.map(_ => 0.0))
  }

  private def list2Matrix(m:List[List[Double]]): DenseMatrix[Double] = {
    val rows = m.size
    val cols = m.headOption.getOrElse(Nil).size
//    println("before toMtr: "+m)
    DenseMatrix.create(cols,rows,m.flatten.toArray).t
  }

  def solve(eqs:DiffEqs): scala.Seq[Double] => Double => DenseMatrix[Double] = {
    val a = list2Matrix(getMatrix(eqs))
    val p = eig(a).eigenvectors
    val pi = inv(p)
    val ev = eig(a).eigenvalues
    def sol(x0:List[Double],t:Double) =
      p * exp(diag(ev *:* t)) * pi * list2Matrix(x0.map(List(_)))

    println("## A:\n"+a)
    println("## p:\n"+p)
    println("## p^-1:\n"+pi)
    println("## eigV: "+ev.toArray.mkString(", "))
    x0:scala.Seq[Double] => (t:Double) => sol(x0.toList,t)
  }



  /// TESTING FUNCTIONS


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
    getMatrix(getFstDiffEq(prog))

  def tryM(prog:String): List[List[Double]] = getFstMatrix(DSL.parse(prog))

  def tryS(prog:String): scala.Seq[Double] => Double => DenseMatrix[Double] = solve(getFstDiffEq(DSL.parse(prog)))

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

}
