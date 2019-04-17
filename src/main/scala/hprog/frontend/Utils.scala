package hprog.frontend

import hprog.ast._

object Utils {
  def replaceVar(by:String=>String,e:Lin): Lin = e match {
    case Var(v) => Var(by(v))
    case Value(v) => e
    case Add(l1, l2) => Add(replaceVar(by,l1),replaceVar(by,l2))
    case Mult(v, l) => Mult(v,replaceVar(by,l))
  }

  def getDiffEqs(prog:Syntax): List[List[DiffEq]]  = prog match {
    case d@DiffEqs(eqs, _) => List(eqs)
    case hprog.ast.Seq(p :: ps) =>
      getDiffEqs(p) ::: getDiffEqs(hprog.ast.Seq(ps))
    case ITE(_, thenP, elseP) =>
      getDiffEqs(thenP) ++ getDiffEqs(elseP)
    case While(c, doP) => getDiffEqs(doP)
    case _ => Nil // Seq(Nil) and Skip
  }

  def getDefVars(eqs: List[DiffEq]): Set[String] =
    eqs.map(_.v.v).toSet

  def getUsedVars(eqs: List[DiffEq]): Set[String] =
    eqs.flatMap(eq => getVars(eq.e)).toSet

  def getVars(lin: Lin): Set[String] = lin match {
    case Var(v) => Set(v)
    case Value(_) => Set()
    case Add(l1, l2) => getVars(l1) ++ getVars(l2)
    case Mult(_, l) => getVars(l)
  }
}
