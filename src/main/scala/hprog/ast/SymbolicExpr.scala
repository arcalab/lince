package hprog.ast

import hprog.ast.SymbolicExpr.{All, Time, Pure, SyExprAll, Vars}
import hprog.backend.Show

object SymbolicExpr {
  trait All
  trait Time extends All
  trait Vars extends All
  trait Pure extends Vars with Time

  type SyExpr     = SymbolicExpr[Pure]
  type SyExprVar  = SymbolicExpr[Vars]
  type SyExprTime = SymbolicExpr[Time]
  type SyExprAll  = SymbolicExpr[All]
}

sealed trait SymbolicExpr[+X<:SymbolicExpr.All] {
  override def toString: String = this match {
//    case SVal(v) if (v-v.round)==0 => v.toInt.toString
//    case SVal(v) => v.toString
    case s:SVal => Show.floatToFraction(s.v)
    case _:SArg => "t"
    case s:SVar => s.v
    case SFun(f, args) => s"$f(${args.mkString(",")})"
    case SDiv(e1, e2) => s"$e1/$e2"
    case SRes(e1,e2) => s"$e1%$e2"
    case SMult(e1, e2) => s"$e1*$e2"
    case SPow(e1, e2) => s"$e1^$e2"
    case SAdd(e1, e2) => s"$e1+$e2"
    //case SSub(SVal(0.0), e2) => s"-$e2"
    case SSub(e1, e2) => s"$e1-$e2"
  }
  def +(that: SyExprAll): SyExprAll = SAdd(this,that)
  def -(that: SyExprAll): SyExprAll = SSub(this,that)
  def *(that: SyExprAll): SyExprAll = SMult(this,that)
  def /(that: SyExprAll): SyExprAll = SDiv(this,that)
  def ^(that: SyExprAll): SyExprAll = SPow(this,that)

  //  def -[X2<:X](that: SageExpr[X2]): SageExpr[X] = SSub(this,that)
//  def *[X2<:X](that: SageExpr[X2]): SageExpr[X] = SMult(this,that)
//  def /[X2<:X](that: SageExpr[X2]): SageExpr[X] = SDiv(this,that)
//  def ^[X2<:X](that: SageExpr[X2]): SageExpr[X] = SPow(this,that)

}

case class SVal(v:Double)                     extends SymbolicExpr[Pure]
case class SArg()                             extends SymbolicExpr[Time]
case class SVar(v:String)                     extends SymbolicExpr[Vars]
case class SFun[X<:All](f:String, args:List[SymbolicExpr[X]])
                                              extends SymbolicExpr[X]
case class SDiv[X<:All](e1:SymbolicExpr[X], e2:SymbolicExpr[X])  extends SymbolicExpr[X]
case class SRes[X<:All](e1:SymbolicExpr[X], e2:SymbolicExpr[X])  extends SymbolicExpr[X]
case class SMult[X<:All](e1:SymbolicExpr[X], e2:SymbolicExpr[X]) extends SymbolicExpr[X]
case class SPow[X<:All](e1:SymbolicExpr[X], e2:SymbolicExpr[X])  extends SymbolicExpr[X]
case class SAdd[X<:All](e1:SymbolicExpr[X], e2:SymbolicExpr[X])  extends SymbolicExpr[X]
case class SSub[X<:All](e1:SymbolicExpr[X], e2:SymbolicExpr[X])  extends SymbolicExpr[X]
