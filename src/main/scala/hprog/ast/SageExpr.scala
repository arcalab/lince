package hprog.ast

import hprog.ast.SageExpr.SExprFun
import hprog.backend.Show

object SageExpr {
  trait All
  trait Arg extends All
  trait Var extends All
  trait Pure extends Var with Arg

  type SExpr    = SageExpr[Pure]
  type SExprV   = SageExpr[Var]
  type SExprT   = SageExpr[Arg]
  type SExprFun = SageExpr[All]
}

sealed abstract class SageExpr[+X<:SageExpr.All] {
  override def toString: String = this match {
//    case SVal(v) if (v-v.round)==0 => v.toInt.toString
//    case SVal(v) => v.toString
    case s:SVal => Show.floatToFraction(s.v)
    case _:SArg => "t"
    case s:SVar => s.v
    case SFun(f, args) => s"$f(${args.mkString(",")})"
    case SDiv(e1, e2) => s"$e1/$e2"
    case SMult(e1, e2) => s"$e1*$e2"
    case SPow(e1, e2) => s"$e1^$e2"
    case SAdd(e1, e2) => s"$e1+$e2"
    //case SSub(SVal(0.0), e2) => s"-$e2"
    case SSub(e1, e2) => s"$e1-$e2"
  }
  def +(that: SExprFun): SExprFun = SAdd(this,that)
  def -(that: SExprFun): SExprFun = SSub(this,that)
  def *(that: SExprFun): SExprFun = SMult(this,that)
  def /(that: SExprFun): SExprFun = SDiv(this,that)
  def ^(that: SExprFun): SExprFun = SPow(this,that)
//  def -[X2<:X](that: SageExpr[X2]): SageExpr[X] = SSub(this,that)
//  def *[X2<:X](that: SageExpr[X2]): SageExpr[X] = SMult(this,that)
//  def /[X2<:X](that: SageExpr[X2]): SageExpr[X] = SDiv(this,that)
//  def ^[X2<:X](that: SageExpr[X2]): SageExpr[X] = SPow(this,that)

}

case class SVal(v:Double)                     extends SageExpr[SageExpr.Pure]
case class SArg()                             extends SageExpr[SageExpr.Arg]
case class SVar(v:String)                     extends SageExpr[SageExpr.Var]
case class SFun[X<:SageExpr.All](f:String,args:List[SageExpr[X]])
                                                                 extends SageExpr
case class SDiv[X<:SageExpr.All](e1:SageExpr[X],e2:SageExpr[X])  extends SageExpr[X]
case class SMult[X<:SageExpr.All](e1:SageExpr[X],e2:SageExpr[X]) extends SageExpr[X]
case class SPow[X<:SageExpr.All](e1:SageExpr[X],e2:SageExpr[X])  extends SageExpr[X]
case class SAdd[X<:SageExpr.All](e1:SageExpr[X],e2:SageExpr[X])  extends SageExpr[X]
case class SSub[X<:SageExpr.All](e1:SageExpr[X],e2:SageExpr[X])  extends SageExpr[X]
