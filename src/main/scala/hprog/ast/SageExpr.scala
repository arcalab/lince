package hprog.ast

import hprog.backend.Show

sealed abstract class SageExpr {
  override def toString: String = this match {
//    case SVal(v) if (v-v.round)==0 => v.toInt.toString
//    case SVal(v) => v.toString
    case SVal(v) => Show.floatToFraction(v)
    case SArg => "t"
    case SVar(v) => v
    case SFun(f, args) => s"$f(${args.mkString(",")})"
    case SDiv(e1, e2) => s"$e1/$e2"
    case SMult(e1, e2) => s"$e1*$e2"
    case SPow(e1, e2) => s"$e1^$e2"
    case SAdd(e1, e2) => s"$e1+$e2"
    case SSub(SVal(0.0), e2) => s"-$e2"
    case SSub(e1, e2) => s"$e1-$e2"
  }
  def +(that: SageExpr): SageExpr = SAdd(this,that)
  def -(that: SageExpr): SageExpr = SSub(this,that)
  def *(that: SageExpr): SageExpr = SMult(this,that)
  def /(that: SageExpr): SageExpr = SDiv(this,that)
  def ^(that: SageExpr): SageExpr = SPow(this,that)

}

case class SVal(v:Double)                     extends SageExpr
case object SArg                              extends SageExpr
case class SVar(v:String)                     extends SageExpr
case class SFun(f:String,args:List[SageExpr]) extends SageExpr
case class SDiv(e1:SageExpr,e2:SageExpr)      extends SageExpr
case class SMult(e1:SageExpr,e2:SageExpr)     extends SageExpr
case class SPow(e1:SageExpr,e2:SageExpr)      extends SageExpr
case class SAdd(e1:SageExpr,e2:SageExpr)      extends SageExpr
case class SSub(e1:SageExpr,e2:SageExpr)      extends SageExpr
