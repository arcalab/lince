package hprog.ast

import hprog.frontend.Show

import scala.runtime.ScalaRunTime

sealed abstract class Progr {
  def ~(other:Progr) = (this,other) match {
    case (Seq(p1),Seq(p2)) => Seq(p1:::p2)
    case (Seq(p1),p2) => Seq(p1:::List(p2))
    case (p1,Seq(p2)) => Seq(p1::p2)
    case (p1,p2) => Seq(List(p1,p2))
  }

  override def toString: String = Show(this)
//  def show = Show(this)
}

//sealed trait Atom extends Progr
case class Seq(ps:List[Progr])                        extends Progr
case class Statement(st:List[Assgn],dur:Option[Expr]) extends Progr {
  def &(e:Expr) = Statement(this.st,Some(e))
  def &(a:Assgn) = Statement(this.st:::List(a),this.dur)
}
case class Assgn(v:Var,e:Expr) {
  def &(e2:Expr) = Statement(List(this),Some(e2))
  def &(a:Assgn) = Statement(List(this,a),None)
  override def toString: String =  Show(this)
}


/**
 * Integer expressions
 */
sealed trait Expr {
  // helpers to DSL
  def +(that:Expr) = Add(this,that)
  def -(that:Expr) = Sub(this,that)
  def *(that:Expr) = Mul(this,that)
  def /(that:Expr) = Div(this,that)
  def ===(that:Expr) = EQ(this,that)
  def >(that:Expr)   = GT(this,that)
  def <(that:Expr)   = LT(this,that)
  def >=(that:Expr)  = GE(this,that)
  def <=(that:Expr)  = LE(this,that)
  // Booleans
  def &&(that:Expr) = (this,that) match {
    case (BVal(true),_) => that
    case(_,BVal(true)) => this
    case (And(e1),And(e2)) => And(e1:::e2)
    case (And(es),_) => if (es contains that) this else And(es:::List(that)) // naive avoidance of repetitions
    case (_,And(es)) => if (es contains this) that else And(this::es)        // naive avoidance of repetitions
    case _ => And(List(this,that))
  }
  def <=>(that:Expr) =
    (this && that) || (Not(this) && Not(that))
  def ||(that:Expr) = Or(this,that)

  override def toString: String =  Show(this)
}

case class Var(name:String,der:Int) extends Expr {
  def :=(e:Expr): Assgn = Assgn(this,e)
  def ! : Var = Var(name,der+1)
//  def unary_! : Var = Var(name,der+1)
}
case class Val(v:Double) extends Expr
case class Add(e1:Expr,e2:Expr) extends Expr
case class Sub(e1:Expr,e2:Expr) extends Expr
case class Mul(e1:Expr,e2:Expr) extends Expr
case class Div(e1:Expr,e2:Expr) extends Expr
// case class Sum(x:Var,from:Expr,to:Expr,e:Expr) extends Expr
// case class ITE(b:BExpr,ifTrue:Expr,ifFalse:Expr) extends Expr


// Booleans
case class BVal(b:Boolean) extends Expr
case class And(es:List[Expr])  extends Expr // special treatment for ands, because constraints in typechecking are a big conjunction
case class Or(e1:Expr,e2:Expr) extends Expr
case class Not(e:Expr)         extends Expr
case class EQ(e1:Expr,e2:Expr) extends Expr
case class GT(e1:Expr,e2:Expr) extends Expr
case class LT(e1:Expr,e2:Expr) extends Expr
case class LE(e1:Expr,e2:Expr) extends Expr
case class GE(e1:Expr,e2:Expr) extends Expr
// case class AndN(x:Var,from:Expr,to:Expr,e:Expr) extends Expr // to is "excluding"
