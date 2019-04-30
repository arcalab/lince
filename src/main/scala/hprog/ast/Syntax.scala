package hprog.ast

/*
New version - correct one:
 p := at | p;p | Skip | if b then p else p | While b p | ... Wait r
 at := assgn | diffEq
 assgn := X := lin
 diffEq := (X' = lin)* ("" | "&" lin | "&" b)
 lin := X | R | lin+lin | R lin

 b?  := true | false | &|! | X {><=} lin

 ---
 examples
 x:=0 ;
 x'=y+2x, y'=3 & 1 ;
 x'=2 & 2


 p'=v, v'=g & p<=0 /\ v<=0 ;
 v:= -0.5*v

 */

sealed abstract class Syntax {
  def ~(other:Syntax) = (this,other) match {
    case (Seq(p1), Seq(p2)) => Seq(p1 ::: p2)
    case (Seq(p1), p2) => Seq(p1 ::: List(p2))
    case (p1, Seq(p2)) => Seq(p1 :: p2)
    case (p1, p2) => Seq(List(p1, p2))
  }
}
// programs
sealed abstract class At                                        extends Syntax
case class            Seq(ps:List[Syntax])                      extends Syntax
case object           Skip                                      extends Syntax
case class            ITE(ifP:Cond, thenP:Syntax, elseP:Syntax) extends Syntax
case class            While(d:LoopGuard,doP:Syntax)             extends Syntax
//case class            SPosition(s1:Syntax,s2:Syntax)            extends Syntax

// atoms
case class Assign(v:Var,e:Lin)               extends At
case class DiffEqs(eqs:List[DiffEq],dur:Dur) extends At {
  def &(dur:Dur) = DiffEqs(eqs,dur) // override dur
  def &(diffEq: DiffEq)   = DiffEqs(eqs++List(diffEq),dur) // add eq
  def &(diffEqs: DiffEqs) = DiffEqs(eqs++diffEqs.eqs,diffEqs.dur) // add eqs and override dur
}

// DiffEq
case class DiffEq(v:Var,e:Lin)

// duration
sealed abstract class Dur
case class  For(t:Value)  extends Dur
case class  Until(c:Cond) extends Dur
case object Forever       extends Dur

// loopguard
sealed abstract  class LoopGuard
case class Counter(i:Int) extends LoopGuard
case class Guard(c:Cond)  extends LoopGuard

// linear expression
sealed abstract class Lin {
  def +(other:Lin) = Add(this,other)
}
case class Var(v:String)       extends Lin {
  def :=(l: Lin): Assign = Assign(this,l)
  def ^=(l: Lin): DiffEq = DiffEq(this,l)
  def >(l: Lin):  Cond = GT(this,l)
  def <(l: Lin):  Cond = LT(this,l)
  def >=(l: Lin): Cond = GE(this,l)
  def <=(l: Lin): Cond = LE(this,l)
  def ===(l: Lin):Cond = EQ(this,l)
}
case class Value(v:Double)     extends Lin {
  def *(l: Lin): Lin = Mult(this,l)
}
case class Add(l1:Lin,l2:Lin)  extends Lin
case class Mult(v:Value,l:Lin) extends Lin

// Conditions
sealed abstract class Cond {
  def &&(that:Cond): Cond  = And(this,that)
  def ||(that:Cond): Cond  = Or(this,that)
  def <=>(that:Cond): Cond =
    (this && that) || (Not(this) && Not(that))
}
case class BVal(b:Boolean)      extends Cond
case class And(c1:Cond,c2:Cond) extends Cond
case class Or(c1:Cond,c2:Cond)  extends Cond
case class Not(c:Cond)          extends Cond
case class EQ(v:Var,l:Lin)      extends Cond
case class GT(v:Var,l:Lin)      extends Cond
case class LT(v:Var,l:Lin)      extends Cond
case class GE(v:Var,l:Lin)      extends Cond
case class LE(v:Var,l:Lin)      extends Cond



// boolean expression
//sealed abstract class BExpr extends Cond
//case class Not(c:Cond)      extends Cond


/*
Ideas for later - obtaining a trajectory:
 - for each basic program (not a sequence)
 - if a new variable is found, assume it was initially 0
 - assignments update the variable
 - keep map of known variables (unknown are 0)
 - mark undefined variables (assumed to be 0)
 - calculate duration of next action
    + could be 0 for assignments
    + could be +inf for "forever"
    + could be a time value
    + could be a condition -- need to solve system?
 - compute function until next action...
 - iterate
 */


//sealed abstract class Progr {
//  def ~(other:Progr) = (this,other) match {
//    case (Seq(p1),Seq(p2)) => Seq(p1:::p2)
//    case (Seq(p1),p2) => Seq(p1:::List(p2))
//    case (p1,Seq(p2)) => Seq(p1::p2)
//    case (p1,p2) => Seq(List(p1,p2))
//  }
//
//  override def toString: String = Show(this)
////  def show = Show(this)
//}
//
////sealed trait Atom extends Progr
//case class Seq(ps:List[Progr])                        extends Progr
//case class Statement(st:List[Assgn],dur:Option[Expr]) extends Progr {
//  def &(e:Expr) = Statement(this.st,Some(e))
//  def &(a:Assgn) = Statement(this.st:::List(a),this.dur)
//}
//case class Assgn(v:Var,e:Expr) {
//  def &(e2:Expr) = Statement(List(this),Some(e2))
//  def &(a:Assgn) = Statement(List(this,a),None)
//  override def toString: String =  Show(this)
//}
//
//
///**
// * Integer expressions
// */
//sealed trait Expr {
//  // helpers to DSL
//  def +(that:Expr) = Add(this,that)
//  def -(that:Expr) = Sub(this,that)
//  def *(that:Expr) = Mul(this,that)
//  def /(that:Expr) = Div(this,that)
//  def ===(that:Expr) = EQ(this,that)
//  def >(that:Expr)   = GT(this,that)
//  def <(that:Expr)   = LT(this,that)
//  def >=(that:Expr)  = GE(this,that)
//  def <=(that:Expr)  = LE(this,that)
//  // Booleans
//  def &&(that:Expr) = (this,that) match {
//    case (BVal(true),_) => that
//    case(_,BVal(true)) => this
//    case (And(e1),And(e2)) => And(e1:::e2)
//    case (And(es),_) => if (es contains that) this else And(es:::List(that)) // naive avoidance of repetitions
//    case (_,And(es)) => if (es contains this) that else And(this::es)        // naive avoidance of repetitions
//    case _ => And(List(this,that))
//  }
//  def <=>(that:Expr) =
//    (this && that) || (Not(this) && Not(that))
//  def ||(that:Expr) = Or(this,that)
//
//  override def toString: String =  Show(this)
//}
//
//case class Var(name:String,der:Int) extends Expr {
//  def :=(e:Expr): Assgn = Assgn(this,e)
//  def ! : Var = Var(name,der+1)
////  def unary_! : Var = Var(name,der+1)
//}
//case class Val(v:Double) extends Expr
//case class Add(e1:Expr,e2:Expr) extends Expr
//case class Sub(e1:Expr,e2:Expr) extends Expr
//case class Mul(e1:Expr,e2:Expr) extends Expr
//case class Div(e1:Expr,e2:Expr) extends Expr
//// case class Sum(x:Var,from:Expr,to:Expr,e:Expr) extends Expr
//// case class ITE(b:BExpr,ifTrue:Expr,ifFalse:Expr) extends Expr
//
//
//// Booleans
//case class BVal(b:Boolean) extends Expr
//case class And(es:List[Expr])  extends Expr // special treatment for ands, because constraints in typechecking are a big conjunction
//case class Or(e1:Expr,e2:Expr) extends Expr
//case class Not(e:Expr)         extends Expr
//case class EQ(e1:Expr,e2:Expr) extends Expr
//case class GT(e1:Expr,e2:Expr) extends Expr
//case class LT(e1:Expr,e2:Expr) extends Expr
//case class LE(e1:Expr,e2:Expr) extends Expr
//case class GE(e1:Expr,e2:Expr) extends Expr
//// case class AndN(x:Var,from:Expr,to:Expr,e:Expr) extends Expr // to is "excluding"


