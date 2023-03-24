package hprog.ast
import Syntax._
/**
A element `p` of the class `Syntax` is our internal representation of a Lince program `p`.

Overview of the grammar:
```
 p := at | at;q
 q:= at | q;q | Skip | if b then q else q | While b do {q} | ... Wait r
 at := assgn | diffEq
 assgn := X := NotLin
 diffEq := (X' = lin) | (X' = lin), For X | (X' = lin), Until args b
 lin := X | R | lin+lin | R*lin | lin*R | X/R

 b?  := true | false | && | ! | || | NotLin {><=} NotLin
```
  */
object Syntax {
  
  sealed abstract class Syntax {
    /** Sequential composition of programs with some pre-processing */
    def ~(other:Syntax): Syntax = other match {
      //case While(pre,d,doP) => While(this~pre,d,doP)
      case _ => Seq(this,other)
    }
  }


  /** An atomic program is a list of assignments and a system of differential equations with a bound */
  case class Atomic(as:List[Assign],de:DiffEqs)        extends Syntax {
    /** Sequential composition of programs with some pre-processing */
    override def ~(p:Syntax): Syntax = (de.dur,p) match {
      case (_,Seq(p,q)) => Seq(this~p,q)
      case (_,While(pre, d, doP)) => While(this~pre,d,doP)
      case (_,_) => Seq(this,p)
    }
  }

  /** Sequence of programs is a program*/
  case class Seq(p:Syntax,q:Syntax)                    extends Syntax
  /**  "If-then-else" is a program*/
  case class ITE(ifP:Cond, thenP:Syntax, elseP:Syntax) extends Syntax
  /** "While" is a program */
  case class While(pre:Syntax,d:LoopGuard,doP:Syntax)  extends Syntax

  
  /** An assignment is a member of the Atomic programs, between a variable and a non linear expression */
  case class Assign(v:VarNotLin,e:NotLin) 

  case class DiffEqs(eqs:List[DiffEq],dur:Dur) {
    def &(dur:Dur): DiffEqs = DiffEqs(eqs,dur) // override dur
    def &(diffEq: DiffEq): DiffEqs = DiffEqs(eqs++List(diffEq),dur) // add eq
    def &(diffEqs: DiffEqs): DiffEqs = DiffEqs(eqs++diffEqs.eqs,diffEqs.dur) // add eqs and override dur
  }

  // DiffEq
  case class DiffEq(v:VarNotLin,e:NotLin)

  // duration
  sealed abstract class Dur
  case class  For(e:NotLin)  extends Dur
  case class  Until(c:Cond, eps:Option[Double], jump:Option[Double]) extends Dur
  case object Forever       extends Dur

  // loopguard
  sealed abstract  class LoopGuard
  case class Counter(i:Int) extends LoopGuard
  case class Guard(c:Cond)  extends LoopGuard

  // non linear expression
  sealed abstract class NotLin {
    def +(other:NotLin): NotLin = AddNotLin(this,other)
  }
  case class VarNotLin(v:String)       extends NotLin {
    def ^=(l: NotLin): DiffEq = DiffEq(this,l) //New
    def :=(l: NotLin): Assign = Assign(this,l)
    def >(l: NotLin):  Cond = GT(this,l)
    def <(l: NotLin):  Cond = LT(this,l)
    def >=(l: NotLin): Cond = GE(this,l)
    def <=(l: NotLin): Cond = LE(this,l)
    def ===(l: NotLin):Cond = EQ(this,l)
  }
  case class ValueNotLin(v:Double)     extends NotLin { 
    def *(l: NotLin): NotLin = MultNotLin(this,l) 
  }
  case class AddNotLin(l1:NotLin,l2:NotLin)  extends NotLin 

  case class MultNotLin(l1:NotLin,l2:NotLin) extends NotLin 
  
  case class DivNotLin(l1:NotLin,l2:NotLin) extends NotLin 

  case class ResNotLin(l1:NotLin,l2:NotLin) extends NotLin 

  case class FuncNotLin(s:String, arg:List[NotLin]) extends NotLin

  case class PowNotLin(l1:NotLin,l2:NotLin) extends NotLin 


/*
  // linear expression
  sealed abstract class Lin {
    def +(other:Lin): Lin = Add(this,other)
  }
  case class Var(v:String)       extends Lin {
    def ^=(l: Lin): DiffEq = DiffEq(this,l)
  }
  case class Value(v:Double)     extends Lin {
    def *(l: Lin): Lin = Mult(this,l)
  }
  case class Add(l1:Lin,l2:Lin)  extends Lin

  case class Mult(l1:Lin,l2:Lin) extends Lin

*/

  // Conditions
  sealed abstract class Cond {
    def &&(that:Cond): Cond  = (this,that) match {
      case (BVal(true),_) => that
      case (_,BVal(true)) => this
      case (BVal(false),_) => BVal(false)
      case (_,BVal(false)) => BVal(false)
      case _ => if (this==that) this else And(this,that)
    }
    def ||(that:Cond): Cond  = (this,that) match {
      case (BVal(true),_) => BVal(true)
      case (_,BVal(true)) => BVal(true)
      case (BVal(false),_) => that
      case (_,BVal(false)) => this
      case _ => if (this==that) this else Or(this,that)
    }
    def <=>(that:Cond): Cond =
      (this && that) || (Not(this) && Not(that))
    def -->(that:Cond): Cond = that || Not(this)
  }
  case class BVal(b:Boolean)      extends Cond
  case class And(c1:Cond,c2:Cond) extends Cond
  case class Or(c1:Cond,c2:Cond)  extends Cond
  case class Not(c:Cond)          extends Cond
  case class EQ(l1:NotLin,l2:NotLin)    extends Cond
  case class GT(l1:NotLin,l2:NotLin)    extends Cond
  case class LT(l1:NotLin,l2:NotLin)    extends Cond
  case class GE(l1:NotLin,l2:NotLin)    extends Cond
  case class LE(l1:NotLin,l2:NotLin)    extends Cond
  



}
