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
  case class Assign(v:Var,e:NotLin) 

  case class DiffEqs(eqs:List[DiffEq],dur:Dur) {
    def &(dur:Dur): DiffEqs = DiffEqs(eqs,dur) // override dur
    def &(diffEq: DiffEq): DiffEqs = DiffEqs(eqs++List(diffEq),dur) // add eq
    def &(diffEqs: DiffEqs): DiffEqs = DiffEqs(eqs++diffEqs.eqs,diffEqs.dur) // add eqs and override dur
  }

  // DiffEq
  case class DiffEq(v:Var,e:NotLin)

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
    def +(other:NotLin): NotLin = Add(this,other)
  }
  case class Var(v:String)       extends NotLin {
    def ^=(l: NotLin): DiffEq = DiffEq(this,l) //New
    def :=(l: NotLin): Assign = Assign(this,l)
    def >(l: NotLin):  Cond = GT(this,l)
    def <(l: NotLin):  Cond = LT(this,l)
    def >=(l: NotLin): Cond = GE(this,l)
    def <=(l: NotLin): Cond = LE(this,l)
    def ===(l: NotLin):Cond = EQ(this,l)
  }
  case class Value(v:Double)     extends NotLin { 
    def *(l: NotLin): NotLin = Mult(this,l) 
  }
  case class Add(l1:NotLin,l2:NotLin)  extends NotLin 

  case class Mult(l1:NotLin,l2:NotLin) extends NotLin 
  
  case class Div(l1:NotLin,l2:NotLin) extends NotLin 

  case class Res(l1:NotLin,l2:NotLin) extends NotLin

  //case class Pow(l1:NotLin,l2:NotLin) extends NotLin  

  case class Func(s:String, arg:List[NotLin]) extends NotLin  
  
/**
sealed abstract class Lin {
  def +(other: Lin):Lin = Add(this,other)
}
case class Var(v: String)    extends Lin {
  def :=(l: Lin): Assign = Assign(this,l)
  def ^=(l: Lin): DiffEq = DiffEq(this,l)
  def >(l: Lin): Cond = GT(this,l)
  def <(l: Lin): Cond = LT(this,l)
  def >=(l: Lin): Cond = GE(this,l)
  def <=(l: Lin): Cond = LE(this,l)
  def ===(l: Lin): Cond = EQ(this,l)
}
case class Value(v: Double)   extends Lin {
  def *(l: Lin): Lin = Mult(this,l)
}
case class Add(l1: Lin,l2: Lin)    extends Lin

case class Mult(v: Value,l: Lin)    extends Lin
*/
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
  
  object GetSyntax {
    private var parsedSyntax: Syntax = null
    private var initialValues: Map[String, List[NotLin]] = Map()

    def getInitialValues(values: Map[String, List[NotLin]]): Unit = {
      initialValues = values
    }

    def addParsedSyntax(syntax: Syntax): Unit = {
      parsedSyntax = syntax
    }

    def allSyntax: List[Syntax] = {
      val sintaxes = getAllSyntax(initialValues)      
      sintaxes
    }

    def getAllSyntax(newVarValues: Map[String, List[NotLin]]): List[Syntax] = {
    if (newVarValues.isEmpty) {
      List(parsedSyntax)     
    } else {
      val balancedVarValues = balanceVarValues(newVarValues)
      
      val numCombinations = balancedVarValues.head._2.length  
      
      val combinations = (0 until numCombinations).map { i =>
        balancedVarValues.map { case (key, values) =>
          key -> values(i)
        }.toMap
      }.toList    
      
      val syntaxes = combinations.map(changeAssignValues(parsedSyntax, _))
      
      syntaxes
    }
  }

    /*def changeAssignValues(syntax: Syntax, newVarValues: Map[String, NotLin]): Syntax = {
      syntax match {
        case Atomic(assigns, diffs) =>
          val newAssigns = assigns.map {
            case Assign(Var(v), l) =>
              newVarValues.get(v) match {
                case Some(newValue) => 
                  Assign(Var(v), newValue)
                case None =>
                  Assign(Var(v), l)
              }
            case other => other
          }
          Atomic(newAssigns, diffs)

        case Seq(p, q) =>
          Seq(changeAssignValues(p, newVarValues), changeAssignValues(q, newVarValues))

        case ITE(cond, thenP, elseP) =>
          ITE(cond, changeAssignValues(thenP, newVarValues), changeAssignValues(elseP, newVarValues))

        case While(pre, guard, doP) =>
          While(changeAssignValues(pre, newVarValues), guard, changeAssignValues(doP, newVarValues))

        case _ => syntax
      }
    }*/
    def changeAssignValues(syntax: Syntax, newVarValues: Map[String, NotLin]): Syntax = {
      def processAssigns(assigns: List[Assign], remainingVarValues: Map[String, NotLin]): (List[Assign], Map[String, NotLin]) = {
        assigns.foldLeft((List.empty[Assign], remainingVarValues)) {
          case ((newAssigns, remainingValues), Assign(Var(v), l)) =>
            remainingValues.get(v) match {
              case Some(newValue) =>
                (newAssigns :+ Assign(Var(v), newValue), remainingValues - v)
              case None =>
                (newAssigns :+ Assign(Var(v), l), remainingValues)
            }
          case ((newAssigns, remainingValues), other) =>
            (newAssigns :+ other, remainingValues)
        }
      }

      def change(syntax: Syntax, remainingVarValues: Map[String, NotLin]): (Syntax, Map[String, NotLin]) = {
        syntax match {
          case Atomic(assigns, diffs) =>
            val (newAssigns, updatedValues) = processAssigns(assigns, remainingVarValues)
            (Atomic(newAssigns, diffs), updatedValues)

          case Seq(p, q) =>
            val (newP, updatedValues) = change(p, remainingVarValues)
            val (newQ, finalValues) = change(q, updatedValues)
            (Seq(newP, newQ), finalValues)

          case ITE(cond, thenP, elseP) =>
            val (newThenP, updatedValues1) = change(thenP, remainingVarValues)
            val (newElseP, updatedValues2) = change(elseP, updatedValues1)
            (ITE(cond, newThenP, newElseP), updatedValues2)

          case While(pre, guard, doP) =>
            val (newPre, updatedValues1) = change(pre, remainingVarValues)
            val (newDoP, updatedValues2) = change(doP, updatedValues1)
            (While(newPre, guard, newDoP), updatedValues2)

          case _ => (syntax, remainingVarValues)
        }
      }

      change(syntax, newVarValues)._1
    }




    def balanceVarValues(newVarValues: Map[String, List[NotLin]]): Map[String, List[NotLin]] = {
      val maxLength = newVarValues.values.map(_.length).max
      newVarValues.map { case (key, values) =>
        val balancedValues = if (values.length < maxLength) {
          values ++ List.fill(maxLength - values.length)(values.head)
        } else {
          values
        }
        key -> balancedValues
      }
    }
  }  
}
