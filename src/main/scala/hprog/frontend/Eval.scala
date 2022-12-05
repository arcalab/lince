package hprog.frontend

import hprog.ast.SymbolicExpr.{SyExpr, SyExprAll, SyExprTime, SyExprVar}
import hprog.ast._
import Syntax._
import hprog.backend.Show
import hprog.frontend.CommonTypes.{Point, SySolution, SySolutionTime, SySolutionVar, Valuation, Solution}
import hprog.frontend.solver.Solver
import scala.math._
import scala.sys.error

object Eval {

//  private type ST = SyExprTime
//  private type SE = SyExpr
//  private type SA = SyExprAll
//  private type SV = SyExprVar


//////////////////////////////////////////////////////////////////////////////

// ALTEREI!!!!!!!!!!!!!!!!!
  def apply(state:Point, lin: Lin): Double = lin match {
    case Var(v) => state(v)
    case Value(v) => v
    case Add(l1, l2) => apply(state,l1) + apply(state,l2)
    case Mult(v,l2)  => apply(state,v)  * apply(state,l2)
    /*
    case Div(l1,l2)  => apply(state,l1) / apply(state,l2)
    case Res(l1,l2)  => apply(state,l1) % apply(state,l2)
    case Sin(l1)  => sin(apply(state,l1))
    case Cos(l1)  => cos(apply(state,l1))
    case Tan(l1)  => tan(apply(state,l1))
    case Pow(l1,l2)  => pow(apply(state,l1),apply(state,l2))
    case Sqrt(l1,l2)  => pow(apply(state,l1),(1/apply(state,l2)))
    */
  }


// ACRESCENTEI !!!!!!!!!!!!!
  def apply(state:Point, notlin: NotLin): Double = notlin match {
    case VarNotLin(v) => state(v)
    case ValueNotLin(v) => v
    case AddNotLin(l1, l2) => apply(state,l1) + apply(state,l2)
    case MultNotLin(l1,l2)  => apply(state,l1)  * apply(state,l2)
    case DivNotLin(l1,l2)  => apply(state,l1) / apply(state,l2)
    case ResNotLin(l1,l2)  => apply(state,l1) % apply(state,l2)
    case SinNotLin(l1)  => math.sin(apply(state,l1))
    case CosNotLin(l1)  => math.cos(apply(state,l1))
    case TanNotLin(l1)  => math.tan(apply(state,l1))
    case PowNotLin(l1,l2)  => pow(apply(state,l1),apply(state,l2))
    //case SqrtNotLin(l1,l2)  => pow(apply(state,l1),(1/apply(state,l2)))
  }
  

  // ALTEREI !!!!!!!!!!!!!!! (nos números, apenas o zero é falso)
  def apply(state:Point, cond: Cond): Boolean =
    cond match {
      case BVal(b) =>  b
      case And(c1, c2) => apply(state,c1) && apply(state,c2)
      case Or(c1, c2) => apply(state,c1) || apply(state,c2)
      case Not(c) => !apply(state,c)
      case EQ(v, l) => apply(state,v) == apply(state,l)
      case GT(v, l) => apply(state,v) >  apply(state,l)
      case LT(v, l) => apply(state,v) <  apply(state,l)
      case GE(v, l) => apply(state,v) >= apply(state,l)
      case LE(v, l) => apply(state,v) <= apply(state,l)
      //case IsoletedCond(l) => if (apply(state,l) != 0) true else false
    }


///////////////////////////////////////////////////////////////////////////////////////////

  def apply(e:SyExprAll, t: Double, x: Valuation): Double = e match {
    case SVal(v) => v
    case _:SArg => t
    case s:SVar if !x.contains(s.v) =>
      throw new RuntimeException(s"Evaluating $e but ${s.v} not found in ${Show(x)}.")
    case s:SVar => apply(x(s.v),t,x)
//    case SVar(v) => apply(x(v),t,x) // not really used - usually v(0) denotes this case
                                           // could create an infinte loop if recursive (not anymore with SExpr)
    case SDiv(e1, e2) => apply(e1,t,x) / apply(e2,t,x)
    case SRes(e1, e2) => apply(e1,t,x) % apply(e2,t,x)
    case SMult(e1, e2) =>apply(e1,t,x) * apply(e2,t,x)
    case SPow(e1, e2) => math.pow(apply(e1,t,x),apply(e2,t,x))
    case SAdd(e1, e2) => apply(e1,t,x) + apply(e2,t,x)
    case SSub(e1, e2) => apply(e1,t,x) - apply(e2,t,x)
    case s:SFun[SymbolicExpr.All] => (s.f,s.args) match {
      case (v,List(SVal(0.0))) if x contains v => apply(x(v),t,x) // could create infinite loop
      case ("max",v1::v2::Nil) => math.max(apply(v1,t,x), apply(v2,t,x))
      case ("exp",v::Nil) => math.exp(apply(v,t,x))
      case ("sin",v::Nil) => math.sin(apply(v,t,x))
      case ("cos",v::Nil) => math.cos(apply(v,t,x))
      case ("tan",v::Nil) => math.tan(apply(v,t,x))
      case ("arcsin",v::Nil) => math.asin(apply(v,t,x))
      case ("arccos",v::Nil) => math.acos(apply(v,t,x))
      case ("arctan",v::Nil) => math.atan(apply(v,t,x))
      case ("sinh",v::Nil) => math.sinh(apply(v,t,x))
      case ("cosh",v::Nil) => math.cosh(apply(v,t,x))
      case ("tanh",v::Nil) => math.tanh(apply(v,t,x))
      case ("sqrt",v::Nil) => math.sqrt(apply(v,t,x))
      case ("log",v::Nil) => math.log(apply(v,t,x))
      case ("log10",v::Nil) => math.log10(apply(v,t,x))
      case (_,_) => throw new RuntimeException(
        s"Unknown function '${s.f}(${s.args.mkString(",")})'")

    }
  }

  // Ignore variables or time arguments
  def apply(e:SyExprTime, t:Double): Double = apply(e,t,Map())
  def apply(e:SyExprVar, x:Valuation): Double = apply(e,0,x)
  def apply(e:SyExpr): Double = apply(e,0,Map())

  def apply(v: Valuation): Point = v.view.mapValues(apply).toMap

  def update(e:SyExprAll, t:SyExpr, v:Valuation): SyExpr =
    updInput(Eval.updTime(t,e),v)

  def update(phi:SySolution, t:SyExpr, v:Valuation): Valuation =
    updInput(v,Eval.updTime(t,phi))


  // variation for numerically computed solutions
  def updateNum(phi: Solution, t: SyExpr, v: Valuation): Valuation =
    phi.view.mapValues(updater => SVal(updater(apply(t))(apply(v)))).toMap
    //updInput(v, Eval.updTime(t, phi))


    
  /** Update an expression by replacing initial values v(0)
    * @param e expression to be updated
    * @param sol solution with the new values
    * @return updated expression
    */
  def updInputFun(e:SyExprAll, sol:Valuation): SyExprTime = e match {
//    case SFun(v, List(SVal(0.0))) if sol contains v  => sol(v)
    case s:SVar                                      => sol(s.v)
    //
    case s:SFun[SymbolicExpr.All] =>
      SFun[SymbolicExpr.Time](s.f,s.args.map(e2 =>  updInputFun(e2,sol)))
    case SDiv(e1, e2)  => SDiv( updInputFun(e1,sol),updInputFun(e2,sol))
    case SRes(e1, e2)  => SRes( updInputFun(e1,sol),updInputFun(e2,sol))
    case SMult(e1, e2) => SMult(updInputFun(e1,sol),updInputFun(e2,sol))
    case SPow(e1, e2)  => SPow( updInputFun(e1,sol),updInputFun(e2,sol))
    case SAdd(e1, e2)  => SAdd( updInputFun(e1,sol),updInputFun(e2,sol))
    case SSub(e1, e2)  => SSub( updInputFun(e1,sol),updInputFun(e2,sol))
    case SVal(v) => SVal(v)
    case t:SArg  => t
  }

  def updInput(e:SyExprVar, sol:Valuation): SyExpr = updInputFun(e,sol) match {
    case t:SyExpr @unchecked => t // guaranteed to succeed (but type eliminated by erasure)
    case v => throw new RuntimeException(s"updating variable in ${Show(e)} does nt yield an SExpr (${Show(v)}).")
  }

  def updTime(t:SyExpr, phi:SySolution): SySolutionVar =
    phi.view.mapValues(updTime(t,_)).toMap
  def updInput(input:Valuation,phi:SySolutionVar): Valuation =
    phi.view.mapValues(updInput(_,input)).toMap
  def updInputFun(input:Valuation,phi:SySolution): SySolutionTime =
    phi.view.mapValues(updInputFun(_,input)).toMap
  def solveValues(s:Solver,phi:Valuation): Valuation =
    phi.view.mapValues(s.solveSymbExpr).toMap

  def updTime(newt: SyExprVar, expr: SyExprAll): SyExprVar = updTimeFun(newt, expr) match {
    case e: SyExprVar @unchecked => e  // guaranteed to succeed (but type eliminated by erasure)
    case v => throw new RuntimeException(s"updating time in ${Show(expr)} does nt yield an SExprV (${Show(v)}).")
//    case _:SArg => newt
//    //
//    case sf:SFun[SageExpr.All]  => SFun(sf.f,sf.args.map((e2:SExprFun) => updTime(newt,e2)))
//    case SDiv(e1, e2)  => SDiv( updTime(newt,e1), updTime(newt,e2))
//    case SMult(e1, e2) => SMult(updTime(newt,e1), updTime(newt,e2))
//    case SPow(e1, e2)  => SPow( updTime(newt,e1), updTime(newt,e2))
//    case SAdd(e1, e2)  => SAdd( updTime(newt,e1), updTime(newt,e2))
//    case SSub(e1, e2)  => SSub( updTime(newt,e1), updTime(newt,e2))
//    case e:SVal => e
//    case e:SVar => e
  }
//  def updTimeV(newt: SyExprTime, expr: SyExprVar): SyExprVar = updTimeFun(newt, expr) match {
//    case e: SyExprVar => e
//    case v => throw new RuntimeException(s"updating time in ${Show(expr)} does nt yield an SExprV (${Show(v)}).")
//  }
//  def updTimeT(newt: SyExprTime, expr: SyExprTime): SyExprTime = updTimeFun(newt, expr) match {
//    case e: SyExprTime => e
//    case v => throw new RuntimeException(s"updating time in ${Show(expr)} does not yield an SExprT (${Show(v)}).")
//  }
  def updTimeFun(newt: SyExprAll, expr:SyExprAll): SyExprAll = expr match {
    case _:SArg => newt
    //
    case sf:SFun[SymbolicExpr.All]  =>
      SFun(sf.f,sf.args.map((e2:SyExprAll) => updTimeFun(newt,e2)))
    case SDiv(e1, e2)  => SDiv( updTimeFun(newt,e1), updTimeFun(newt,e2))
    case SRes(e1, e2)  => SRes( updTimeFun(newt,e1), updTimeFun(newt,e2))
    case SMult(e1, e2) => SMult(updTimeFun(newt,e1), updTimeFun(newt,e2))
    case SPow(e1, e2)  => SPow( updTimeFun(newt,e1), updTimeFun(newt,e2))
    case SAdd(e1, e2)  => SAdd( updTimeFun(newt,e1), updTimeFun(newt,e2))
    case SSub(e1, e2)  => SSub( updTimeFun(newt,e1), updTimeFun(newt,e2))
    case e:SVal => e
    case e:SVar => e
  }

  // replace variables by their expression in programs
//  def updInput(syntax:Syntax,x:Valuation): Syntax = syntax match {
//    case Atomic(as, de) => Atomic(as.map(a=>Assign(a.v,updInput(a.e,x))), updInput(de,x))
//    case Seq(p, q) => Seq(updInput(p,x),updInput(q,x))
//    case ITE(ifP, thenP, elseP) => ITE(updInput(ifP,x),updInput(thenP,x),updInput(elseP,x))
//    case While(pre, d, doP) => While(updInput(pre,x),updInput(d,x),updInput(doP,x))
//  }

  // replace variables by their expression in boolean conditions
//  def updInput(cond: Cond, x: Valuation) : Cond = cond match {
//    case BVal(b) => cond
//    case And(c1, c2) => And(updInput(c1,x),updInput(c2,x))
//    case Or(c1, c2) => Or(updInput(c1,x),updInput(c2,x))
//    case Not(c) => Not(updInput(c,x))
//    case EQ(l1, l2) => EQ(updInput(l1,x),updInput(l2,x))
//    case GT(l1, l2) => GT(updInput(l1,x),updInput(l2,x))
//    case LT(l1, l2) => LT(updInput(l1,x),updInput(l2,x))
//    case GE(l1, l2) => GE(updInput(l1,x),updInput(l2,x))
//    case LE(l1, l2) => LE(updInput(l1,x),updInput(l2,x))
//  }

//  def updInput(lin: Lin, x: Valuation): Lin = lin match {
//    case Var(v) => x.get(v) match {
//      case Some(e) => syExpToLin(e)
//      case None => lin
//    }
//    case Value(_) => lin
//    case Add(l1, l2) => Add(updInput(l1,x),updInput(l2,x))
//    case Mult(v, l) => Mult(v,updInput(l,x))
//  }

//  def syExpToLin(expr: SyExpr): Lin = expr match {
//    case SVal(v) => Var(v)
//  }



// Não soube fazer para os que têm ???
  def lin2sage(l:Lin): SyExprVar = l match {
    case Var(v) => SVar(v) //SFun(v,List(SVal(0))) //SVar(v)
    case Value(v) => SVal(v)
    case Add(l1, l2) => SAdd(lin2sage(l1),lin2sage(l2))
    case Mult(v, l2) => SMult(SVal(v.v),lin2sage(l2))
    /*
    case Div(l1, l2) => SDiv(lin2sage(l1),lin2sage(l2))
    case Pow(l1,l2) => SPow(lin2sage(l1),lin2sage(l2))
    //case Sin(l1) =>???
    //case Cos(l1) =>???
    //case Tan(l1) =>???
    //case Sqrt(l1,l2) =>???
    */

  }


 // Estou na dúvida para os que têm ???, mas penso que seja o que está comentando
  def notlin2sage(l:NotLin): SyExprVar = l match {
    case VarNotLin(v) => SVar(v) //SFun(v,List(SVal(0))) //SVar(v)
    case ValueNotLin(v) => SVal(v)
    case AddNotLin(l1, l2) => SAdd(notlin2sage(l1),notlin2sage(l2))
    case MultNotLin(l1, l2) => SMult(notlin2sage(l1),notlin2sage(l2))
    case DivNotLin(l1, l2) => SDiv(notlin2sage(l1),notlin2sage(l2))
    case PowNotLin(l1,l2) => SPow(notlin2sage(l1),notlin2sage(l2))
    case SinNotLin(l1) =>  SFun("sin",List(notlin2sage(l1)))
    case CosNotLin(l1) =>  SFun("cos",List(notlin2sage(l1)))
    case TanNotLin(l1) => SFun("tan",List(notlin2sage(l1)))
    //case SqrtNotLin(l1,l2) => SFun("sqrt",List(notlin2sage(l1)))
    case ResNotLin(l1,l2) => SRes(notlin2sage(l1),notlin2sage(l2)) // SFun("Res",List(notlin2sage(l1),notlin2sage(l2)))

  }

  //def syExprToLin(e:SyExpr): Lin =


  def sFunToSVar(e: SyExprVar): SyExprVar = e match {
    case _:SVal => e
    case _:SArg => e
    case _:SVar => e
//    case sf:SFun[_] => SVar(sf.f): SymbolicExpr[SymbolicExpr.Var]
    case SFun(f, List(SVal(0))) => SVar(f)
    case SFun(f,a) =>
      error(s"Trying to calculate the value of an expression with a function $f(${a.map(Show(_)).mkString(",")}")
    case SDiv(e1, e2) => SDiv(sFunToSVar(e1),sFunToSVar(e2))
    case SRes(e1, e2) => SRes(sFunToSVar(e1),sFunToSVar(e2))
    case SMult(e1, e2) => SMult(sFunToSVar(e1),sFunToSVar(e2))
    case SPow(e1, e2) => SPow(sFunToSVar(e1),sFunToSVar(e2))
    case SAdd(e1, e2) => SAdd(sFunToSVar(e1),sFunToSVar(e2))
    case SSub(e1, e2) => SSub(sFunToSVar(e1),sFunToSVar(e2))
  }

  def baseSage(vs:Iterable[String]): SySolution =
    vs.map(v => v -> SFun(v,List(SVal(0.0)))).toMap


  def simplifyMan(e:SyExprAll): SyExprAll = e match {
    case _:SVal => e
    case _:SArg => e
    case _:SVar => e
    case SFun(f, args) => SFun(f,args.map(simplifyMan))
    case SDiv(e1, e2) => (simplifyMan(e1),simplifyMan(e2)) match {
      case (x,SVal(1)) => x
      case (SVal(0),_) => SVal(0)
      case (SVal(x),SVal(y)) => SVal(x/y)
      case (x,y) => SDiv(x,y)
    }
    case SRes(e1, e2) => (simplifyMan(e1),simplifyMan(e2)) match {
      case (x,SVal(1)) => SVal(0)
      case (SVal(1),_) => SVal(1)
      case (SVal(0),_) => SVal(0)
      case (SVal(x),SVal(y)) => SVal(x%y)
      case (x,y) => SRes(x,y)
    }
    case SMult(e1, e2) => (simplifyMan(e1),simplifyMan(e2)) match {
      case (x,SVal(1)) => x
      case (SVal(1),x) => x
      case (SVal(0),_) => SVal(0)
      case (_,SVal(0)) => SVal(0)
      case (SVal(x),SVal(y)) => SVal(x*y)
      case (x,y) => SMult(x,y)
    }
    case SPow(e1, e2) => (simplifyMan(e1),simplifyMan(e2)) match {
      case (x,SVal(1)) => x
      case (_,SVal(0)) => SVal(1)
      case (SVal(0),_) => SVal(0)
      case (SVal(1),_) => SVal(1)
      case (SVal(x),SVal(y)) => SVal(math.pow(x,y))
      case (x,y) => SPow(x,y)
    }
    case SAdd(e1, e2) => (simplifyMan(e1),simplifyMan(e2)) match {
      case (x,SVal(0)) => x
      case (SVal(0),x) => x
      case (SVal(x),SVal(y)) => SVal(x+y)
      case (SAdd(e11,e12),e21) => simplifyMan(SAdd(e11,SAdd(e12,e21)))
      case (x,y) => SAdd(x,y)
    }
    case SSub(e1, e2) => (simplifyMan(e1),simplifyMan(e2)) match {
      case (x,SVal(0)) => x
      case (SVal(x),SVal(y)) => SVal(x-y)
      case (SSub(e11,e12),e21) => simplifyMan(SSub(e11,SAdd(e12,e21)))
      case (x,y) => SSub(x,y)
    }
  }

}
