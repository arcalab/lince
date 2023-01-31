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



//////////////////////////////////////////////////////////////////////////////

  /** Evaluation of a linear expression. */
  def apply(state:Point, lin: Lin): Double = lin match {
    case Var(v) => state(v)
    case Value(v) => v
    case Add(l1, l2) => apply(state,l1) + apply(state,l2)
    case Mult(v,l2)  => apply(state,v)  * apply(state,l2)
   
  }


  /** Evaluation of a non-linear expression. */
  def apply(state:Point, notlin: NotLin): Double = notlin match {
    case VarNotLin(v) => state(v)
    case ValueNotLin(v) => v
    case AddNotLin(l1, l2) => apply(state,l1) + apply(state,l2)
    case MultNotLin(l1,l2)  => apply(state,l1)  * apply(state,l2)
    case DivNotLin(l1,l2)  => apply(state,l1) / apply(state,l2)
    case ResNotLin(l1,l2)  => apply(state,l1) % apply(state,l2)
    case PowNotLin(l1,l2)  => pow(apply(state,l1),apply(state,l2))
    case FuncNotLin(s,list) => (s,list) match {
      case ("PI",Nil) => math.Pi
      case ("E",Nil) => math.E
      case ("max",v1::v2::Nil) => math.max(apply(state,v1), apply(state,v2))
      case ("min",v1::v2::Nil) => math.min(apply(state,v1), apply(state,v2))
      case ("exp",v::Nil) => math.exp(apply(state,v))
      case ("sin",v::Nil) => math.sin(apply(state,v))
      case ("cos",v::Nil) => math.cos(apply(state,v))
      case ("tan",v::Nil) => math.tan(apply(state,v))
      case ("arcsin",v::Nil) => math.asin(apply(state,v))
      case ("arccos",v::Nil) => math.acos(apply(state,v))
      case ("arctan",v::Nil) => math.atan(apply(state,v))
      case ("sinh",v::Nil) => math.sinh(apply(state,v))
      case ("cosh",v::Nil) => math.cosh(apply(state,v))
      case ("tanh",v::Nil) => math.tanh(apply(state,v))
      case ("sqrt",v::Nil) => math.sqrt(apply(state,v))
      case ("log",v::Nil) => math.log(apply(state,v))
      case ("log10",v::Nil) => math.log10(apply(state,v))
      case (_,_) => throw new RuntimeException(s"Unknown function '${s}(${list.mkString(",")})', or the number of arguments are incorrect")

    }
    
  }
  

  // New
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
      
    }


///////////////////////////////////////////////////////////////////////////////////////////

  def apply(e:SyExprAll, t: Double, x: Valuation): Double = e match {
    case SVal(v) => v
    case _:SArg => t
    case SVar("e")  if !x.contains("e")  => math.E
    case SVar("pi") if !x.contains("pi") => math.Pi
    case s:SVar if !x.contains(s.v) =>
      throw new RuntimeException(s"Evaluating $e but ${s.v} not found in [${Show(x)}].")
    case s:SVar => apply(x(s.v),t,x)
    case SDiv(e1, e2) => apply(e1,t,x) / apply(e2,t,x)
    case SRes(e1, e2) => apply(e1,t,x) % apply(e2,t,x)
    case SMult(e1, e2) =>apply(e1,t,x) * apply(e2,t,x)
    case SPow(e1, e2) => math.pow(apply(e1,t,x),apply(e2,t,x))
    case SAdd(e1, e2) => apply(e1,t,x) + apply(e2,t,x)
    case SSub(e1, e2) => apply(e1,t,x) - apply(e2,t,x)
    case s:SFun[SymbolicExpr.All] => (s.f,s.args) match {
      case (v,List(SVal(0.0))) if x contains v => apply(x(v),t,x) // could create infinite loop
      case ("PI",Nil) => math.Pi
      case ("E",Nil) => math.E
      case ("max",v1::v2::Nil) => math.max(apply(v1,t,x), apply(v2,t,x))
      case ("min",v1::v2::Nil) => math.min(apply(v1,t,x), apply(v2,t,x))
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
        s"Unknown function '${s.f}(${s.args.mkString(",")})',or the number of arguments are incorrect")

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
   


    
  /** Update an expression by replacing initial values v(0)
    * @param e expression to be updated
    * @param sol solution with the new values
    * @return updated expression
    */
  def updInputFun(e:SyExprAll, sol:Valuation): SyExprTime = e match {
    case s:SVar => sol(s.v)
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
  }

  def updTimeFun(newt: SyExprAll, expr:SyExprAll): SyExprAll = expr match {
    case _:SArg => newt
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

  

  def lin2sage(l:Lin): SyExprVar = l match {
    case Var(v) => SVar(v) //SFun(v,List(SVal(0))) //SVar(v)
    case Value(v) => SVal(v)
    case Add(l1, l2) => SAdd(lin2sage(l1),lin2sage(l2))
    case Mult(v, l2) => SMult(SVal(v.v),lin2sage(l2))


  }


 // New
  def notlin2sage(l:NotLin): SyExprVar = l match {
    case VarNotLin(v) => SVar(v) //SFun(v,List(SVal(0))) //SVar(v)
    case ValueNotLin(v) => SVal(v)
    case AddNotLin(l1, l2) => SAdd(notlin2sage(l1),notlin2sage(l2))
    case MultNotLin(l1, l2) => SMult(notlin2sage(l1),notlin2sage(l2))
    case DivNotLin(l1, l2) => SDiv(notlin2sage(l1),notlin2sage(l2))
    case PowNotLin(l1,l2) => SPow(notlin2sage(l1),notlin2sage(l2))
    case FuncNotLin(s,list)=>SFun(s,list.map((l:NotLin) => notlin2sage(l)))
    case ResNotLin(l1,l2) => SRes(notlin2sage(l1),notlin2sage(l2)) 

  }

  


  def sFunToSVar(e: SyExprVar): SyExprVar = e match {
    case _:SVal => e
    case _:SArg => e
    case _:SVar => e
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
