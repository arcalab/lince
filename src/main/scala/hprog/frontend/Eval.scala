package hprog.frontend

import hprog.ast.SageExpr.{SExpr, SExprFun, SExprT, SExprV}
import hprog.ast._
import hprog.backend.Show
import hprog.frontend.Semantics.{Point, SageSolution, Valuation}

object Eval {

  def apply(state:Point, lin: Lin): Double = lin match {
    case Var(v) => state(v)
    case Value(v) => v
    case Add(l1, l2) => apply(state,l1) + apply(state,l2)
    case Mult(v, l)  => apply(state,v)  * apply(state,l)
  }

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

  def apply(e:SExprFun, t: Double, x: Valuation): Double = e match {
    case SVal(v) => v
    case _:SArg => t
    case SVar(v) => apply(x(v),t,x) // not really used - usually v(0) denotes this case
                                           // could create an infinte loop if recursive (not anymore with SExpr)
    case SDiv(e1, e2) => apply(e1,t,x) / apply(e2,t,x)
    case SMult(e1, e2) =>apply(e1,t,x) * apply(e2,t,x)
    case SPow(e1, e2) => math.pow(apply(e1,t,x),apply(e2,t,x))
    case SAdd(e1, e2) => apply(e1,t,x) + apply(e2,t,x)
    case SSub(e1, e2) => apply(e1,t,x) - apply(e2,t,x)
    case s:SFun[SageExpr.All] => (s.f,s.args) match {
      case (v,List(SVal(0.0))) if x contains v => apply(x(v),t,x) // could create infinite loop
      case ("exp",v::Nil) => math.exp(apply(v,t,x))
      case ("sin",v::Nil) => math.sin(apply(v,t,x))
      case ("cos",v::Nil) => math.cos(apply(v,t,x))
      case ("tan",v::Nil) => math.atan(apply(v,t,x))
      case ("arcsin",v::Nil) => math.asin(apply(v,t,x))
      case ("arccos",v::Nil) => math.acos(apply(v,t,x))
      case ("arctan",v::Nil) => math.atan(apply(v,t,x))
      case ("sqrt",v::Nil) => math.sqrt(apply(v,t,x))
      case ("log",v::Nil) => math.log(apply(v,t,x))
      case ("log10",v::Nil) => math.log10(apply(v,t,x))
      case (_,_) => throw new RuntimeException(
        s"Unknown function '${s.f}(${s.args.mkString(",")})'")

    }
  }

  // Ignore variables or time arguments
  def apply(e:SExprT, t:Double): Double = apply(e,t,Map())
  def apply(e:SExprV, x:Valuation): Double = apply(e,0,x)
  def apply(e:SExpr): Double = apply(e,0,Map())

  def apply(v: Valuation): Point = v.mapValues(apply)

  def update(e:SExprFun,t:SExpr,v:Valuation): SExpr =
    updInput(Eval.updTime(t,e),v)

  /** Update an expression by replacing initial values v(0)
    * @param e expression to be updated
    * @param sol solution with the new values
    * @return updated expression
    */
  def updInputFun(e:SExprFun, sol:Valuation): SExprT = e match {
    case SFun(v, List(SVal(0.0))) if sol contains v  => sol(v)
    case SVar(v)                                     => sol(v)
    //
    case s:SFun[SageExpr.Var] => SFun(s.f,s.args.map(e2 =>  updInputFun(e2,sol)))
    case SDiv(e1, e2)  => SDiv( updInputFun(e1,sol),updInputFun(e2,sol))
    case SMult(e1, e2) => SMult(updInputFun(e1,sol),updInputFun(e2,sol))
    case SPow(e1, e2)  => SPow( updInputFun(e1,sol),updInputFun(e2,sol))
    case SAdd(e1, e2)  => SAdd( updInputFun(e1,sol),updInputFun(e2,sol))
    case SSub(e1, e2)  => SSub( updInputFun(e1,sol),updInputFun(e2,sol))
    case SVal(v) => SVal(v)
    case t:SArg  => t
  }

  def updInput(e:SExprV,sol:Valuation): SExpr = updInputFun(e,sol) match {
    case t:SExpr => t
    case v => throw new RuntimeException(s"updating variable in ${Show(e)} does nt yield an SExpr (${Show(v)}).")
  }

  def updTime(newt: SExprV, expr: SExprFun): SExprV = updTimeFun(newt, expr) match {
    case e: SExprV => e
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
  def updTimeV(newt: SExprT, expr: SExprV): SExprV = updTimeFun(newt, expr) match {
    case e: SExprV => e
    case v => throw new RuntimeException(s"updating time in ${Show(expr)} does nt yield an SExprV (${Show(v)}).")
  }
  def updTimeT(newt: SExprT, expr: SExprT): SExprT = updTimeFun(newt, expr) match {
    case e: SExprT => e
    case v => throw new RuntimeException(s"updating time in ${Show(expr)} does nt yield an SExprT (${Show(v)}).")
  }
  def updTimeFun(newt: SExprFun, expr:SExprFun): SExprFun = expr match {
    case _:SArg => newt
    //
    case sf:SFun[SageExpr.All]  =>
      SFun(sf.f,sf.args.map((e2:SExprFun) => updTimeFun(newt,e2)))
    case SDiv(e1, e2)  => SDiv( updTimeFun(newt,e1), updTimeFun(newt,e2))
    case SMult(e1, e2) => SMult(updTimeFun(newt,e1), updTimeFun(newt,e2))
    case SPow(e1, e2)  => SPow( updTimeFun(newt,e1), updTimeFun(newt,e2))
    case SAdd(e1, e2)  => SAdd( updTimeFun(newt,e1), updTimeFun(newt,e2))
    case SSub(e1, e2)  => SSub( updTimeFun(newt,e1), updTimeFun(newt,e2))
    case e:SVal => e
    case e:SVar => e
  }

  def lin2sage(l:Lin): SExprV = l match {
    case Var(v) => SFun(v,List(SVal(0))) //SVar(v)
    case Value(v) => SVal(v)
    case Add(l1, l2) => SAdd(lin2sage(l1),lin2sage(l2))
    case Mult(v, l2) => SMult(SVal(v.v),lin2sage(l2))
  }

  def baseSage(vs:Iterable[String]): SageSolution =
    vs.map(v => v -> SFun(v,List(SVal(0.0)))).toMap


  def simplifyMan(e:SExprFun): SExprFun = e match {
    case SVal(_) => e
    case SArg() => e
    case SVar(_) => e
    case SFun(f, args) => SFun(f,args.map(simplifyMan))
    case SDiv(e1, e2) => (simplifyMan(e1),simplifyMan(e2)) match {
      case (x,SVal(1)) => x
      case (SVal(0),_) => SVal(0)
      case (SVal(x),SVal(y)) => SVal(x/y)
      case (x,y) => SDiv(x,y)
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
