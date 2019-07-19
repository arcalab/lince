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

  def updTime(newt: SExpr, expr: SExprFun): SExprV = expr match {
    case _:SArg => newt
    //
    case sf:SFun[SageExpr.All]  => SFun(sf.f,sf.args.map((e2:SExprFun) => updTime(newt,e2)))
    case SDiv(e1, e2)  => SDiv( updTime(newt,e1), updTime(newt,e2))
    case SMult(e1, e2) => SMult(updTime(newt,e1), updTime(newt,e2))
    case SPow(e1, e2)  => SPow( updTime(newt,e1), updTime(newt,e2))
    case SAdd(e1, e2)  => SAdd( updTime(newt,e1), updTime(newt,e2))
    case SSub(e1, e2)  => SSub( updTime(newt,e1), updTime(newt,e2))
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


}
