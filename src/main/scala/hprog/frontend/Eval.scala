package hprog.frontend

import hprog.ast._
import hprog.frontend.Semantics.{SageSolution, Valuation}

object Eval {

  def apply(state:Map[String,Double], lin: Lin): Double = lin match {
    case Var(v) => state(v)
    case Value(v) => v
    case Add(l1, l2) => apply(state,l1) + apply(state,l2)
    case Mult(v, l)  => apply(state,v)  * apply(state,l)
  }

  def apply(state:Map[String,Double], cond: Cond): Boolean =
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

  def apply(e:SageExpr, t: Double, x: Valuation): Double = e match {
    case SVal(v) => v
    case SArg => t
    case SVar(v) => x(v) // not really used - usually v(0) denotes this case
    case SDiv(e1, e2) => apply(e1,t,x) / apply(e2,t,x)
    case SMult(e1, e2) =>apply(e1,t,x) * apply(e2,t,x)
    case SPow(e1, e2) => math.pow(apply(e1,t,x),apply(e2,t,x))
    case SAdd(e1, e2) => apply(e1,t,x) + apply(e2,t,x)
    case SSub(e1, e2) => apply(e1,t,x) - apply(e2,t,x)
    case SFun(f, args) => (f,args) match {
      case (v,List(SVal(0.0))) if x contains v => x(v)
      case ("exp",v::Nil) => math.exp(apply(v,t,x))
      case ("sin",v::Nil) => math.sin(apply(v,t,x))
      case ("cos",v::Nil) => math.cos(apply(v,t,x))
      case ("sqrt",v::Nil) => math.sqrt(apply(v,t,x))
      case ("log",v::Nil) => math.log(apply(v,t,x))
      case ("log10",v::Nil) => math.log10(apply(v,t,x))
      case (_,v) => throw new RuntimeException(
        s"Unknown function '$f(${args.mkString(",")})'")

    }
  }

  /** Replaces in `sol2` its initial values by `sol1`, and shifts time by `r`
    * @param r delay to shift time
    * @param sol1 initial assignment of symbolic functions to variables
    * @param sol2 follow up assignment, to be updated with `sol1` and `r`
    * @return updated `sol2` with `sol1` and `r`
    */
  def compose(r:Double,sol1:SageSolution,sol2:SageSolution): SageSolution = {
    println(s"Composing\n  ${sol1}  with\n  ${sol2}  after\n  $r  got")
    val res = sol2.mapValues(expr => update(r, expr, sol1))
    println(s"  ${res}")
    res
  }
  def compose(r:Double,sol1:Option[SageSolution],sol2:Option[SageSolution]): Option[SageSolution] =
    (sol1,sol2) match {
      case (Some(s1),Some(s2)) => Some(compose(r,s1,s2))
      case _ => None
    }
  private def update(r:Double,e:SageExpr,sol:SageSolution): SageExpr = e match {
    case SFun(v, List(SVal(0.0))) if sol contains v  =>
      shiftTime(r,sol(v))
    //
    case SFun(f, args) => SFun(f,args.map(e2 =>  update(r,e2,sol)))
    case SDiv(e1, e2)  => SDiv( update(r,e1,sol),update(r,e2,sol))
    case SMult(e1, e2) => SMult(update(r,e1,sol),update(r,e2,sol))
    case SPow(e1, e2)  => SPow( update(r,e1,sol),update(r,e2,sol))
    case SAdd(e1, e2)  => SAdd( update(r,e1,sol),update(r,e2,sol))
    case SSub(e1, e2)  => SSub( update(r,e1,sol),update(r,e2,sol))
    case _ => e
  }
  def shiftTime(r: Double, expr: SageExpr): SageExpr =
    updTime(SSub(SArg,SVal(r)),expr)

  def addTime(r: Double, expr: SageExpr): SageExpr =
    updTime(SAdd(SArg,SVal(r)),expr)

  def setTime(r: Double, expr: SageExpr): SageExpr =
    updTime(SVal(r),expr)

  private def updTime(newt: SageExpr, expr: SageExpr): SageExpr = expr match {
    case SArg => newt
    //
    case SFun(f, args) => SFun(f,args.map(e2 =>   updTime(newt,e2)))
    case SDiv(e1, e2)  => SDiv( updTime(newt,e1), updTime(newt,e2))
    case SMult(e1, e2) => SMult(updTime(newt,e1), updTime(newt,e2))
    case SPow(e1, e2)  => SPow( updTime(newt,e1), updTime(newt,e2))
    case SAdd(e1, e2)  => SAdd( updTime(newt,e1), updTime(newt,e2))
    case SSub(e1, e2)  => SSub( updTime(newt,e1), updTime(newt,e2))
    case _ => expr
  }

  def lin2sage(l:Lin): SageExpr = l match {
    case Var(v) => SVar(v)
    case Value(v) => SVal(v)
    case Add(l1, l2) => SAdd(lin2sage(l1),lin2sage(l2))
    case Mult(v, l) => SMult(SVal(v.v),lin2sage(l))
  }

  //  def apply[A,MA<:Traversable[A]](tr:Traj[A,MA], i:List[Double], cond: Cond): Double => Boolean =
//    t => cond match {
//      case BVal(b) =>  b
//      case And(c1, c2) => apply(tr,i,c1)(t) && apply(tr,i,c2)(t)
//      case Or(c1, c2) => apply(tr,i,c1)(t) || apply(tr,i,c2)(t)
//      case Not(c) => !apply(tr,i,c)(t)
//      case EQ(v, l) => apply(tr,i,v)(t) == apply(tr,i,l)(t)
//      case GT(v, l) => apply(tr,i,v)(t) >  apply(tr,i,l)(t)
//      case LT(v, l) => apply(tr,i,v)(t) <  apply(tr,i,l)(t)
//      case GE(v, l) => apply(tr,i,v)(t) >= apply(tr,i,l)(t)
//      case LE(v, l) => apply(tr,i,v)(t) <= apply(tr,i,l)(t)
//    }
//
//  def apply[A,MA<:Traversable[A]](tr:Traj[A,MA], i: List[Double], lin: Lin): Double => A =
//    t => lin match {
//      case Var(v)      =>
//        val res = tr(t).
//        res
//      case Value(v)    => v
//      case Add(l1, l2) => apply(tr,i,l1)(t) + apply(tr,i,l2)(t)
//      case Mult(v, l)  => apply(tr,i,v)(t) * apply(tr,i,l)(t)
//    }

  def baseSage(vs:Iterable[String]): SageSolution =
    vs.map(v => v -> SFun(v,List(SVal(0.0)))).toMap


}
