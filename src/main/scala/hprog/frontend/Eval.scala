package hprog.frontend

import hprog.ast._

object Eval {
  def apply(tr:Trajectory, i:List[Double], cond: Cond): Double => Boolean =
    t => cond match {
      case BVal(b) =>  b
      case And(c1, c2) => apply(tr,i,c1)(t) && apply(tr,i,c2)(t)
      case Or(c1, c2) => apply(tr,i,c1)(t) || apply(tr,i,c2)(t)
      case Not(c) => !apply(tr,i,c)(t)
      case EQ(v, l) => apply(tr,i,v)(t) == apply(tr,i,l)(t)
      case GT(v, l) => apply(tr,i,v)(t) >  apply(tr,i,l)(t)
      case LT(v, l) => apply(tr,i,v)(t) <  apply(tr,i,l)(t)
      case GE(v, l) => apply(tr,i,v)(t) >= apply(tr,i,l)(t)
      case LE(v, l) => apply(tr,i,v)(t) <= apply(tr,i,l)(t)
    }

   def apply(tr:Trajectory, i: List[Double], lin: Lin): Double => Double =
    t => lin match {
      case Var(v)      =>
        val res = tr.function(i,t)(tr.vars.indexOf(v))
        res
      case Value(v)    => v
      case Add(l1, l2) => apply(tr,i,l1)(t) + apply(tr,i,l2)(t)
      case Mult(v, l)  => apply(tr,i,v)(t) * apply(tr,i,l)(t)
    }

  def apply(state:Map[String,Double], lin: Lin): Option[Double] = lin match {
    case Var(v) => state.get(v)
    case Value(v) => Some(v)
    case Add(l1, l2) => for (v1<-apply(state,l1);v2<-apply(state,l1)) yield v1+v2
    case Mult(v, l) => for (v1<-apply(state,v);v2<-apply(state,l)) yield v1*v2
  }

}
