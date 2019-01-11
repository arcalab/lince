package hprog.frontend

import hprog.ast._

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


}
