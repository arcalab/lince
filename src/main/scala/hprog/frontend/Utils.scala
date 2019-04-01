package hprog.frontend

import hprog.ast._

object Utils {
  def replaceVar(by:String=>String,e:Lin): Lin = e match {
    case Var(v) => Var(by(v))
    case Value(v) => e
    case Add(l1, l2) => Add(replaceVar(by,l1),replaceVar(by,l2))
    case Mult(v, l) => Mult(v,replaceVar(by,l))
  }

}
