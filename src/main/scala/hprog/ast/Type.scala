package hprog.ast

/**
  * Created by jose on 17/07/18.
 */


sealed abstract class ExprType
case object IntType  extends ExprType { override def toString = "I"}
case object BoolType extends ExprType { override def toString = "B"}


