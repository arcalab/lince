package hprog.frontend

import hprog.ast._
import Syntax._
import hprog.backend.Show
import hprog.common.TypeCheckException

/**
 * Created by jose on 02/08/18.
 */
object TypeCheck {
  case class Result(errors:Option[List[String]]) { // None = ok, Just(errors) = not ok
    def ++(other:Result): Result =
      if (errors.isEmpty && other.errors.isEmpty) Result(None)
      else Result(Some(errors.getOrElse(Nil) ++ other.errors.getOrElse(Nil)))
  }


  def isDur(c:Cond): Boolean = c match {
    case And(c1, c2) => isDur(c1) && isDur(c2)
    case Or(c1, c2) => isDur(c1) && isDur(c2)
    case EQ(v, l) => true
    case _ => false
  }

}
