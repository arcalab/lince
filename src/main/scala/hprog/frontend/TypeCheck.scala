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

  //  def seq[A](f:(A,A)=>A)(x:Result[A],y:Result[A]): Result[A] = (x,y) match {
  //    case (Left(m1),Left(m2)) => Left(m1++m2)
  //    case (Left(m1),_) => Left(m1)
  //    case (Right(a),Right(b)) => Right(f(a,b))
  //  }
  //  def seqUnit(x:Result[Unit],y:Result[Unit]): Result[Unit] = seq((_,_)=>())(x,y)

//  def findProblems(f:Syntax=>Result, prog:Syntax): Result =
//    f(prog) ++
//      (prog match {
//        //        case Assign(v, e) => Result(None)
//        //        case DiffEqs(eqs, dur) => Result(None)
//        case Seq(ps) => ps.map(findProblems(f,_)).foldRight(Result(None))(_++_)
//        //        case Skip => Result(None)
//        case ITE(_, thenP, elseP) => findProblems(f,thenP) ++ findProblems(f,elseP)
//        case While(_, doP) => findProblems(f,doP)
//        case _ => Result(None)
//      })

  def isDur(c:Cond): Boolean = c match {
    case And(c1, c2) => isDur(c1) && isDur(c2)
    case Or(c1, c2) => isDur(c1) && isDur(c2)
    //    case BVal(b) => false
    //    case Not(c) => false
    case EQ(v, l) => true
    //    case GT(v, l) => false
    //    case LT(v, l) => false
    //    case GE(v, l) => false
    //    case LE(v, l) => false
    case _ => false
  }

//  def validDur(prog: Syntax): Result = findProblems({
//    case DiffEqs(_,Until(cond)) if !isDur(cond) => Result(Some(List(s"invalid duration condition: ${Show(cond)}")))
//    case _ => Result(None)
//  },prog
//  )
}
