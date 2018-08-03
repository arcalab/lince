package hprog.frontend

import hprog.ast._
import hprog.common.TypeCheckException

/**
 * Created by jose on 02/08/18.
 */
object TypeCheck {
//
//  // set of variables with their types
//  private class Context {
//    private type Ctx = Map[String,ExprType]
//    protected val vars: Ctx = Map()
//    private def build(i:Set[String],b:Set[String]) = new Context {
//      override val vars: Ctx = i.map(_->IntType).toMap ++ b.map(_->BoolType).toMap
//    }
//
//    /** checks if a variable is in the context. */
//    def contains(variable:String): Boolean = vars contains variable
//    /** checks if a variable is in the context. */
//    def apply(v:Var): Boolean = contains (v.name)
//    /** checks if a variable is in the context with a given type. */
//    def apply(v:Var,et:ExprType):Boolean = vars.get(v.name).contains(et)
//    /** Check if 2 contexts are disjoint */
//    def disjoint(other:Context): Boolean =
//      (vars.keySet  & other.vars.keySet)  == Set()
////    def addInt(v:String): Context =
////      addVar(Var(v),IntType)
////    def addBool(v:String): Context = {
////      addVar(Var(v),BoolType)
////    }
//
//    def addVar(v:Var,et: ExprType): Context = {
//      val mv = vars
//      new Context {
//        override val vars: Ctx = mv + (v.name -> et)
//      }
//    }
//
//
//    /** Number of variables. */
//    def size: Int = vars.size
//
//    override def toString: String =
//      vars.map(et=>et._1+":"+et._2).mkString("[",",","]")
//  }
//
//
//  private var seed = 0
//  private def fresh() = {seed += 1; "x"+seed}
//  private def fresh(base:Var) = {seed += 1; base.name+"!"+seed}
//
//
////  def isIExpr(gamma:Context,a:Expr): Boolean = a match {
////    case Var(x) => gamma(Var(x))
////    case _: Expr => true
////    case _: Expr => false
////  }
//
//
//  def apply(p:Progr) = {
//    seed=0
//    check(new Context(),p)
//  }
//
//  private def check(gamma:Context,p:Progr): Unit = p match {
//    case Seq(Nil) => {}
//    case Seq(p1::ps) => {check(gamma,p1); check(gamma,Seq(ps))}
//    case Statement(Nil,dur) => checkDur(gamma,dur)
//    case Statement(Assgn(v,exp)::rest,dur) =>
//      check(gamma,exp,IntType)
//      checkDur(gamma,dur)
//  }
//  private def checkDur(g:Context,dur:Option[Expr]) = dur.map(check(g,_,BoolType))
//
//  private def isInt(e:Expr,t:ExprType): Unit =
//    if (t!=IntType) throw new TypeCheckException(s"${e} is not type Int")
//  private def isBool(e:Expr,t:ExprType): Unit =
//    if (t!=BoolType) throw new TypeCheckException(s"${e} is not type Bool")
//
//
//  def check(gamma:Context,a:Expr,t:ExprType):Unit = a match {
//    case v@Var(x,d)     => if (!gamma(v,t)) throw new TypeCheckException(s"$x:${t}_$d not in the context $gamma")
//
//    case Val(_)      => isInt(a,t)
//    case Add(e1, e2) => isInt(a,t); check(gamma,e1,t); check(gamma,e2,t)
//    case Sub(e1, e2) => isInt(a,t); check(gamma,e1,t); check(gamma,e2,t)
//    case Mul(e1, e2) => isInt(a,t); check(gamma,e1,t); check(gamma,e2,t)
//    case Div(e1, e2) => isInt(a,t); check(gamma,e1,t); check(gamma,e2,t)
//
//    case BVal(_)     => isBool(a,t)
//    case EQ(e1, e2)  => isBool(a,t); check(gamma,e1,IntType); check(gamma,e2,IntType)
//    case GT(e1, e2)  => isBool(a,t); check(gamma,e1,IntType); check(gamma,e2,IntType)
//    case LT(e1, e2)  => isBool(a,t); check(gamma,e1,IntType); check(gamma,e2,IntType)
//    case GE(e1, e2)  => isBool(a,t); check(gamma,e1,IntType); check(gamma,e2,IntType)
//    case LE(e1, e2)  => isBool(a,t); check(gamma,e1,IntType); check(gamma,e2,IntType)
//    case And(Nil)    =>
//    case And(e::es)  => isBool(a,t); check(gamma,e,t); check(gamma,And(es),t)
//    case Or(e1, e2)  => isBool(a,t); check(gamma,e1,t); check(gamma,e2,t)
//    case Not(e1)     => isBool(a,t); check(gamma,e1,t)
//  }
//
//
//  // Checks if `gamma,x |- c`, returns its type and a rename for `x` if `x` already exists in `gamma`.
//  private def checkAndAddVar(gamma:Context,x:Var,et:ExprType,e:Expr): Unit = {
//      check(gamma.addVar(x,et), e,et)
//  }
}
