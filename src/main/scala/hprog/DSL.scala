package hprog

import java.io.File

import hprog.ast._
import hprog.common.TypeCheckException
import hprog.lang.Parser

import scala.language.implicitConversions
import scala.util.control.NonFatal

/**
 * Created by jose on 17/07/18.
 */
object DSL {
  implicit def str2Var(s:String): Var = ast.Var(s,0)
  implicit def bool2BExp(b:Boolean): BVal = BVal(b)
  implicit def real2Exp(n:Double): Val = Val(n)
  implicit def int2Exp(n:Int): Val = Val(n)

  implicit def assg2St(a:Assgn): Statement = Statement(List(a),None)

  type E = Expr
  // helper for DSL

  def not(b:Expr) = Not(b)

  // included for the demo at FACS'15
//  val x:I="x"; val y:I="y"; val z:I="z"; val n:I="n"; val b:B="b"; val c:B="c"
  val x:Var="x"; val y:Var="y"


  // examples
  val ex1 = x:=2            // assignment
  val ex2 = (y:=3) & 34     // statement
  val ex3 = (x:=2) & (y:=3) // statement
  val ex4 = (x:=2) ~ (y:=3) // program
  val ex5 = ((x := 2) & (y:=3) & 34) ~ ((x.! :=2) & (x > 2)) // program
  val ex6 = (x.! := (x > (y*3))) & (x<5)


}
