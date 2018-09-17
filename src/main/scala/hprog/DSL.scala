package hprog

import java.io.File

import hprog.ast._
import hprog.common.{ParserException, TypeCheckException}
import hprog.lang.Parser

import scala.language.implicitConversions

/**
 * Created by jose on 17/07/18.
 */
object DSL {
  implicit def str2Var(s:String): Var = ast.Var(s)
  implicit def bool2Cond(b:Boolean): BVal = BVal(b)
  implicit def real2Lin(n:Double): Value = Value(n)
  implicit def int2Lin(n:Int): Value = Value(n)
  implicit def real2Dur(n:Double): Dur = For(Value(n))
  implicit def int2Dur(n:Int): Dur = For(Value(n))
  implicit def cond2Dur(c:Cond): Dur = When(c)
  implicit def dEq2dEqs(de:DiffEq): DiffEqs= DiffEqs(List(de),Forever)


  val x:Var="x"; val y:Var="y"; val p:Var="p"; val v:Var="v"; val g:Var="g"

  // examples
  val ex1 = Assign(x,Value(2))            // assignment
  val ex2 = (y^=3) & 34     // statement
  val ex3 = (x^=2) & (y^=3) // statement
  val ex4 = (x^=2) ~ (y^=3) // program
  val ex5 = (x:=0) ~ ((x^=2) & (y^=3) & 34) ~ ((x^=2) & (x > 2)) // program
  val ex6 = (x^=1) & ((x > 3*x) && (x<5))
  val ex7 = ((p^=v) & (v^=g) & ((p<=0) && (v<=0))) ~ (v := (-0.5)*v) // bouncing ball
  val ex8 = (x^=1) & (((x > 3*x) && (x<5)) || (y>=3))
  val ex9 = (x^=1) & ((x > 3*x) && ((x<5) || (y>=3)))

  /**
    * Parses a string into a program.
    * @param s string representing a program
    * @return parsed program
    */
  def parse(s:String): Prog =  Parser.parse(s) match {
    case Parser.Success(result, next) => result
    case f: Parser.NoSuccess => throw new ParserException(f.msg)
  }

  val parseWithError: String => Parser.ParseResult[Prog] = Parser.parse

}
