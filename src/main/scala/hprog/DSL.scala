package hprog

import hprog.ast.SymbolicExpr.SyExprAll
import hprog.ast._
import Syntax._
import hprog.common.ParserException
import hprog.frontend.CommonTypes.Valuation
import hprog.frontend.solver.LiveSageSolver
import hprog.frontend.{CommonTypes, Deviator, Distance, Traj}
import hprog.lang.{Parser, Parser2, SageParser}
import hprog.frontend.Utils
import scala.language.implicitConversions

/**
 * Created by jose on 17/07/18.
 */
object DSL {
  implicit def str2VarNotLin(s:String): Var = Var(s) //new
  implicit def bool2Cond(b:Boolean): BVal = BVal(b)
  implicit def real2NotLin(n:Double): Value = Value(n)//new
  implicit def int2NotLin(n:Int): Value = Value(n)//new
  implicit def real2Dur(n:Double): Dur = For(Value(n))
  implicit def int2Dur(n:Int): Dur = For(Value(n))
  implicit def cond2Dur(c:Cond): Dur = Until(c,None,None)
  implicit def dEq2dEqs(de:DiffEq): DiffEqs= DiffEqs(List(de),Forever)
  implicit def assg2Atom(a:Assign): Atomic = Atomic(List(a),DiffEqs(Nil,For(Value(0))))
  implicit def dEqs2Atom(des:DiffEqs): Atomic = Atomic(Nil,des)
 

 
  /**
    * Parses a string into a program.
    * @param s string representing a program
    * @return parsed program
    */
  def parse(s:String): Syntax =  {
    Parser.parse(s) match {
      case Parser.Success(result, _) =>
        result
      case f: Parser.NoSuccess =>
        throw new ParserException(f.toString)
    }
  }

  /**
    * Parses a string into an expression.
    * @param s string representing an expression
    * @return parsed expression
    */
  def parseExpr(s:String): SyExprAll =  {
    SageParser.parseExpr(s) match {
      case SageParser.Success(result, _) =>
        result
      case f: SageParser.NoSuccess =>
        throw new ParserException(f.toString)
    }
  }

  /**
    * Parses a string into an expression.
    * @param s string representing an expression
    * @return parsed expression
    */
  def parseCond(s:String): Cond =  {
    Parser2.parseCond(s) match {
      case Right(result) => //Parser.Success(result, _) =>
        result
      case Left(f) => //: Parser.NoSuccess =>
        throw new ParserException(f.toString)
    }
  }

  val parseWithError: String => Either[String,Syntax] = Parser2.parse

}
