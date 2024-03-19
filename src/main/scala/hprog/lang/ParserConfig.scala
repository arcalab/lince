
package hprog.lang

import hprog.ast._
import SyntaxConfig._
import hprog.ast.SymbolicExpr.SyExprVar
import hprog.common.ParserException
import hprog.frontend.Utils

import scala.util.matching.Regex
import scala.util.parsing.combinator._
import scala.math._

/**
  * Parser for Hybrid Programs, using parsing combinators.
  * Created by Ricardo and José in 31/1/23.
  */


// It will take a set of characters and return the corresponding Syntax
object ParserConfig extends RegexParsers {

  /**
    * Main function that parses a string.
    *
    * @param c string representing a program
    * @return Parse result (parsed(connector) or failure(error))
    */
  def parse(c: String): ParseResult[SyntaxConfig] = parseAll(progP, c)
  override def skipWhitespace = true

  override val whiteSpace: Regex = "( |\t|\r|\f|\n|//.*)+".r
  val variable: Parser[String] = """[a-z][a-zA-Z0-9_]*""".r
  
  /** Parsr for a real number */
  lazy val realP: Parser[Double] =
    """-?[0-9]+(\.([0-9]+))?""".r ^^ { s: String => s.toDouble }

  /** Parser for an integer number */
  lazy val intP: Parser[Int] =
    """[0-9]+""".r ^^ { s: String => s.toInt }
 

  //   ///////////////
  //   /// Program ///
  //   ///////////////

  /*lazy val axis: Parser[SyntaxConfig] =
    "Axis Variables=[" ~>  variable ~ "," ~ tail <~ "]" ^^ {
      case v ~ _ ~ t => v ~ t
    }*/

  /*lazy val tail: Parser[SyntaxConfig] = 
   variable |
   variable ~ "," ~ tail ^^ {
    case v ~ _ ~ t => v ~ t
   }*/

  // Parser for a program that checks if the program is closed before returning
  lazy val progP: Parser[SyntaxConfig] =
    opt(axisList) ~ opt(maxTime) ~ opt(maxIterations) ^^ {
      case Some(axis) ~ Some(mt) ~ Some(mi) => Prog(axis, mt, mi)
      case Some(axis) ~ Some(mt) ~ None => Prog(axis, mt, Value(100.0))
      case Some(axis) ~ None ~ Some(mi) => Prog(axis, Value(20.0), mi)
      case Some(axis) ~ None ~ None => Prog(axis, Value(20.0), Value(100.0))
      case None ~ Some(mt) ~ Some(mi) => Prog(AxisList(List()), mt, mi)
      case None ~ Some(mt) ~ None => Prog(AxisList(List()), mt, Value(100.0))
      case None ~ None ~ Some(mi) => Prog(AxisList(List()), Value(20.0), mi)
      case None ~ None ~ None => Prog(AxisList(List()), Value(20.0), Value(100.0))
    }

  // Parser for axis variables list
  lazy val axisList: Parser[AxisList] =
    "Axis Variables=[" ~> repsep(variable, ",") <~ "]" ^^ { vars =>
      if (vars.length < 2)
        throw new ParserException("At least two variables are required in axis declaration")
      else
        AxisList(vars.map(Var))
    }

  // Parser for maxTime
  lazy val maxTime: Parser[Value] =
    "maxTime=" ~> realP ^^ { s => Value(s) }

  // Parser for maxIterations
  lazy val maxIterations: Parser[Value] =
    "maxIterations=" ~> intP ^^ { s => Value(s) }


}

