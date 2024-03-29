package hprog.lang

import hprog.ast.SyntaxConfig._
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
  def parse(c: String): ParseResult[SyntaxConfig] = parseAll(config, c)
  override def skipWhitespace = true

  override val whiteSpace: Regex = "( |\t|\r|\f|\n|//.*)+".r
  val variable: Parser[String] = """"[a-zA-Z][a-zA-Z0-9_]*"""".r

  /** Parser for a real number */
  lazy val realP: Parser[Double] =
    """-?[0-9]+(\.([0-9]+))?""".r ^^ { s: String => s.toDouble }

  /** Parser for an integer number */
  lazy val intP: Parser[Int] =
    """[0-9]+""".r ^^ { s: String => s.toInt }

  // Parser for a program that checks if the program is closed before returning
  lazy val config: Parser[SyntaxConfig] =
    repsep(axis | maxTime | maxIterations, ",") ^^ { results =>
      var axisOpt: Option[AxisList] = None
      var maxTimeOpt: Option[MaxTime] = None
      var maxIterationsOpt: Option[MaxIterations] = None

      results.foreach {
        case axisList @ AxisList(vars) => 
          if (axisOpt.isDefined) 
            throw new ParserException("Duplicate Axis List declaration")
          axisOpt = Some(axisList)
          
        case v: MaxTime => 
          if (maxTimeOpt.isDefined) 
            throw new ParserException("Duplicate Max Time declaration")
          maxTimeOpt = Some(v)
        
        case v: MaxIterations => 
          if (maxIterationsOpt.isDefined) 
            throw new ParserException("Duplicate max Iterations declaration")
          maxIterationsOpt = Some(v)
          
        case _ => throw new ParserException("Invalid configuration format")
      }

      SyntaxConfig(axisOpt, maxTimeOpt, maxIterationsOpt)
    } 

  // Parser for axis variables list
  lazy val axis: Parser[AxisList] =
    "Axis:[" ~> repsep(variable, ",") <~ "]" ^^ { vars =>
      if (vars.length < 2)
        throw new ParserException("At least two variables are required in axis declaration")
      else
        AxisList(vars.map(Var))
    }

  // Parser for maxTime
  lazy val maxTime: Parser[MaxTime] =
    "maxTime:" ~> realP ^^ { s => MaxTime(s) }

  // Parser for maxIterations
  lazy val maxIterations: Parser[MaxIterations] =
    "maxIterations:" ~> intP ^^ { s => MaxIterations(s) }
    
}
