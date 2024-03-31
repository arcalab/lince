package hprog.lang

import hprog.ast.SyntaxConfig._
import hprog.ast.SyntaxConfig.{ConfigVal, StrValue, MaxTimeValue, MaxIterationsValue, SeqValue}
import hprog.ast.SymbolicExpr.SyExprVar
import hprog.common.ParserException
import hprog.frontend.Utils

import scala.util.matching.Regex
import scala.util.parsing.combinator._
import scala.math._

/**
  * Parser for Hybrid Programs, using parsing combinators.
  */

object ParserConfig extends RegexParsers {

  def parse(c: String): ParseResult[SyntaxConfig] = parseAll(config, c)
  override def skipWhitespace = true

  override val whiteSpace: Regex = "( |\t|\r|\f|\n|//.*)+".r
  val variable: Parser[String] = """"[a-zA-Z][a-zA-Z0-9_]*"""".r

  lazy val realP: Parser[Double] =
    """-?[0-9]+(\.([0-9]+))?""".r ^^ { s: String => s.toDouble }

  lazy val intP: Parser[Int] =
    """[0-9]+""".r ^^ { s: String => s.toInt }

  lazy val config: Parser[SyntaxConfig] =
    repsep(configOption, ",") ^^ { options =>
      SyntaxConfig(options.toMap)
    }

  lazy val configOption: Parser[(String, ConfigVal)] =
    axis | maxTime | maxIterations

  lazy val axis: Parser[(String, ConfigVal)] =
    "Axis:[" ~> repsep(variable, ",") <~ "]" ^^ { vars =>
      if (vars.length < 2)
        throw new ParserException("At least two variables are required in axis declaration")
      else
        "Axis" -> SeqValue(vars.map(StrValue))
    }

  lazy val maxTime: Parser[(String, ConfigVal)] =
    "maxTime:" ~> realP ^^ { s => "MaxTimeValue" -> MaxTimeValue(s) }

  lazy val maxIterations: Parser[(String, ConfigVal)] =
    "maxIterations:" ~> intP ^^ { s => "MaxIterationsValue" -> MaxIterationsValue(s) }
    
}

