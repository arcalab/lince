package hprog.lang

import hprog.ast.SyntaxConfig._
import hprog.ast.SyntaxConfig.{ConfigVal, StrValue, MaxTimeValue, MaxIterationsValue, AxisListValue, VarList, GraphTypeValue, InitialValuesValue, PerturbationUpToValue}
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
  val variable: Parser[String] = """[a-zA-Z][a-zA-Z0-9_]*""".r

  lazy val realP: Parser[Double] =
    """-?[0-9]+(\.([0-9]+))?""".r ^^ { s: String => s.toDouble }

  lazy val intP: Parser[Int] =
    """[0-9]+""".r ^^ { s: String => s.toInt }

  lazy val config: Parser[SyntaxConfig] =
    repsep(configOption, ",") ^^ { options =>
      SyntaxConfig(options.toMap)
    }

  lazy val configOption: Parser[(String, ConfigVal)] =
    axis | maxTime | maxIterations | graphType | initialValues | perturbationUpTo

  lazy val axis: Parser[(String, ConfigVal)] =
    "Axis:[" ~> repsep(tripleVarVariable | pairVariable | singleVariable, ",") <~ "]" ^^ { vars =>
      "Axis" -> AxisListValue(vars)
    }

  lazy val tripleVarVariable: Parser[ConfigVal] =
    "(" ~> variable ~ ("," ~> variable) ~ ("," ~> variable <~ ")") ^^ {
    case v1 ~ v2 ~ v3 => VarList(List(StrValue(v1), StrValue(v2), StrValue(v3)))
  }

  lazy val pairVariable: Parser[ConfigVal] =
    "(" ~> variable ~ ("," ~> variable <~ ")") ^^ {
      case v1 ~ v2 => VarList(List(StrValue(v1), StrValue(v2)))
    }

  lazy val singleVariable: Parser[ConfigVal] =
    variable ^^ { v => StrValue(v) }

  lazy val maxTime: Parser[(String, ConfigVal)] =
    "maxTime:" ~> realP ^^ { s => "MaxTimeValue" -> MaxTimeValue(s) }

  lazy val maxIterations: Parser[(String, ConfigVal)] =
    "maxIterations:" ~> intP ^^ { s => "MaxIterationsValue" -> MaxIterationsValue(s) }

   lazy val graphType: Parser[(String, ConfigVal)] =
    "graphType:" ~> variable ^^ { t => "GraphTypeValue" -> GraphTypeValue(t) }
  
  lazy val initialValues: Parser[(String, ConfigVal)] =
    "initialValues:[" ~> repsep(initialValue, ",") <~ "]" ^^ { values =>
      "InitialValues" -> InitialValuesValue(values)
    }

  lazy val initialValue: Parser[(String, List[Double])] =
    "(" ~> variable ~ ("," ~> "[" ~> repsep(realP, ",") <~ "]" <~ ")") ^^ {
      case v ~ list => ("_" + v, list)
    }

  lazy val perturbationUpTo: Parser[(String, ConfigVal)] =
    "perturbationUpTo:" ~> realP ^^ { v => "PerturbationUpTo" -> PerturbationUpToValue(v) }
}
