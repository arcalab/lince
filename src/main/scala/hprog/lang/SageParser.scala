package hprog.lang

import hprog.common.ParserException
import hprog.frontend.Semantics.{Valuation,SFunction,SolVars}

import scala.util.matching.Regex
import scala.util.parsing.combinator._

/**
  * Parser for Hybrid Programs, using parsing combinators.
  * Created by jose on 19/07/2018.
  */
object SageParser extends RegexParsers {

  /**
    * Main function that parses a string.
    *
    * @param c string representing a reply from Sage
    * @return Parse result (parsed(functions) or failure(error))
    */
  def parse(c: String): ParseResult[SolVars] = parseAll(sols, c)

  //  def pexp(c:String): ParseResult[Cond] = parseAll(condP,c)

  override def skipWhitespace = true

  override val whiteSpace: Regex = "( |\t|\r|\f|\n|//.*)+".r
//  override val whiteSpace: Regex = "[ \t\r\f\n]+".r
  val identifier: Parser[String] = """[a-z][a-zA-Z0-9_]*""".r
  val identifierCap: Parser[String] = """[a-zA-Z][a-zA-Z0-9_]*""".r
  val nameP: Parser[String] = "[a-zA-Z0-9.-_!$]+".r
  val floatP: Parser[String] = """[a-z][a-zA-Z0-9_]*""".r

  //   ///////////////
  //   /// Program ///
  //   ///////////////

  lazy val sols: Parser[SolVars] =
    "["~>solsIn<~"]"

  lazy val solsIn: Parser[SolVars] = 
    eqDef ~ opt("," ~> solsIn) ^^ {
      case e ~ None => e
      case e1 ~ Some(e2) => e1 ++ e2
    }  
  lazy val eqDef: Parser[SolVars] = 
    identifier ~ "(_t_) == " ~ eqExpr ^^ {
      case id ~ _ ~ exp => SolVars(Set(id),Map(id->exp))
    }

  lazy val eqExpr: Parser[SFunction] = 
    prod ~ opt(("+"~eqExpr)|("-"~eqExpr)) ^^ {
      case e ~ None => e
      case e1 ~ Some("+"~e2) => (t:Double) => (ctx:Valuation) => e1(t)(ctx) + e2(t)(ctx)
      case e1 ~ Some("-"~e2) => (t:Double) => (ctx:Valuation) => e1(t)(ctx) - e2(t)(ctx)
    }

  lazy val prod: Parser[SFunction] = 
    expn ~ opt(("*"~prod)|("/"~prod)) ^^ {
      case e ~ None => e
      case e1 ~ Some("*"~e2) => (t:Double) => (ctx:Valuation) => e1(t)(ctx) * e2(t)(ctx)
      case e1 ~ Some("/"~e2) => (t:Double) => (ctx:Valuation) => e1(t)(ctx) / e2(t)(ctx)
    }

  lazy val expn: Parser[SFunction] =
    lit ~ opt("^"~>lit) ^^ {
      case e ~ None => e
      case e1 ~ Some(e2) => (t:Double) => (ctx:Valuation) => Math.pow(e1(t)(ctx),e2(t)(ctx))
    }

  lazy val lit: Parser[SFunction] =
    function | rational | time | "("~>lit<~")" | negation

  lazy val negation: Parser[SFunction] =
    "-"~>lit ^^ (e => (t: Double) => (ctx: Valuation) => -e(t)(ctx))
  lazy val time: Parser[SFunction] =
    "_t_" ^^^ ((t:Double)=>(_:Valuation)=>t)
  lazy val rational: Parser[SFunction] =
    float~opt("/"~>float) ^^ {
      case f~mbf => (_:Double) => (_:Valuation) => f / mbf.getOrElse(1.0)
    }
  lazy val float: Parser[Double] =
    """-?[0-9]+(\.([0-9]+))?""".r ^^ { s: String => s.toDouble }

  lazy val function: Parser[SFunction] =
    identifier~"("~eqExpr~")" ^^ {
      case name~_~arg~_ => (t:Double)=>(ctx:Valuation)=> (name,arg(t)(ctx)) match {
        case (_,0) if ctx.contains(name) => ctx(name)
        case ("sin",v) => Math.sin(v)
        case ("cos",v) => Math.cos(v)
        case (_,v) => throw new ParserException(s"Unknown function '$name($v)'")
      }
    }
}

