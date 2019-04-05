package hprog.lang

import hprog.common.ParserException

import scala.util.matching.Regex
import scala.util.parsing.combinator._

/**
  * Parser for Hybrid Programs, using parsing combinators.
  * Created by jose on 19/07/2018.
  */
object SageParser extends RegexParsers {

  case class SageSol(vars:Set[String],sol: Map[String,SageFunction]) {
    def ++(that:SageSol): SageSol = {
      SageSol( vars ++ that.vars , sol ++ that.sol )
    }
  }
  type SageFunction = Double => InitVals => Double
  type InitVals = Map[String,Double]
 
  /**
    * Main function that parses a string.
    *
    * @param c string representing a reply from Sage
    * @return Parse result (parsed(functions) or failure(error))
    */
  def parse(c: String): ParseResult[SageSol] = parseAll(sols, c)

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

  lazy val sols: Parser[SageSol] =
    "["~>solsIn<~"]"

  lazy val solsIn: Parser[SageSol] = 
    eqDef ~ opt("," ~> solsIn) ^^ {
      case e ~ None => e
      case e1 ~ Some(e2) => e1 ++ e2
    }  
  lazy val eqDef: Parser[SageSol] = 
    identifier ~ "(_t_) == " ~ eqExpr ^^ {
      case id ~ _ ~ exp => SageSol(Set(id),Map(id->exp))
    }

  lazy val eqExpr: Parser[SageFunction] = 
    prod ~ opt(("+"~eqExpr)|("-"~eqExpr)) ^^ {
      case e ~ None => e
      case e1 ~ Some("+"~e2) => (t:Double) => (ctx:InitVals) => e1(t)(ctx) + e2(t)(ctx) 
      case e1 ~ Some("-"~e2) => (t:Double) => (ctx:InitVals) => e1(t)(ctx) - e2(t)(ctx) 
    }

  lazy val prod: Parser[SageFunction] = 
    expn ~ opt(("*"~prod)|("/"~prod)) ^^ {
      case e ~ None => e
      case e1 ~ Some("*"~e2) => (t:Double) => (ctx:InitVals) => e1(t)(ctx) * e2(t)(ctx) 
      case e1 ~ Some("/"~e2) => (t:Double) => (ctx:InitVals) => e1(t)(ctx) / e2(t)(ctx) 
    }

  lazy val expn: Parser[SageFunction] =
    lit ~ opt("^"~>lit) ^^ {
      case e ~ None => e
      case e1 ~ Some(e2) => (t:Double) => (ctx:InitVals) => Math.pow(e1(t)(ctx),e2(t)(ctx))
    }

  lazy val lit: Parser[SageFunction] =
    function | rational | time | "("~>lit<~")" | negation

  lazy val negation: Parser[SageFunction] =
    "-"~>lit ^^ (e => (t: Double) => (ctx: InitVals) => -e(t)(ctx))
  lazy val time: Parser[SageFunction] =
    "_t_" ^^^ ((t:Double)=>(_:InitVals)=>t)
  lazy val rational: Parser[SageFunction] =
    float~opt("/"~>float) ^^ {
      case f~mbf => (_:Double) => (_:InitVals) => f / mbf.getOrElse(1.0)
    }
  lazy val float: Parser[Double] =
    """-?[0-9]+(\.([0-9]+))?""".r ^^ { s: String => s.toDouble }

  lazy val function: Parser[SageFunction] =
    identifier~"("~eqExpr~")" ^^ {
      case name~_~arg~_ => (t:Double)=>(ctx:InitVals)=> (name,arg(t)(ctx)) match {
        case (_,0) if ctx.contains(name) => ctx(name)
        case ("sin",v) => Math.sin(v)
        case ("cos",v) => Math.cos(v)
        case (_,v) => throw new ParserException(s"Unknown function '$name($v)'")
      }
    }
}

