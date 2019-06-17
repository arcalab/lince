package hprog.lang

import hprog.ast.{SAdd, SArg, SDiv, SFun, SMult, SPow, SSub, SVal, SageExpr}
import hprog.common.ParserException
import hprog.frontend.Semantics.{SFunction, SolVars, Valuation}

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
    "["~>solsIn<~"]" |
      eqExpr ^^ (f => SolVars(Map(""->f)))

  lazy val solsIn: Parser[SolVars] = 
    eqDef ~ opt("," ~> solsIn) ^^ {
      case e ~ None => e
      case e1 ~ Some(e2) => e1.sol.keySet.intersect(e2.sol.keySet).toList match {
        case Nil => SolVars(e1.sol ++ e2.sol)
        case shared => throw new ParserException(s"Variable(s) defined more than once: ${
          shared.mkString(",")}")
      }
    }
  lazy val eqDef: Parser[SolVars] = 
    identifier ~ "(_t_) == " ~ eqExpr ^^ {
      case id ~ _ ~ exp => SolVars(Map(id->exp))
    }

  lazy val eqExpr: Parser[SageExpr] =
    prod ~ opt(("+"~eqExpr)|("-"~eqExpr)) ^^ {
      case e ~ None => e
      case e1 ~ Some("+"~e2) => SAdd(e1,e2)
      case e1 ~ Some("-"~e2) => SSub(e1,e2)
      case _ ~ Some(s~_) => throw new ParserException(s"Unknown operator $s")
    }

  lazy val prod: Parser[SageExpr] =
    expn ~ opt(("*"~prod)|("/"~prod)) ^^ {
      case e ~ None => e
      case e1 ~ Some("*"~e2) => SMult(e1,e2)
      case e1 ~ Some("/"~e2) => SDiv(e1,e2)
      case _ ~ Some(s~_) => throw new ParserException(s"Unknown operator $s")
    }

  lazy val expn: Parser[SageExpr] =
    "e" ~ "^" ~ lit ^^ {
      case _~_~e => SFun("exp",List(e))//(t:Double) => (ctx:Valuation) => Math.exp(e(t)(ctx))
    } |
    lit ~ opt("^"~>lit) ^^ {
      case e ~ None => e
      case e1 ~ Some(e2) => SPow(e1,e2)//(t:Double) => (ctx:Valuation) => Math.pow(e1(t)(ctx),e2(t)(ctx))
    }

  lazy val lit: Parser[SageExpr] =
    function | rational | time | "("~>lit<~")" | negation

  lazy val negation: Parser[SageExpr] =
    "-"~>lit ^^ (e => SSub(SVal(0.0),e)) // (e => (t: Double) => (ctx: Valuation) => -e(t)(ctx))
  lazy val time: Parser[SageExpr] =
    "_t_" ^^^ SArg // ((t:Double)=>(_:Valuation)=>t)
  lazy val rational: Parser[SageExpr] =
    float~opt("/"~>float) ^^ {
      case f~None => f
      case f~Some(f2) => SDiv(f,f2) // (_:Double) => (_:Valuation) => f / mbf.getOrElse(1.0)
    }
  lazy val float: Parser[SageExpr] =
    """-?[0-9]+(\.([0-9]+))?""".r ^^ { s: String => SVal(s.toDouble) }

  lazy val function: Parser[SageExpr] =
    identifier~"("~eqExprs~")" ^^ {
      case name~_~arg~_ =>
        SFun(name,arg)
//      case name~_~arg~_ => (t:Double)=>(ctx:Valuation)=> (name,arg(t)(ctx)) match {
//        case (_,0) if ctx.contains(name) => ctx(name)
//        case ("sin",v) => Math.sin(v)
//        case ("cos",v) => Math.cos(v)
//        case ("sqrt",v) => Math.sqrt(v)
//        case ("log",v) => Math.log(v)
//        case ("log10",v) => Math.log10(v)
//        case (_,v) => throw new ParserException(s"Unknown function '$name($v)'")
//      }
    }
  lazy val eqExprs: Parser[List[SageExpr]] =
    eqExpr ~ opt(","~>eqExprs) ^^ {
      case e~None => List(e)
      case e~Some(e2) => e::e2
    }
}

