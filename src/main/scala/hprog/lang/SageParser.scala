package hprog.lang

import hprog.ast.SymbolicExpr.{All, SyExprAll, SyExprTime}
import hprog.ast._
import hprog.common.ParserException
import hprog.frontend.Eval
import hprog.frontend.CommonTypes.SySolution

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
  def parseSol(c: String): ParseResult[SySolution] = {
    val res = parseAll(sols, c)
//    res match {
//      case Success(result, _) =>
//        println(s"[ParserS] $c  -->  ${hprog.backend.Show(result)}")
//      case _ =>
//    }
    res
  }

  /**
    * Main function that parses a string into a SageExpr.
    *
    * @param c string representing a reply from Sage
    * @return Parse result (parsed(functions) or failure(error))
    */
  def parseExpr(c: String): ParseResult[SyExprAll] = {
    val res = parseAll(eqExpr, c)
//    res match {
//      case Success(result, _) =>
//        println(s"[Parser] $c  -->  ${hprog.backend.Show.pp(result)}")
//      case _ =>
//    }
    res
  }

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

  lazy val sols: Parser[SySolution] =
    "["~>solsIn<~"]" |
      eqExpr ^^ (f => Map(""->f))

  lazy val solsIn: Parser[SySolution] =
    eqDef ~ opt("," ~> solsIn) ^^ {
      case e ~ None => e
      case e1 ~ Some(e2) => e1.keySet.intersect(e2.keySet).toList match {
        case Nil => e1 ++ e2
        case shared => throw new ParserException(s"Variable(s) defined more than once: ${
          shared.mkString(",")}")
      }
    }
  lazy val eqDef: Parser[SySolution] =
    identifier ~ "(_t_) == " ~ eqExpr ^^ {
      case id ~ _ ~ exp => Map(id->exp)
    }

  lazy val eqExpr: Parser[SyExprAll] =
    prod ~ opt(("+"~eqExpr)|("-"~negEqExpr)) ^^ {
      case e ~ None => e
      case e1 ~ Some("+"~e2) => SAdd(e1,e2)
      case e1 ~ Some("-"~e2) => SAdd(e1,e2)
      case _ ~ Some(s~_) => throw new ParserException(s"Unknown operator $s")
    }

  // negate first part of a sum
  lazy val negEqExpr: Parser[SyExprAll] =
    prod ~ opt(("+"~eqExpr)|("-"~negEqExpr)) ^^ {
      case e ~ None => invert(e)
      case e1 ~ Some("+"~e2) => SAdd(invert(e1),e2)
      case e1 ~ Some("-"~e2) => SAdd(invert(e1),e2)
      case _ ~ Some(s~_) => throw new ParserException(s"Unknown operator $s")
    }


  lazy val prod: Parser[SyExprAll] =
    expn ~ opt(("*"~prod)|("/"~prod)) ^^ {
      case e ~ None => e
      case e1 ~ Some("*"~e2) => SMult(e1,e2)
      case e1 ~ Some("/"~e2) => SDiv(e1,e2)
      case _ ~ Some(s~_) => throw new ParserException(s"Unknown operator $s")
    }

  lazy val expn: Parser[SyExprAll] =
    opt("-") ~ "e" ~ "^" ~ lit ^^ {
      case None~_~_~e => SFun("exp",List(e))//(t:Double) => (ctx:Valuation) => Math.exp(e(t)(ctx))
      case _~_~_~e => invert(SFun("exp",List(e)))//(t:Double) => (ctx:Valuation) => Math.exp(e(t)(ctx))
    } |
    lit ~ opt("^"~>lit) ^^ {
      case e ~ None => e
      case e1 ~ Some(e2) => SPow(e1,e2)//(t:Double) => (ctx:Valuation) => Math.pow(e1(t)(ctx),e2(t)(ctx))
    }

  lazy val lit: Parser[SyExprAll] =
    function | rational | time | "("~>eqExpr<~")" | negation

  lazy val negation: Parser[SyExprAll] =
    "-"~>lit ^^ (e => SSub(SVal(0.0),e)) // (e => (t: Double) => (ctx: Valuation) => -e(t)(ctx))
  def time: Parser[SyExprTime] =
    "_t_" ^^^ SArg() // ((t:Double)=>(_:Valuation)=>t)
  lazy val rational: Parser[SyExprAll] =
    float~opt("/"~>float) ^^ {
      case f~None => f
      case f~Some(f2) => SDiv(f,f2) // (_:Double) => (_:Valuation) => f / mbf.getOrElse(1.0)
    }
  lazy val float: Parser[SyExprAll] =
    """-?[0-9]+(\.([0-9]+))?(e-?([0-9]+))?""".r ^^ { s: String => SVal(s.toDouble) }

  lazy val function: Parser[SyExprAll] =
    identifier~opt("("~>eqExprs<~")") ^^ {
      case name~Some(arg) =>
        SFun(name,arg)
      case name~_ =>
        SVar(name)
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
  lazy val eqExprs: Parser[List[SyExprAll]] =
    eqExpr ~ opt(","~>eqExprs) ^^ {
      case e~None => List(e)
      case e~Some(e2) => e::e2
    }

  def invert(e: SyExprAll): SyExprAll =
    e match {
      case SVal(v) =>  SVal(-v)
      case _:SArg     => SMult(SVal(-1),e)
      case _:SVar     => SMult(SVal(-1),e)
      case SFun(_, _) => SMult(SVal(-1),e)
      case SPow(_, _) => SMult(SVal(-1),e)
      case SDiv(e1, e2)  => SDiv(invert(e1),e2)
      case SRes(e1, e2)  => SRes(invert(e1),e2)
      case SMult(e1, e2) => SMult(invert(e1),e2)
      case SAdd(e1, e2)  => SAdd(invert(e1),invert(e2))
      case SSub(e1, e2)  => SSub(e2,e1)
    }

  }

