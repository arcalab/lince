package hprog.lang

import hprog.ast._

import scala.util.matching.Regex
import scala.util.parsing.combinator._

/**
  * Parser for Hybrid Programs, using parsing combinators.
  * Created by jose on 19/07/2018.
  */
object Parser extends RegexParsers {

  /**
    * Main function that parses a string.
    *
    * @param c string representing a program
    * @return Parse result (parsed(connector) or failure(error))
    */
  def parse(c: String): ParseResult[Prog] = parseAll(progP, c)

  //  def pexp(c:String): ParseResult[Cond] = parseAll(condP,c)

  override def skipWhitespace = true

  override val whiteSpace: Regex = "[ \t\r\f\n]+".r
  val identifier: Parser[String] = """[a-z][a-zA-Z0-9_]*""".r
  val identifierCap: Parser[String] = """[a-zA-Z][a-zA-Z0-9_]*""".r
  val nameP: Parser[String] = "[a-zA-Z0-9.-_!$]+".r

  //   ///////////////
  //   /// Program ///
  //   ///////////////

  lazy val progP: Parser[Prog] =
    basicProg ~ opt(";" ~> progP) ^^ {
      case p1 ~ Some(p2) => p1 ~ p2
      case p ~ None => p
    }

  lazy val basicProg: Parser[Prog] =
    "skip" ^^ (_ => Skip) |
      "while" ~> condP ~ "{" ~ progP ~ "}" ^^ {
        case c ~ _ ~ p ~ _ => While(c, p)
      } |
      "if" ~> condP ~ "then" ~ progP ~ "else" ~ progP ^^ {
        case c ~ _ ~ p1 ~ _ ~ p2 => ITE(c, p1, p2)
      } |
      atomP

  lazy val atomP: Parser[At] =
    identifier ~ ":=" ~ linP ^^ {
      case v ~ _ ~ l => Assign(Var(v), l)
    } |
      diffEqsP ~ opt("&" ~> durP) ^^ {
        case des ~ d => des & d.getOrElse(Forever)
      }

  lazy val diffEqsP: Parser[DiffEqs] =
    identifier ~ "=" ~ linP ~ opt("," ~> diffEqsP) ^^ {
      case v ~ _ ~ l ~ Some(eqs) => DiffEqs(List(Var(v) ^= l), Forever) & eqs
      case v ~ _ ~ l ~ None => DiffEqs(List(Var(v) ^= l), Forever)
    }

  lazy val durP: Parser[Dur] =
    condP ^^ When |
      realP ^^ Value.andThen(For)

  lazy val linP: Parser[Lin] =
    identifier ~ opt(linVarContP) ^^ {
      case v ~ f => f.getOrElse((x: Var) => x)(Var(v))
    } |
      realP ~ opt(linValContP) ^^ {
        case v ~ f => f.getOrElse((x: Value) => x)(Value(v))
      } |
      "(" ~> linP <~ ")"

  lazy val linVarContP: Parser[Var => Lin] =
    "+" ~> linP ^^ { l => v: Var => v + l } |
      "*" ~> realP ^^ { r => v: Var => Value(r) * v }
  lazy val linValContP: Parser[Value => Lin] =
    "+" ~> linP ^^ { l => v: Value => v + l } |
      "*" ~> linP ^^ { l => v: Value => v * l }

  lazy val condP: Parser[Cond] =
    disjP ~ opt("/\\" ~> condP) ^^ {
      case e1 ~ Some(e2) => e1 && e2
      case e1 ~ None => e1
    }

  lazy val disjP: Parser[Cond] =
    equivP ~ opt("\\/" ~> disjP) ^^ {
      case e1 ~ Some(e2) => e1 || e2
      case e1 ~ None => e1
    }

  lazy val equivP: Parser[Cond] =
    negP ~ opt("<->" ~> equivP) ^^ {
      case e1 ~ Some(e2) => e1 || e2
      case e1 ~ None => e1
    } //|
  //     "("~equivP~")" ^^ { case _~e~_ => e }
  lazy val negP: Parser[Cond] =
    "!" ~ "(" ~> condP <~ ")" ^^ Not |
      //    "!"~>litP ^^ Not |
      "(" ~> condP <~ ")" |
      bopP

  //  lazy val litP: Parser[Cond] =
  //    identifierOrBool
  //
  lazy val bopP: Parser[Cond] =
    identifier ~ opt(bcontP) ^? ( {
      case "true" ~ None => BVal(true)
      case "false" ~ None => BVal(false)
      case e ~ Some(co) => co(Var(e))
    }, {
      case e ~ None => s"Not a condition: $e"
    })

  lazy val bcontP: Parser[Var => Cond] =
    "<=" ~> linP ^^ (e2 => (e1: Var) => e1 <= e2) |
    ">=" ~> linP ^^ (e2 => (e1: Var) => e1 >= e2) |
    "<" ~> linP ^^ (e2 => (e1: Var) => e1 < e2) |
    ">" ~> linP ^^ (e2 => (e1: Var) => e1 > e2) |
    "==" ~> linP ^^ (e2 => (e1: Var) => e1 === e2)


  lazy val realP: Parser[Double] =
    """[0-9]+(\.([0-9]+))?""".r ^^ { s: String => s.toDouble }
}

