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
  def parse(c: String): ParseResult[Syntax] = parseAll(progP, c)

  //  def pexp(c:String): ParseResult[Cond] = parseAll(condP,c)

  override def skipWhitespace = true

  override val whiteSpace: Regex = "[ \t\r\f\n]+".r
  val identifier: Parser[String] = """[a-z][a-zA-Z0-9_]*""".r
  val identifierCap: Parser[String] = """[a-zA-Z][a-zA-Z0-9_]*""".r
  val nameP: Parser[String] = "[a-zA-Z0-9.-_!$]+".r

  //   ///////////////
  //   /// Program ///
  //   ///////////////

  lazy val progP: Parser[Syntax] =
    basicProg ~ opt(";" ~> progP) ^^ {
      case p1 ~ Some(p2) => p1 ~ p2
      case p ~ None => p
    }

  lazy val basicProg: Parser[Syntax] =
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
    condP ^^ Until |
    realP ^^ Value.andThen(For)
//////////

  lazy val linP: Parser[Lin] =
    linParcelP ~opt(("+"~>linP)|("-"~>negLinP)) ^^ {
      case l1~Some(l2) => l1+l2
      case l1~_        => l1
    }
  lazy val negLinP: Parser[Lin] =
    linParcelP ~opt(("+"~>linP)|("-"~>negLinP)) ^^ {
      case l1~Some(l2) => invert(l1)+l2
      case l1~_        => invert(l1)
  }

  lazy val linParcelP: Parser[Lin] =
    "-"~>linMultP ^^ invert |
    linMultP


//  lazy val linP: Parser[Lin] =
//    opt("-") ~ linLitP ~ opt(linContP) ^^ {
//      case sign ~ l1 ~ Some(l2) => mbInvert(sign, l1) + l2
//      case sign ~ l1 ~ _        => mbInvert(sign, l1)
//    }
//  // r*var | r*(..) | var*r | (..)*r | r | var | (...)
  lazy val linMultP: Parser[Lin] = //...r | r*linAt | var*r |
    realP ~ opt("*"~>linAtP) ^^ {
      case r ~ Some(l) => Mult(Value(r),l)
      case r ~ _ => Value(r)
    } |
    linAtP ~ opt("*"~>realP) ^^ {
      case l ~ Some(r) => Mult(Value(r),l)
      case l ~ _ => l
    }
//  // + lin  | - lin
//  lazy  val linContP: Parser[Lin] = // + | -
//    "+" ~> linP |
//    "-" ~> linP ^^ invert
  // var | (lin)
  lazy val linAtP: Parser[Lin] =
    identifier ^^ Var |
    "("~>linP<~")"

/////////////////

  //  lazy val linP: Parser[Lin] =
//    opt("-") ~ identifier ~ opt(linVarContP) ^^ {
//      case sign ~ vr ~ f => mbInvert(sign,f.getOrElse((x: Var) => x)(Var(vr)))
//    } |
//    opt("-") ~ realP ~ opt(linValContP) ^^ {
//      case sign ~ vl ~ f => mbInvert(sign,f.getOrElse((x: Value) => x)(Value(vl)))
//    } |
//    "(" ~> linP <~ ")"
//
//  lazy val linVarContP: Parser[Var => Lin] =
//    "-" ~> linP ^^ { l => v: Var => v + invert(l) } |
//    "+" ~> linP ^^ { l => v: Var => v + l } |
//    "*" ~> realP ^^ { r => v: Var => Value(r) * v }
//  lazy val linValContP: Parser[Value => Lin] =
//    "-" ~> linP ^^ { l => v: Value => v + invert(l) } |
//    "+" ~> linP ^^ { l => v: Value => v + l } |
//    "*" ~> linP ^^ { l => v: Value => v * l }

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
    """-?[0-9]+(\.([0-9]+))?""".r ^^ { s: String => s.toDouble }

  // AUX
  private def invert(lin: Lin): Lin = lin match {
    case Var(v) =>  Mult(Value(-1),Var(v))
    case Value(v) => Value(-v)
    case Add(l1, l2) => Add(invert(l1),invert(l2))
    case Mult(Value(v), l) => Mult(Value(-v),l)
  }
  private def mbInvert(sign:Option[_],lin:Lin): Lin =
    if (sign.isDefined) invert(lin) else lin
}

