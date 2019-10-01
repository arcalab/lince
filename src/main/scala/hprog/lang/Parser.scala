package hprog.lang

import hprog.ast._
import hprog.common.ParserException
import hprog.frontend.Utils

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

  /**
    * Main function that parses a string into a Condition.
    *
    * @param c string representing the condition
    * @return Parse result (parsed(cond) or failure(error))
    */
  def parseCond(c: String): ParseResult[Cond] = parseAll(condP, c)

  //  def pexp(c:String): ParseResult[Cond] = parseAll(condP,c)

  override def skipWhitespace = true

  override val whiteSpace: Regex = "( |\t|\r|\f|\n|//.*)+".r
//  override val whiteSpace: Regex = "[ \t\r\f\n]+".r
  val identifier: Parser[String] = """[a-z][a-zA-Z0-9_]*""".r
  val identifierCap: Parser[String] = """[a-zA-Z][a-zA-Z0-9_]*""".r
  val nameP: Parser[String] = "[a-zA-Z0-9.-_!$]+".r

  val skip = Atomic(Nil,DiffEqs(Nil,For(Value(0))))
  //   ///////////////
  //   /// Program ///
  //   ///////////////

  lazy val progP: Parser[Syntax] =
    seqP ^^ { stx =>
      Utils.isClosed(stx) match {
        case Left(msg) => throw new ParserException(msg)
        case Right(_) =>
          // println(s"parsed ${stx}")
          stx
      }
    }

  lazy val seqP: Parser[Syntax] =
    basicProg ~ opt(";" ~> seqP) ^^ {
      case p1 ~ Some(p2) => p1 ~ p2
      case p ~ None => p
    }

  lazy val basicProg: Parser[Syntax] =
    "skip" ~> opt("for"~>realP) ^^ {
      case None => skip
      case Some(real) => Atomic(Nil,DiffEqs(Nil,For(Value(real))))
    } |
    "while" ~> whileGuard ~ "{" ~ seqP ~ "}" ^^ {
      case c ~ _ ~ p ~ _ => While(skip, c, p)
    } |
    "repeat" ~> intPP ~ "{" ~ seqP ~ "}" ^^ {
      case c ~ _ ~ p ~ _ => While(skip, Counter(c), p)
    } |
    "if" ~> condP ~ "then" ~ blockP ~ "else" ~ blockP ^^ {
      case c ~ _ ~ p1 ~ _ ~ p2 => ITE(c, p1, p2)
    } |
    "wait"~>realP ^^ {
      time:Double => Atomic(Nil,DiffEqs(Nil,For(Value(time))))
    }|
    atomP

  lazy val blockP: Parser[Syntax] =
    "{"~>seqP<~"}" |
    basicProg

  lazy val whileGuard: Parser[LoopGuard] = {
    condP ^^ Guard |
    intPP ^^ Counter
  }

//  lazy val atomP: Parser[At] =
//    identifier ~ ":=" ~ linP ^^ {
//      case v ~ _ ~ l => Assign(Var(v), l)
//    } |
//      diffEqsP ~ opt("&" ~> durP) ^^ {
//        case des ~ d => des & d.getOrElse(Forever)
//      }

  lazy val atomP: Parser[Atomic] =
    identifier ~ ":=" ~ linP ^^ {
      case v ~ _ ~ l => Atomic(List(Assign(Var(v), l)),DiffEqs(Nil,For(Value(0))))
    } |
    diffEqsP ~ opt(durP) ^^ {
      case des ~ d => Atomic(Nil,des & d.getOrElse(Forever))
    }


  lazy val diffEqsP: Parser[DiffEqs] =
    identifier ~ "'" ~ "=" ~ linP ~ opt("," ~> diffEqsP) ^^ {
      case v ~ _ ~ _ ~ l ~ Some(eqs) => DiffEqs(List(Var(v) ^= l), Forever) & eqs
      case v ~ _ ~ _ ~ l ~ None => DiffEqs(List(Var(v) ^= l), Forever)
    }

  lazy val durP: Parser[Dur] =
    "until" ~ opt(untilArgs) ~ condP ^^ {
      case _ ~ None ~ cond => Until(cond,0.01,None)
      case _ ~ Some(args) ~ cond => Until(cond,args._1,args._2)
    } |
    "for" ~> realP ^^ Value.andThen(For)

  lazy val untilArgs:Parser[(Double,Option[Double])] =
    "_"~realP~opt(","~>realP) ^^ {
      case _~eps~jump => (eps,jump)
    }
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
    }: PartialFunction[String ~ Option[Var=>Cond],Cond],
    {
      case e ~ _ => s"Not a condition: $e"
    })

  lazy val bcontP: Parser[Var => Cond] =
    "<=" ~> linP ^^ (e2 => (e1: Var) => e1 <= e2) |
    ">=" ~> linP ^^ (e2 => (e1: Var) => e1 >= e2) |
    "<" ~> linP ^^ (e2 => (e1: Var) => e1 < e2) |
    ">" ~> linP ^^ (e2 => (e1: Var) => e1 > e2) |
    "==" ~> linP ^^ (e2 => (e1: Var) => e1 === e2)


  lazy val realP: Parser[Double] =
    """-?[0-9]+(\.([0-9]+))?""".r ^^ { s: String => s.toDouble }

  lazy val intPP: Parser[Int] =
    "("~>intP<~")" |
    intP
  lazy val intP: Parser[Int] =
    """[0-9]+""".r ^^ { s: String => s.toInt }

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

