package hprog.lang

import hprog.ast._
import hprog.ast.SymbolicExpr.SyExprVar
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
  val identifier: Parser[String] = """[a-z][a-zA-Z0-9_]*""".r
  val identifierCap: Parser[String] = """[a-zA-Z][a-zA-Z0-9_]*""".r
  val nameP: Parser[String] = "[a-zA-Z0-9.-_!$]+".r

  val skip = Atomic(Nil,DiffEqs(Nil,For(Value(0))))

  //   ///////////////
  //   /// Program ///
  //   ///////////////
  /** Parser for a program that checks if the program is closed before returning. */
  lazy val progP: Parser[Syntax] =
    seqP ^^ { stx =>
      Utils.isClosed(stx) match {
        case Left(msg) => throw new ParserException(msg)
        case Right(_) => stx
      }
    }

  /** Parser for a sequence of programs */
  lazy val seqP: Parser[Syntax] =
    basicProg ~ opt(seqP) ^^ {
      case p1 ~ Some(p2) => p1 ~ p2
      case p ~ None => p
    }

  /** Parser for a basic program: "skip", "while", "repeat", "if", "wait", or an atomic program (see below) */
  lazy val basicProg: Parser[Syntax] =
    "skip" ~> opt("for"~>realP) <~ ";" ^^ {
      case None => skip
      case Some(real) => Atomic(Nil,DiffEqs(Nil,For(Value(real))))
    } |
    "while" ~> whileGuard ~ "do" ~ "{" ~ seqP ~ "}" ^^ {
      case c ~ _ ~ _ ~ p ~ _ => While(skip, c, p)
    } |
    "repeat" ~> intPP ~ "{" ~ seqP ~ "}" ^^ {
      case c ~ _ ~ p ~ _ => While(skip, Counter(c), p)
    } |
    "if" ~> condP ~ "then" ~ blockP ~ "else" ~ blockP ^^ {
      case c ~ _ ~ p1 ~ _ ~ p2 => ITE(c, p1, p2)
    } |
    ("wait"~>linP) <~ ";" ^^ {
      time => Atomic(Nil,DiffEqs(Nil,For(time)))
    }|
    atomP

  /** parser of a program wrapped with curly brackets or a basic program */
  lazy val blockP: Parser[Syntax] =
    "{"~>seqP<~"}" |
    basicProg

  /** Parser for the guard of a while loop (a condition of an integer) */
  lazy val whileGuard: Parser[LoopGuard] = {
    condP ^^ Guard |
    intPP ^^ Counter
  }

  /** Parser for an atomic program: an assignment or a set of diff equations. */
  lazy val atomP: Parser[Atomic] =
    (identifier ~ ":=" ~ linP) <~ ";" ^^ {
      case v ~ _ ~ l => Atomic(List(Assign(Var(v), l)),DiffEqs(Nil,For(Value(0))))
    } |
    (diffEqsP ~ opt(durP)) <~ ";" ^^ {
      case des ~ d => Atomic(Nil,des & d.getOrElse(Forever))
    }

  /** Parser for  differential equations */
  lazy val diffEqsP: Parser[DiffEqs] =
    identifier ~ "'" ~ "=" ~ linP ~ opt("," ~> diffEqsP) ^^ {
      case v ~ _ ~ _ ~ l ~ Some(eqs) => DiffEqs(List(Var(v) ^= l), Forever) & eqs
      case v ~ _ ~ _ ~ l ~ None => DiffEqs(List(Var(v) ^= l), Forever)
    }

  /** Parser for the duration part ("until" or "for") after the differential equations */
  lazy val durP: Parser[Dur] =
    "until" ~ opt(untilArgs) ~ condP ^^ {
      case _ ~ None ~ cond => Until(cond,None /*0.01*/,None)
      case _ ~ Some(args) ~ cond => Until(cond,Some(args._1),args._2)
    } |
    "for" ~> linP ^^ For
             //realP ^^ Value.andThen(For)

  /** Parser for the arguments of an "until" block (after diff equations)  */
  lazy val untilArgs:Parser[(Double,Option[Double])] =
    "_"~realP~opt(","~>realP) ^^ {
      case _~eps~jump => (eps,jump)
    }

  //////////

  /** Parser for a linear integer expression */
  lazy val linP: Parser[Lin] =
    linParcelP ~opt(("+"~>linP)|("-"~>negLinP)) ^^ {
      case l1~Some(l2) => l1+l2
      case l1~_        => l1
    }
  private lazy val negLinP: Parser[Lin] =
    linParcelP ~opt(("+"~>linP)|("-"~>negLinP)) ^^ {
      case l1~Some(l2) => invert(l1)+l2
      case l1~_        => invert(l1)
  }

  /** Parser for a parcel (element being added/subtracted) */
  lazy val linParcelP: Parser[Lin] =
    "-"~>linMultP ^^ invert |
    linMultP


//  lazy val linP: Parser[Lin] =
//    opt("-") ~ linLitP ~ opt(linContP) ^^ {
//      case sign ~ l1 ~ Some(l2) => mbInvert(sign, l1) + l2
//      case sign ~ l1 ~ _        => mbInvert(sign, l1)
//    }
//  // r*var | r*(..) | var*r | (..)*r | r | var | (...)

  /** Parser for the multiplication of atomic values (real, variables, or
    * linear expressions), guaranteeing the result is a linear expression:
    * multiplications must have a real in one of its sides. */
  lazy val linMultP: Parser[Lin] = //...r | r*linAt | var*r |
    realP ~ opt("*"~>linAtP) ^^ {
      case r ~ Some(l) => Mult(Value(r),l)
      case r ~ _ => Value(r)
    } |
    linAtP ~ opt("*"~>realP) ^^ {
      case l ~ Some(r) => Mult(Value(r),l)
      case l ~ _ => l
    }
  /** Atomic linear expression is a variable or a linear expression */
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

  /** Parser for a boolean expression with variables as a possible conjunction of `disjP` */
  lazy val condP: Parser[Cond] =
    disjP ~ opt("/\\" ~> condP) ^^ {
      case e1 ~ Some(e2) => e1 && e2
      case e1 ~ None => e1
    }

  /** Parser for a (possible) disjunction of equivalences */
  lazy val disjP: Parser[Cond] =
    equivP ~ opt("\\/" ~> disjP) ^^ {
      case e1 ~ Some(e2) => e1 || e2
      case e1 ~ None => e1
    }

  /** Parser for a (possible) equivalence (via "<->") of simpler conditions */
  lazy val equivP: Parser[Cond] =
    negP ~ opt("<->" ~> equivP) ^^ {
      case e1 ~ Some(e2) => e1 || e2
      case e1 ~ None => e1
    } //|
  //     "("~equivP~")" ^^ { case _~e~_ => e }
  /** Parser of a (possibly negated) block of a condition or a literal or inequality `bopP` */
  lazy val negP: Parser[Cond] =
    "!" ~ "(" ~> condP <~ ")" ^^ Not |
      //    "!"~>litP ^^ Not |
      "(" ~> condP <~ ")" |
      bopP

  //  lazy val litP: Parser[Cond] =
  //    identifierOrBool
  //
  /** Parser for a literal (true/false/var) or an inequality (var<=exp) */
  lazy val bopP: Parser[Cond] =
    identifier ~ opt(bcontP) ^? ( {
      case "true" ~ None => BVal(true)
      case "false" ~ None => BVal(false)
      case e ~ Some(co) => co(Var(e))
    }: PartialFunction[String ~ Option[Var=>Cond],Cond],
    {
      case e ~ _ => s"Not a condition: $e"
    })

  /** Parser for an inequality of (linear) integer expressions */
  lazy val bcontP: Parser[Var => Cond] =
    "<=" ~> linP ^^ (e2 => (e1: Var) => e1 <= e2) |
    ">=" ~> linP ^^ (e2 => (e1: Var) => e1 >= e2) |
    "<" ~> linP ^^ (e2 => (e1: Var) => e1 < e2) |
    ">" ~> linP ^^ (e2 => (e1: Var) => e1 > e2) |
    "==" ~> linP ^^ (e2 => (e1: Var) => e1 === e2) |
    "!=" ~> linP ^^ (e2 => (e1: Var) => Not(e1 === e2))


  /** Parsr for a real number */
  lazy val realP: Parser[Double] =
    """-?[0-9]+(\.([0-9]+))?""".r ^^ { s: String => s.toDouble }

  /** Parser for an integer number, possibly with parenthesis */
  lazy val intPP: Parser[Int] =
    "("~>intP<~")" |
    intP
  /** Parser for an integer number  */
  lazy val intP: Parser[Int] =
    """[0-9]+""".r ^^ { s: String => s.toInt }

//  def intExpr: Parser[SyExprVar] =
//    SageParser.eqExpr ^^ { Utils.asSyExprVar } // assuming it has no variable "_t_"

  /** Auxiliary: function that negates a (linear) integer expression */
  private def invert(lin: Lin): Lin = lin match {
    case Var(v) =>  Mult(Value(-1),Var(v))
    case Value(v) => Value(-v)
    case Add(l1, l2) => Add(invert(l1),invert(l2))
    case Mult(Value(v), l) => Mult(Value(-v),l)
  }
  /** Auxiliary: experimental function that also negates a (linear)
    * integer expression, possibly avoiding negation.   */
  private def mbInvert(sign:Option[_],lin:Lin): Lin =
    if (sign.isDefined) invert(lin) else lin
}

