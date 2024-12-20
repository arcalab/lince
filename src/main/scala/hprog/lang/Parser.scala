
package hprog.lang

import hprog.ast._
import Syntax._
import hprog.ast.SymbolicExpr.SyExprVar
import hprog.common.ParserException
import hprog.frontend.Utils

import scala.util.matching.Regex
import scala.util.parsing.combinator._
import scala.math._

/**
  * Parser for Hybrid Programs, using parsing combinators.
  * Created by Ricardo and José in 31/1/23.
  */


// It will take a set of characters and return the corresponding Syntax
object Parser extends RegexParsers {

  /**
    * Main function that parses a string.
    *
    * @param c string representing a program
    * @return Parse result (parsed(connector) or failure(error))
    */
    def parse(c: String): ParseResult[Syntax] = {
      variables = List.empty[String]  
      initialValues = Map.empty[String, List[Expr]]
      parseAll(progP, c)
    }
  /**
    * Main function that parses a string into a Condition.
    *
    * @param c string representing the condition
    * @return Parse result (parsed(cond) or failure(error))
    */
    def parseCond(c: String): ParseResult[Cond] = {
      variables = List.empty[String]  
      initialValues = Map.empty[String, List[Expr]]
      parseAll(condP, c)
    }


  override def skipWhitespace = true

  override val whiteSpace: Regex = "( |\t|\r|\f|\n|//.*)+".r
  val identifier: Parser[String] = """[a-z][a-zA-Z0-9_]*""".r
  val identifierCap: Parser[String] = """[a-zA-Z][a-zA-Z0-9_]*""".r
  val nameP: Parser[String] = "[a-zA-Z0-9.-_!$]+".r

  val skip = Atomic(Nil, DiffEqs(Nil, For(Value(0))))  
  var initialValues: Map[String, List[Expr]] = Map()
  var variables: List[String] = List()

  //   ///////////////
  //   /// Program ///
  //   ///////////////

  /** Parser for a program that checks if the program is closed before returning. */


  lazy val progP: Parser[Syntax] =   
    declr ^^ { stx =>
      Utils.isClosed(stx) match {
        case Left(msg) => throw new ParserException(msg)
        case Right(_) => {
          Syntax.GetSyntax.addParsedSyntax(stx)
          Syntax.GetSyntax.getInitialValues(initialValues)
          variables = List.empty[String]
          initialValues = Map()
          //var aux:Map[String,NotLin]=Map()
          //var x=Utils.updateSyntax(stx,aux,0,Utils.extractVarsDifEqs(stx),0)
          //x._1
          stx          
        }
      }
    }




  //lazy val progP: Parser[Syntax] = seqP


  // Parser to obligate program to have a atomic/s (declarations of variables) followed by instructions
  lazy val declr: Parser[Syntax] =
    atomP ~ opt(seqP) ^^ {
      case a ~ Some(n) => a ~ n
      case a ~ None => a
    }


  // instructions or sequence of instructions
  lazy val seqP: Parser[Syntax] =
    basicProg ~ opt(seqP) ^^ {
      case p1 ~ Some(p2) => p1 ~ p2
      case p ~ None => p
    }

  /** Parser for a basic program: "skip", "while", "repeat", "if", "wait", or an atomic program (see below) */
  lazy val basicProg: Parser[Syntax] =
    "skip" ~> opt("for" ~> realP) <~ ";" ^^ {
      case None => skip
      case Some(real) => Atomic(Nil, DiffEqs(Nil, For(Value(real))))
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
      ("wait" ~> exprP) <~ ";" ^^ {
        time => Atomic(Nil, DiffEqs(Nil, For(time)))
      } |
      atomP


  /** parser of a program wrapped with curly brackets or a basic program */
  lazy val blockP: Parser[Syntax] =
    "{" ~> seqP <~ "}" |
      basicProg

  /** Parser for the guard of a while loop (a condition of an integer) */
  lazy val whileGuard: Parser[LoopGuard] = {
    condP ^^ Guard |
      intPP ^^ Counter
  }

  // Parser for array values
  lazy val arrayP: Parser[List[Expr]] =
    "[" ~> repsep(exprP, ",") <~ "]" |
    ("[" ~> intP) ~ (".." ~> intP <~ "]") ^^ {
      case i1 ~ i2 => (for (x<-i1 to i2) yield Value(x)).toList
    }

  /*/** Parser for an atomic program: an assignment or a set of diff equations. */
  lazy val atomP: Parser[Atomic] =
    (identifier ~ ":=" ~ (notlinP | arrayP)) <~ ";" ^^ {
      case v ~ _ ~ l => l match {
        case list: List[NotLin] =>
          if (!variables.contains(v)) {
            val error = s"""The attribution for the variavle ${v} with values: ${list} is done in the wrong place"""
            throw new Exception(error)
          }
          initialValues += ("_" + v -> list)
          Atomic(List(Assign(Var("_" + v), list.head)), DiffEqs(Nil, For(Value(0))))
        case expr: NotLin =>
          if (!variables.contains(v)) {
            variables =  variables + List(v)
          }
          Atomic(List(Assign(Var("_" + v), expr)), DiffEqs(Nil, For(Value(0))))
      }
    } |
    (diffEqsP ~ opt(durP)) <~ ";" ^^ {
      case des ~ d => Atomic(Nil, des & d.getOrElse(Forever))
  }
  */
  lazy val atomP: Parser[Atomic] =
  (identifier ~ ":=" ~ (exprP | arrayP)) <~ ";" ^^ {
    case v ~ _ ~ l => l match {
      case listE: List[_] =>
        if (variables.contains(v)) {
          val error = s"""The assignment for the variable $v with values: $listE is done in the wrong place"""
          throw new Exception(error)
        }
        val list: List[Expr] = listE.asInstanceOf[List[Expr]] // list must be an expression, since exprP/arrayP are
        variables = variables :+ v
        initialValues += ("_" + v -> list)
        Atomic(List(Assign(Var("_" + v), list.head)), DiffEqs(Nil, For(Value(0))))

      case expr: Expr =>
        if (!variables.contains(v)) {
          variables = variables :+ v
        }
        Atomic(List(Assign(Var("_" + v), expr)), DiffEqs(Nil, For(Value(0))))
    }
  } |
  (diffEqsP ~ opt(durP)) <~ ";" ^^ {
    case des ~ d => Atomic(Nil, des & d.getOrElse(Forever))
  }



  /** Parser for  differential equations */
  lazy val diffEqsP: Parser[DiffEqs] =
    identifier ~ "'" ~ "=" ~ exprP ~ opt("," ~> diffEqsP) ^^ {
      case v ~ _ ~ _ ~ l ~ Some(eqs) => DiffEqs(List(Var("_" + v) ^= l), Forever) & eqs
      case v ~ _ ~ _ ~ l ~ None => DiffEqs(List(Var("_" + v) ^= l), Forever)
    }

  /** Parser for the duration part ("until" or "for") after the differential equations */
  lazy val durP: Parser[Dur] =
    "until" ~ opt(untilArgs) ~ condP ^^ {
      case _ ~ None ~ cond => Until(cond, None /*0.01*/ , None)
      case _ ~ Some(args) ~ cond => Until(cond, Some(args._1), args._2)
    } |
      "for" ~> exprP ^^ For

  /** Parser for the arguments of an "until" block (after diff equations) */
  lazy val untilArgs: Parser[(Double, Option[Double])] =
    "_" ~ realP ~ opt("," ~> realP) ^^ {
      case _ ~ eps ~ jump => (eps, jump)
    }






  ///////////////////// non linear expressions /////////////////////////////

  lazy val exprP: Parser[Expr] =
    summandP ~ opt(("+" ~> exprP) | ("-" ~> negExprP)) ^^ {
      case l1 ~ Some(l2) => l1 + l2
      case l1 ~ _ => l1

    }

  private lazy val negExprP: Parser[Expr] =
    summandP ~ opt(("+" ~> exprP) | ("-" ~> negExprP)) ^^ {
      case l1 ~ Some(l2) => invertExpr(l1) + l2
      case l1 ~ _ => invertExpr(l1)
    }

  lazy val summandP: Parser[Expr] =
    "-" ~> multP ^^ invertExpr |
      multP


  lazy val multP: Parser[Expr] =
    divP ~ opt("*" ~> multP) ^^ {
      case l1 ~ Some(l2) => Mult(l1, l2)
      case l1 ~ None => l1

    }

  lazy val divP: Parser[Expr] =
    remainderP ~ opt("/" ~> divP) ^^ {
      case l1 ~ Some(l2) => Div(l1, l2)
      case l1 ~ None => l1

    }


  lazy val remainderP: Parser[Expr] =
    exponE ~ opt("%" ~> remainderP) ^^ {
      case l1 ~ Some(l2) => Res(l1, l2)
      case l1 ~ None => l1

    }

  /*
  lazy val notlinAtP: Parser[NotLin] =
    "pi" ~ "(" ~ ")" ^^ {
      case _ ~ _ ~ _=> FuncNotLin("PI",List())
    }|
    "e" ~ "(" ~ ")" ~ opt("^" ~> notlinOthers)^^ {
      case _ ~ _ ~ _ ~ None => FuncNotLin("E",List())
      case _ ~ _ ~ _ ~ Some(l1) => FuncNotLin("exp",List(l1))
    }|
    "pow" ~ "(" ~ notlinOthers ~ "," ~ notlinOthers ~ ")" ^^{
      case _ ~ _ ~ l1 ~ _ ~ l2 ~ _=> PowNotLin(l1,l2)
    }|
     notlinOthers ~ opt("^" ~>  notlinOthers)  ^^{
      case l1 ~ Some(l2) => PowNotLin(l1,l2)
      case l1 ~ _ => l1
    }

  lazy val notlinOthers: Parser[NotLin]=
    realP ^^ {
      ValueNotLin
    }|
    identifier ~ opt("("~>argsFunction<~")") ^^{
      case s ~ Some(arguments) => FuncNotLin(s,arguments)
      case s ~ _ => VarNotLin (s)
    }|
    "("~>notlinP<~")" ^^ {
      case l => l
    }
*/


  lazy val exponE: Parser[Expr] =
    literalP ~ "^" ~ literalP ^^ {
      case l1 ~ _ ~ l2 => Func("pow",List(l1, l2))
    } |
      literalP


  lazy val literalP: Parser[Expr] =
    "pi" ~ "(" ~ ")" ~ opt("^" ~> literalP) ^^ {
      case _ ~ _ ~ _ ~ None => Func("PI", List()) //mitigate numetical errors
      case _ ~ _ ~ _ ~ Some(l1) => Func("pow",List(Func("PI", List()), l1))
    } |
    "e" ~ "(" ~ ")" ~ opt("^" ~> literalP) ^^ {
      case _ ~ _ ~ _ ~ None =>Func("E", List()) //mitigate numetical errors
      case _ ~ _ ~ _ ~ Some(l1) => Func("exp", List(l1))
    } |
    "pow" ~ "(" ~ exprP ~ "," ~ exprP ~ ")" ^^ {
      case _ ~ _ ~ l1 ~ _ ~ l2 ~ _ => Func("pow",List(l1, l2))
    } |
    realP ^^ {
      Value
    } |
     identifier ~ "("~")" ^^ {
      case s ~ _ ~ _  => Func(s, List())
    } |
    identifier ~ opt("(" ~> argsFunction <~ ")") ^^ {
      case s ~ Some(arguments) => Func(s, arguments)
      case s ~ _ => Var("_" + s)
    } |
    "(" ~> exprP <~ ")" ^^ {
      case l => l
    }


  lazy val argsFunction: Parser[List[Expr]] =
    exprP ~ opt("," ~> argsFunction) ^^ {
      case n ~ Some(ns) => n :: ns
      case n ~ _ => List(n)
    }




  /*
  ////////// linear expression ///////////////
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
  lazy val linMultP: Parser[Lin] =
    seqRealIdentP

  lazy val seqRealIdentP: Parser[Lin]=
    seqRealIdentATP ~ opt("*"~>seqRealIdentP) ^^{
      case s ~ None => s
      case s ~ Some(l) => Mult(s,l)
    }
  lazy val seqRealIdentATP:Parser[Lin]=
    realP ^^ Value|
    identifier ^^ Var|
    "("~>linP<~")"
*/


  /*
  /** Parser for the multiplication of atomic values (real, variable, or
    * linear expressions), guaranteeing the result is a linear expression:
    * multiplications must have a real (just a real number or a result of an expression) in one of its sides, or in both. */
  lazy val linMultP: Parser[Lin] =
    opt(seqRealP <~ "*")  ~  linAtP ~ opt(seqMultDiv) ^^ {
      case None  ~ l ~ None => l
      case Some(s1) ~ l ~ None => Mult(Value(s1),l)
      case None  ~ l ~ Some(s1) => Mult(Value(s1),l)
      case Some(s1) ~ l ~ Some(s2) => Mult(Value(s1*s2),l)
    }

  /** Atomic linear expression is a variable, a real (just a real number or a result of an expression) or a linear expression */
  lazy val linAtP: Parser[Lin] =
    identifier ^^ Var |
    realP ^^ Value |
    "("~>linP<~")"
    //realP ^^ Value |
// Sequence of a multiplication/division of a real expressions in the right side of the atomic values
lazy val seqMultDiv:Parser[Double]=
  "*" ~ reallinAtP ~ opt(seqMultDiv) ^^ {
    case _ ~ r ~ None => r
    case _ ~ r ~ Some(s) => r*s
  } |
  "/" ~ reallinAtP ~ opt(seqMultDiv) ^^ {
    case _ ~ r ~ None => 1/r
    case _ ~ r ~ Some(s) => (1/r)*s
  } |
  "*" ~ "(" ~ realLinP ~ ")" ~ opt(seqMultDiv) ^^ {
    case _ ~ _ ~ r ~ _ ~ None => r
    case _ ~ _ ~ r ~ _ ~ Some(s)  => r*s
  } |
  "/" ~ "(" ~ realLinP ~ ")" ~ opt(seqMultDiv) ^^ {
    case _ ~ _ ~ r ~ _ ~ None => 1/r
    case _ ~ _ ~ r ~ _ ~ Some(s)  => (1/r)*s
  }
// // Sequence of a multiplication/division of a real expressions in the left side of the atomic values
lazy val seqRealP:Parser[Double]=
   reallinAtP ~ opt(seqMultDiv) ^^ {
    case r ~ None => r
    case r ~ Some(s) => r*s
   }
// Parser of real expressions
lazy val realLinP: Parser[Double]=
  reallinParcelP ~opt(("+"~>realLinP)|("-"~>realnegLinP)) ^^ {
  case l1~Some(l2) => l1+l2
  case l1~_        => l1
}
private lazy val realnegLinP: Parser[Double] =
reallinParcelP ~opt(("+"~>realLinP)|("-"~>realnegLinP)) ^^ {
  case l1~Some(l2) => -l1+l2
  case l1~_        => -l1
}
lazy val reallinParcelP: Parser[Double] =
  "-"~>reallinMultP ^^ {
   case r => -r
  }|
  reallinMultP
lazy val reallinMultP: Parser[Double] =
    reallinDivP ~ opt("*"~>reallinMultP) ^^ {
      case l1 ~ Some(l2) => l1*l2
      case l1 ~ None => l1

    }
  lazy val reallinDivP: Parser[Double] =
    reallinResP ~ opt("/"~>reallinDivP) ^^ {
      case l1 ~ Some(l2) => l1/l2
      case l1 ~ None => l1

    }
  lazy val reallinResP: Parser[Double] =
    reallinAtP ~ opt("%"~>reallinResP) ^^ {
      case l1 ~ Some(l2) => l1%l2
      case l1 ~ None => l1

    }


 // atomics of the real expressions
  lazy val reallinAtP: Parser[Double] =
    "pi" ~ "(" ~ ")" ^^ {
      case _ ~ _ ~ _=> math.Pi
    }|
    opt("-") ~ "e" ~ "(" ~ ")" ~ opt("^" ~> reallinOthers)^^ {
      case None ~ _ ~ _ ~ _ ~ None => math.E
      case None ~ _ ~ _ ~ _ ~ Some(l1) => math.exp(l1)
      case Some(_) ~ _ ~ _ ~ _ ~ None => - math.E
      case Some(_) ~ _ ~ _ ~ _ ~ Some(l1) => - math.exp(l1)
    }|
    opt("-") ~ "pow" ~ "(" ~ reallinOthers ~ "," ~ reallinOthers ~ ")" ^^{
      case None ~ _ ~ _ ~ l1 ~ _ ~ l2 ~ _=> math.pow(l1,l2)
      case Some(_) ~ _ ~ _ ~ l1 ~ _ ~ l2 ~ _=> - math.pow(l1,l2)
    }|
     opt("-") ~ reallinOthers ~ opt("^" ~>  reallinOthers)  ^^{
      case None ~ l1 ~ Some(l2) => math.pow(l1,l2)
      case None ~ l1 ~ None => l1
      case Some(_) ~ l1 ~ None => -l1
      case Some(_) ~ l1 ~ Some(l2) => -math.pow(l1,l2)
    }
  lazy val reallinOthers: Parser[Double]=
    realP|
    "sin" ~ "(" ~ realLinP ~ ")" ^^ {
      case _ ~ _ ~ r ~ _ => math.sin(r)
    }|
    "cos" ~ "(" ~ realLinP ~ ")" ^^ {
      case _ ~ _ ~ r ~ _ => math.cos(r)
    }|
    "tan" ~ "(" ~ realLinP ~ ")" ^^ {
      case _ ~ _ ~ r ~ _ => math.tan(r)
    }|
    "arcsin" ~ "(" ~ realLinP ~ ")" ^^ {
      case _ ~ _ ~ r ~ _ => math.asin(r)
    }|
    "arccos" ~ "(" ~ realLinP ~ ")" ^^ {
      case _ ~ _ ~ r ~ _ => math.acos(r)
    }|
    "arctan" ~ "(" ~ realLinP ~ ")" ^^ {
      case _ ~ _ ~ r ~ _ => math.atan(r)
    }|
    "sinh" ~ "(" ~ realLinP ~ ")" ^^ {
      case _ ~ _ ~ r ~ _ => math.sinh(r)
    }|
    "cosh" ~ "(" ~ realLinP ~ ")" ^^ {
      case _ ~ _ ~ r ~ _ => math.cosh(r)
    }|
    "tanh" ~ "(" ~ realLinP ~ ")" ^^ {
      case _ ~ _ ~ r ~ _ => math.tanh(r)
    }|
    "sqrt" ~ "(" ~ realLinP ~ ")" ^^ {
      case _ ~ _ ~ r ~ _ => math.sqrt(r)
    }|
    "log" ~ "(" ~ realLinP ~ ")" ^^ {
      case _ ~ _ ~ r ~ _ => math.log(r)
    }|
    "log10" ~ "(" ~ realLinP ~ ")" ^^ {
      case _ ~ _ ~ r ~ _ => math.log10(r)
    }|
    "max" ~ "(" ~ realLinP ~ "," ~ realLinP ~ ")" ^^ {
      case _ ~ _ ~ r1 ~ _ ~ r2 ~ _ => math.max(r1,r2)
    }|
    "min" ~ "(" ~ realLinP ~ "," ~ realLinP ~ ")" ^^ {
      case _ ~ _ ~ r1 ~ _ ~ r2 ~ _ => math.min(r1,r2)
    }|
    "exp" ~ "(" ~ realLinP ~ ")" ^^ {
      case _ ~ _ ~ r ~ _ => math.exp(r)
    }|
    "("~>realLinP<~")" ^^ {
      case l => l
    }
  */

  //////////////////// Conditions //////////////////////////////////////////

  lazy val condP: Parser[Cond] =
    disjP ~ opt("&&" ~> condP) ^^ {
      case e1 ~ Some(e2) => e1 && e2
      case e1 ~ None => e1
    }

  /** Parser for a (possible) disjunction of equivalences */
  lazy val disjP: Parser[Cond] =
    equivP ~ opt("||" ~> disjP) ^^ {
      case e1 ~ Some(e2) => e1 || e2
      case e1 ~ None => e1
    }


  /** Parser for a (possible) equivalence (via "<->") of simpler conditions */
  lazy val equivP: Parser[Cond] =
    negCondP ~ opt("<=>" ~> equivP) ^^ {
      case e1 ~ Some(e2) => e1 || e2
      case e1 ~ None => e1
    }
  /** Parser of a (possibly negated) block of a condition or a literal or inequality `bopP` */
  lazy val negCondP: Parser[Cond] =
    "!" ~ "(" ~> condP <~ ")" ^^ Not |
      bopP |
      "(" ~> condP <~ ")" |
      condP


  lazy val bopP: Parser[Cond] =
    "true" ^^ {
      case _ => BVal(true)
    } |
      "false" ^^ {
        case _ => BVal(false)
      } |
      exprP ~ "<=" ~ exprP ^^ {
        case l1 ~ _ ~ l2 => LE(l1, l2)
      } |
      exprP ~ ">=" ~ exprP ^^ {
        case l1 ~ _ ~ l2 => GE(l1, l2)
      } |
      exprP ~ "<" ~ exprP ^^ {
        case l1 ~ _ ~ l2 => LT(l1, l2)
      } |
      exprP ~ ">" ~ exprP ^^ {
        case l1 ~ _ ~ l2 => GT(l1, l2)
      } |
      exprP ~ "==" ~ exprP ^^ {
        case l1 ~ _ ~ l2 => EQ(l1, l2)
      } |
      exprP ~ "!=" ~ exprP ^^ {
        case l1 ~ _ ~ l2 => Not(EQ(l1, l2))
      }

  /** Parsr for a real number */
  lazy val realP: Parser[Double] =
    """-?[0-9]+(\.([0-9]+))?""".r ^^ { s: String => s.toDouble }

  /** Parser for an integer number, possibly with parenthesis */
  lazy val intPP: Parser[Int] =
    "(" ~> intP <~ ")" |
      intP
  /** Parser for an integer number */
  lazy val intP: Parser[Int] =
    """-?[0-9]+""".r ^^ { s: String => s.toInt }

  lazy val histP: Parser[(Cond,Either[Double,Int])] =
    ("histogram" ~> ":" ~> condP) ~ ("@"~>whenHistP).? ^^ {
      case c ~ Some(lim) => (c,lim)
      case c ~ None => (c,Right(33))
    }
  lazy val whenHistP: Parser[Either[Double,Int]] =
    "every" ~> realP   ^^ { Left(_) } |
      intP <~ "times" ^^ {Right(_)}
  /*
  /** Auxiliary: function that negates a (linear) integer expression */
  private def invert(lin: Lin): Lin = lin match {
    case Var(v) =>  Mult(Value(-1),Var(v))
    case Value(v) => Value(-v)
    case Add(l1, l2) => Add(invert(l1),invert(l2))
    case Mult(l1, l2) => Mult(Mult(Value(-1),l1),l2) //new
  }
*/


  /** Auxiliary: function that negates a (non linear) integer expression */
  private def invertExpr(lin: Expr): Expr = lin match {
    case Var(v) => Mult(Value(-1), Var(v))
    case Value(v) => Value(-v)
    case Add(l1, l2) => Add(invertExpr(l1), invertExpr(l2))
    case Mult(l1, l2) => Mult(invertExpr(l1), l2)
    case Div(l1, l2) => Div(invertExpr(l1), l2)
    case Res(l1, l2) => Res(invertExpr(l1), l2)
    //case Pow(l1, l2) => MultNotLin(ValueNotLin(-1), PowNotLin(l1, l2))
    case Func(s, ns) => Mult(Value(-1), Func(s, ns))
  }


  /*
  /** Auxiliary: experimental function that also negates a (linear)
    * integer expression, possibly avoiding negation.   */
  private def mbInvert(sign:Option[_],lin:Lin): Lin =
    if (sign.isDefined) invert(lin) else lin
    */
}