
package hprog.lang

import hprog.ast._
import Syntax._
import hprog.ast.SymbolicExpr.SyExprVar
import hprog.common.ParserException
import hprog.frontend.Utils

import scala.util.matching.Regex
import scala.util.parsing.combinator._

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
  def parse(c: String): ParseResult[Syntax] = parseAll(progP, c)

  /**
    * Main function that parses a string into a Condition.
    *
    * @param c string representing the condition
    * @return Parse result (parsed(cond) or failure(error))
    */
  def parseCond(c: String): ParseResult[Cond] = parseAll(condP, c)


  override def skipWhitespace = true

  override val whiteSpace: Regex = "( |\t|\r|\f|\n|//.*)+".r  
  val identifier: Parser[String] = """[a-z][a-zA-Z0-9_]*""".r
  val identifierCap: Parser[String] = """[a-zA-Z][a-zA-Z0-9_]*""".r
  val nameP: Parser[String] = "[a-zA-Z0-9.-_!$]+".r

  val skip = Atomic(Nil,DiffEqs(Nil,For(ValueNotLin(0))))  

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
 
 


//lazy val progP: Parser[Syntax] = seqP


// Parser to obligate program to have a atomic/s (declarations of variables) followed by instructions
  lazy val seqP: Parser[Syntax] =
    atomP ~ opt(nextInstructions) ^^{
     case a ~ Some(n) => a ~ n
     case a ~ None => a
    }
 

// instructions or sequence of instructions
  lazy val nextInstructions: Parser[Syntax]=
     basicProg ~ opt(nextInstructions) ^^ {
      case p1 ~ Some(p2) => p1 ~ p2
      case p ~ None => p
    } 
    
  /** Parser for a basic program: "skip", "while", "repeat", "if", "wait", or an atomic program (see below) */
   lazy val basicProg: Parser[Syntax] =
    "skip" ~> opt("for"~>realP)<~";" ^^ { 
      case None => skip
      case Some(real) => Atomic(Nil,DiffEqs(Nil,For(ValueNotLin(real)))) 
    } |
    "while" ~> whileGuard ~ "do" ~ "{" ~ nextInstructions ~ "}" ^^ {
      case c ~ _ ~ _ ~ p ~ _ => While(skip, c, p) 
    } |
    "repeat" ~> intPP ~ "{" ~ nextInstructions ~ "}" ^^ { 
      case c ~ _ ~ p ~ _ => While(skip, Counter(c), p)
    
    } |
    "if" ~> condP ~ "then" ~ blockP ~ "else" ~ blockP ^^ {
      case c ~ _ ~ p1 ~ _ ~ p2 => ITE(c, p1, p2)
    } |
    ("wait"~>notlinP)<~";" ^^ {
      time => Atomic(Nil,DiffEqs(Nil,For(time)))
    }|
    atomP
    

  /** parser of a program wrapped with curly brackets or a basic program */
  lazy val blockP: Parser[Syntax] =
    "{"~>nextInstructions<~"}" |
    basicProg

  /** Parser for the guard of a while loop (a condition of an integer) */
  lazy val whileGuard: Parser[LoopGuard] = {
    condP ^^ Guard |
    intPP ^^ Counter
  }

  /** Parser for an atomic program: an assignment or a set of diff equations. */
  lazy val atomP: Parser[Atomic] =
    (identifier ~ ":=" ~ notlinP)<~";" ^^ {
      case v ~ _ ~ l => Atomic(List(Assign(VarNotLin(v), l)),DiffEqs(Nil,For(ValueNotLin(0))))
    } |
    (diffEqsP ~ opt(durP))<~";" ^^ {
      case des ~ d => Atomic(Nil,des & d.getOrElse(Forever))
    } |
    durP <~";" ^^{
      case d => Atomic(Nil,DiffEqs(Nil,d)) // upgrate
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
    "for" ~> notlinP ^^ For

  /** Parser for the arguments of an "until" block (after diff equations)  */
  lazy val untilArgs:Parser[(Double,Option[Double])] =
    "_"~realP~opt(","~>realP) ^^ {
      case _~eps~jump => (eps,jump)
    }






///////////////////// non linear expressions /////////////////////////////

  lazy val notlinP:Parser[NotLin]=
    notlinParcelP ~opt(("+"~>notlinP)|("-"~>negnotLinP)) ^^ {
      case l1~Some(l2) => l1+l2
      case l1~_        => l1

   }

  private lazy val negnotLinP: Parser[NotLin] =
    notlinParcelP ~opt(("+"~>notlinP)|("-"~>negnotLinP)) ^^ {
      case l1~Some(l2) => invertNotLin(l1)+l2
      case l1~_        => invertNotLin(l1)
  }

  lazy val notlinParcelP: Parser[NotLin] =
    "-"~>notlinMultP ^^ invertNotLin |
    notlinMultP


  lazy val notlinMultP: Parser[NotLin] = 
    notlinDivP ~ opt("*"~>notlinMultP) ^^ {
      case l1 ~ Some(l2) => MultNotLin(l1,l2)
      case l1 ~ None => l1
      
    } 

  lazy val notlinDivP: Parser[NotLin] = 
    notlinResP ~ opt("/"~>notlinDivP) ^^ {
      case l1 ~ Some(l2) => DivNotLin(l1,l2)
      case l1 ~ None => l1
      
    }


  lazy val notlinResP: Parser[NotLin] = 
    notlinAtP ~ opt("%"~>notlinResP) ^^ {
      case l1 ~ Some(l2) => ResNotLin(l1,l2)
      case l1 ~ None => l1
      
    } 

    
 

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



  lazy val argsFunction: Parser[List[NotLin]] =
    notlinP ~ opt("," ~> argsFunction) ^^{
      case n ~ Some(ns) => n::ns
      case n ~ _ => List(n)
    }


  
   

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



  /** Parser for the multiplication of atomic values (real, variables, or
    * linear expressions), guaranteeing the result is a linear expression:
    * multiplications must have a real in one of its sides. */
  lazy val linMultP: Parser[Lin] = //...r | r*linAt | var*r |
    realP ~ opt("*"~>linAtP) ^^ {
      case r ~ Some(l) => Mult(Value(r),l)
      case r ~ _ => Value(r)
    } |
    linAtP ~ opt(("*" ~ realP) | ("/" ~ realP)) ^^ {
      case l ~ Some("*"~r) => Mult(Value(r),l)
      case l ~ Some("/"~r) => Mult(Value(1/r),l)
      case l ~ _ => l
    } 
  
  /** Atomic linear expression is a variable or a linear expression */
  lazy val linAtP: Parser[Lin] =
    identifier ^^ Var |
    "("~>linP<~")"


    
  
  
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
    negP ~ opt("<=>" ~> equivP) ^^ {
      case e1 ~ Some(e2) => e1 || e2
      case e1 ~ None => e1
    } 
  /** Parser of a (possibly negated) block of a condition or a literal or inequality `bopP` */
  lazy val negP: Parser[Cond] =
    "!" ~ "(" ~> condP <~ ")" ^^ Not |
    bopP |
    "("~>condP<~")"|
    condP 
   





  lazy val bopP: Parser[Cond]=
    "true" ^^ {
      case _ => BVal(true) 
    } |
    "false" ^^ {
      case _ => BVal(false) 
    } |
    notlinP ~ "<=" ~ notlinP ^^ {
      case l1 ~ _ ~ l2 => LE(l1,l2)
    } |
    notlinP ~ ">=" ~ notlinP ^^ {
      case l1 ~ _ ~ l2 => GE(l1,l2)
    } |
    notlinP ~ "<" ~ notlinP ^^ {
      case l1 ~ _ ~ l2 => LT(l1,l2)
    } |
    notlinP ~ ">" ~ notlinP ^^ {
      case l1 ~ _ ~ l2 => GT(l1,l2)
    } |
    notlinP ~ "==" ~ notlinP ^^ {
      case l1 ~ _ ~ l2 => EQ(l1,l2)
    } |
    notlinP ~ "!=" ~ notlinP ^^ {
      case l1 ~ _ ~ l2 => Not(EQ(l1,l2))
    } 

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


  /** Auxiliary: function that negates a (linear) integer expression */
  private def invert(lin: Lin): Lin = lin match {
    case Var(v) =>  Mult(Value(-1),Var(v))
    case Value(v) => Value(-v)
    case Add(l1, l2) => Add(invert(l1),invert(l2))
    case Mult(Value(v), l) => Mult(Value(-v),l)
  }
    


/** Auxiliary: function that negates a (non linear) integer expression */
  private def invertNotLin(lin: NotLin): NotLin = lin match {
    case VarNotLin(v) =>  MultNotLin(ValueNotLin(-1),VarNotLin(v))
    case ValueNotLin(v) => ValueNotLin(-v)
    case AddNotLin(l1, l2) => AddNotLin(invertNotLin(l1),invertNotLin(l2))
    case MultNotLin(l1, l2) => MultNotLin(invertNotLin(l1),l2)
    case DivNotLin(l1,l2) => DivNotLin(invertNotLin(l1),l2)
    case ResNotLin(l1,l2) => ResNotLin(invertNotLin(l1),l2)
    case PowNotLin(l1,l2) => MultNotLin(ValueNotLin(-1),PowNotLin(l1,l2))
    case FuncNotLin(s,ns) => MultNotLin(ValueNotLin(-1),FuncNotLin(s,ns)) 
  }


  /** Auxiliary: experimental function that also negates a (linear)
    * integer expression, possibly avoiding negation.   */
  private def mbInvert(sign:Option[_],lin:Lin): Lin =
    if (sign.isDefined) invert(lin) else lin
}

