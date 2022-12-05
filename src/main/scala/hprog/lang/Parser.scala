
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
  * Created by jose on 19/07/2018.
  */


// Vai pegar num conjunto de caracteres e retornar o Syntax correspondente
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

  override val whiteSpace: Regex = "( |\t|\r|\f|\n|//.*)+".r  // Penso que sirva para o parser interprete como espaços em branco e não processe
  val identifier: Parser[String] = """[a-z][a-zA-Z0-9_]*""".r
  val identifierCap: Parser[String] = """[a-zA-Z][a-zA-Z0-9_]*""".r
  val nameP: Parser[String] = "[a-zA-Z0-9.-_!$]+".r

  val skip = Atomic(Nil,DiffEqs(Nil,For(ValueNotLin(0)))) // deve servir para ignorar 

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
     
  /** Parser for a sequence of programs */
  // Gramática para uma sequeência de programas
  // Podemos ter um basicprog seguido por ponto e virgula e por por outro basicprog, ou nada 
  lazy val seqP: Parser[Syntax] =
    basicProg ~ opt(seqP) ^^ {
      case p1 ~ Some(p2) => p1 ~ p2
      case p ~ None => p
    }

  /** Parser for a basic program: "skip", "while", "repeat", "if", "wait", or an atomic program (see below) */

   
  // COLOCAR O SKIP COM NOTLIN
  lazy val basicProg: Parser[Syntax] =
    "skip" ~> opt("for"~>realP)<~";" ^^ { //penso que o skip sirva para o programa estar sem fazer nada durante o tempo que o skip indica
      case None => skip
      case Some(real) => Atomic(Nil,DiffEqs(Nil,For(ValueNotLin(real)))) // Nil é uma lista vazia
    } |
    "while" ~> whileGuard ~ "do" ~ "{" ~ seqP ~ "}" ^^ {
      case c ~ _ ~ _ ~ p ~ _ => While(skip, c, p) // while sem quaqulquer "pre"
    } |
    "repeat" ~> intPP ~ "{" ~ seqP ~ "}" ^^ { // "Repeat" int { seqp }
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
    "{"~>seqP<~"}" |
    basicProg

  /** Parser for the guard of a while loop (a condition of an integer) */
  lazy val whileGuard: Parser[LoopGuard] = {
    condP ^^ Guard |
    intPP ^^ Counter
  }

  /** Parser for an atomic program: an assignment or a set of diff equations. */
  // Acrescentei a possibilidade de haver algo assim: v:=0; for 0 
  // Assim ele não dá erro caso apanhe algo assim e trata isso devidamente
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
             //realP ^^ Value.andThen(For)

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
    realP ^^ {
      ValueNotLin
    }|
    "sin" ~ "(" ~ notlinP ~ ")" ^^{
      case _ ~ _ ~ l ~ _ => SinNotLin(l)
    }|
    "cos" ~ "(" ~ notlinP ~ ")" ^^{
      case _ ~ _ ~ l ~ _ => CosNotLin(l)
    }|
    "tan" ~ "(" ~ notlinP ~ ")" ^^{
      case _ ~ _ ~ l ~ _ => TanNotLin(l)
    }|
    "pow" ~ "(" ~ notlinP ~ "," ~ notlinP ~ ")" ^^{
      case _ ~ _ ~ l1 ~ _ ~ l2 ~ _=> PowNotLin(l1,l2)
    }|
    "sqrt" ~ "(" ~ notlinP ~ "," ~  notlinP ~ ")" ^^{
      case _ ~ _ ~ l1 ~ _ ~ l2 ~ _ => PowNotLin(l1,DivNotLin(ValueNotLin(1),l2))
    }|
    "("~>notlinP<~")" ^^ {
      case l => l
    }|
    identifier ^^ {
      VarNotLin 
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

/*

  lazy val linMultP: Parser[Lin] = //...r | r*linAt | var*r |
    realP ~ "*" ~ linMultP ^^ {
      case r ~ _ ~ lm => Mult(Value(r),lm) 
    } |
    linMultP ~ "*" ~ realP ^^ {
      case lm ~ _ ~ r => Mult(Value(r),lm) 
    } |
    linMultP ~ "/" ~ realP ^^ {
      case lm ~ _ ~ r => Mult(Value(1/r),lm)
    } |
    identifier ^^ Var |
    realP ^^ Value |
    "("~>linP<~")" 


*/
    
  
  
/*
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
    } |
    linAtP ~ opt("/"~>realP) ^^ {
      case l ~ Some(r) => Mult(Value(1/r),l)
      case l ~ _ => l
    }
  /** Atomic linear expression is a variable or a linear expression */
  lazy val linAtP: Parser[Lin] =
    identifier ^^ Var |
    "("~>linP<~")" 
    
*/







  /*
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
    linrealP ~ "*" ~ linAtP ~ "/" ~ linrealP ^^ {
      case lr1 ~ _ ~ la ~ _ ~ lr2 => Mult(lr1,Div(la,lr2)) 
    } |
    linAtP ~ "/" ~ linrealP ^^ {
      case la ~ _ ~ lr => Div(la,lr)
    } |
    linrealP ~ opt("*"~>linAtP) ~ opt("*"~>linrealP) ^^ {
      case l1 ~ Some(l2) ~ Some(l3) => Mult(Mult(l1,l2),l3)
      case l1 ~ None ~ Some(l3) => Mult(l1,l3)
      case l1 ~ Some(l2) ~ None => Mult(l1,l2)
      case l1 ~ None ~ None => l1
    } | 
    linAtP ~ opt("*"~>linrealP) ^^ {
      case l1 ~ Some(l2) => Mult(l2,l1)
      case l1 ~ None => l1
    } |
    "("~>linP<~")" ^^{
      case l => l
    }


    
  /** Atomic linear expression is a variable or a linear expression */
  lazy val linAtP: Parser[Lin] =
    identifier ^^ Var |
    "("~>linP<~")"



  lazy val linrealP:Parser[Lin]=
   linrealMultP


  lazy val linrealMultP: Parser[Lin] = 
    linrealDivP ~ opt("*"~>linrealMultP) ^^ {
      case l1 ~ Some(l2) => Mult(l1,l2)
      case l1 ~ None => l1
      
    } 

  lazy val linrealDivP: Parser[Lin] = 
    linrealResP ~ opt("/"~>linrealDivP) ^^ {
      case l1 ~ Some(l2) => Div(l1,l2)
      case l1 ~ None => l1
      
    }


  lazy val linrealResP: Parser[Lin] = 
    linrealAtP ~ opt("%"~>linrealResP) ^^ {
      case l1 ~ Some(l2) => Res(l1,l2)
      case l1 ~ None => l1
      
    } 

    
 
  lazy val linrealAtP: Parser[Lin] =
    realP ^^ {
      case r => Value(r)
    }|
    "sin" ~ "(" ~ linrealP ~ ")" ^^{
      case _ ~ _ ~ l ~ _ => Sin(l)
    }|
    "cos" ~ "(" ~ linrealP ~ ")" ^^{
      case _ ~ _ ~ l ~ _ => Cos(l)
    }|
    "tan" ~ "(" ~ linrealP ~ ")" ^^{
      case _ ~ _ ~ l ~ _ => Tan(l)
    }|
    "pow" ~ "(" ~ linrealP ~ "," ~ linrealP ~ ")" ^^{
      case _ ~ _ ~ l1 ~ _ ~ l2 ~ _=> Pow(l1,l2)
    }|
    "sqrt" ~ "(" ~ linrealP ~ "," ~  linrealP ~ ")" ^^{
      case _ ~ _ ~ l1 ~ _ ~ l2 ~ _ => Sqrt(l1,l2)
    }|
    "("~>sumrealP<~")" ^^ {
      case l => l
    }

   

  lazy val sumrealP:Parser[Lin]=
    realParcelP ~opt(("+"~>sumrealP)|("-"~>negreal)) ^^ {
      case l1~Some(l2) => l1+l2
      case l1~_        => l1

   }

  private lazy val negreal: Parser[Lin] =
    realParcelP ~opt(("+"~>sumrealP)|("-"~>negreal)) ^^ {
      case l1~Some(l2) => invert(l1)+l2
      case l1~_        => invert(l1)
  }

  lazy val realParcelP: Parser[Lin] =
    "-"~>linrealMultP ^^ invert |
    linrealMultP


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
  
  // Não percebi este!!!!!!!!!!!!!
  /** Parser for a (possible) equivalence (via "<->") of simpler conditions */
  lazy val equivP: Parser[Cond] =
    negP ~ opt("<=>" ~> equivP) ^^ {
      case e1 ~ Some(e2) => e1 || e2
      case e1 ~ None => e1
    } //|
  //     "("~equivP~")" ^^ { case _~e~_ => e }
  /** Parser of a (possibly negated) block of a condition or a literal or inequality `bopP` */
  lazy val negP: Parser[Cond] =
    "!" ~ "(" ~> condP <~ ")" ^^ Not |
    "("~>condP<~")"|
    bopP |
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
    } /* Não vou colocar isto pois vai tirar disciplina á linguagem se existir|
    notlinP ^^ {
      case l => Not(EQ(l,ValueNotLin(0)))
      //case l => IsoletedCond(l)
    }
    */

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
    /*
    case Div(l1,l2) => Div(invert(l1),l2)
    case Res(l1,l2) => Res(invert(l1),l2)
    case Sin(l1) => Mult(Value(-1),Sin(l1))
    case Cos(l1) => Mult(Value(-1),Cos(l1))
    case Tan(l1) => Mult(Value(-1),Tan(l1))
    case Pow(l1,l2) => Mult(Value(-1),Pow(l1,l2))
    case Sqrt(l1,l2) => Mult(Value(-1),Sqrt(l1,l2))
    */
  


/** Auxiliary: function that negates a (non linear) integer expression */
  private def invertNotLin(lin: NotLin): NotLin = lin match {
    case VarNotLin(v) =>  MultNotLin(ValueNotLin(-1),VarNotLin(v))
    case ValueNotLin(v) => ValueNotLin(-v)
    case AddNotLin(l1, l2) => AddNotLin(invertNotLin(l1),invertNotLin(l2))
    case MultNotLin(l1, l2) => MultNotLin(invertNotLin(l1),l2)
    case DivNotLin(l1,l2) => DivNotLin(invertNotLin(l1),l2)
    case ResNotLin(l1,l2) => ResNotLin(invertNotLin(l1),l2)
    case SinNotLin(l1) => MultNotLin(ValueNotLin(-1),SinNotLin(l1))
    case CosNotLin(l1) => MultNotLin(ValueNotLin(-1),CosNotLin(l1))
    case TanNotLin(l1) => MultNotLin(ValueNotLin(-1),TanNotLin(l1))
    case PowNotLin(l1,l2) => MultNotLin(ValueNotLin(-1),PowNotLin(l1,l2))
    //case SqrtNotLin(l1,l2) => MultNotLin(ValueNotLin(-1),SqrtNotLin(l1,l2))
  }


  /** Auxiliary: experimental function that also negates a (linear)
    * integer expression, possibly avoiding negation.   */
  private def mbInvert(sign:Option[_],lin:Lin): Lin =
    if (sign.isDefined) invert(lin) else lin
}

