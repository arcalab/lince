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
     * @param c string representing a program
     * @return Parse result (parsed(connector) or failure(error))
     */
   def parse(c:String): ParseResult[Progr] = parseAll(progrP,c)
   def pexp(c:String): ParseResult[Expr] = parseAll(exprP,c)

   override def skipWhitespace = true
   override val whiteSpace: Regex = "[ \t\r\f\n]+".r
   val identifier: Parser[String] = """[a-z][a-zA-Z0-9_]*""".r
   val identifierCap: Parser[String] = """[a-zA-Z][a-zA-Z0-9_]*""".r
   val nameP: Parser[String] = "[a-zA-Z0-9.-_!$]+".r

//   /** Parses basic primitives */
//   def inferPrim(s:String): Connector = s match {
//     case "fifo"     => fifo
//     case "fifofull" => fifofull
//     case "drain"    => drain
//     case "id"       => id
//     case "ids"      => lam(n,id^n)
//     case "dupl"     => dupl
//     case "lossy"    => lossy
//     case "merger"   => merger
//     case "swap"     => swap
//     case "writer"   => Prim("writer",Port(IVal(0)),Port(IVal(1)))
//     case "reader"   => Prim("reader",Port(IVal(1)),Port(IVal(0)))
//     case "node"     => SubConnector(s,Repository.node, Nil)
//     case "dupls"    => SubConnector(s,Repository.dupls, Nil)
//     case "mergers"  => SubConnector(s,Repository.mergers, Nil)
//     case "zip"      => SubConnector(s,Repository.zip, Nil)
//     case "unzip"    => SubConnector(s,Repository.unzip, Nil)
//     case "exrouter" => SubConnector(s,Repository.exrouter, Nil)
//     case "exrouters"=> SubConnector(s,Repository.nexrouter, Nil)
//     case "fifoloop" => SubConnector(s,Repository.fifoloop, Nil)
//     case "sequencer"=> SubConnector(s,Repository.sequencer, Nil)
//     case _          => str2conn(s)
//   }



//   ///////////////
//   /// Program ///
//   ///////////////

     def progrP: Parser[Progr] =
       statementP~opt(";"~progrP) ^^ {
         case p1~Some(_~p2) => p1 ~ p2
         case p1~None => p1
       }
     def statementP: Parser[Progr] =
       assignsP~opt("&"~exprP)  ^^ {
         case as~Some(_~e) => Statement(as,Some(e))
         case as~None      => Statement(as,None)
       }

     def assignsP: Parser[List[Assgn]] =
       varP~":="~exprP~opt(","~assignsP) ^^ {
         case v~_~e~Some(_~as) => Assgn(v,e)::as
         case v~_~e~None       => List(Assgn(v,e))
       }

     def varP: Parser[Var] =
       identifier~opt(primesP) ^^ {
         case s~Some(n) => Var(s,n)
         case s~None => Var(s,0)
       }
     def primesP:Parser[Int] =
       "'"~opt(primesP) ^^ {
         case _~None => 1
         case _~Some(n) => 1+n
       }




   ////////////////
   // expression //
   ////////////////


   def exprP: Parser[Expr] = // redundancy to give priority to true/false over variable "true"/"false"
     "(" ~ exprP ~ ")" ^^ {case _ ~ e ~ _ => e } |
//     identifierOrBool |
     conjP

   def identifierOrBool: Parser[Expr] =
     identifier ^^ {
       case "true" => BVal(true)
       case "false" => BVal(false)
       case x => Var(x,0)}

   // boolean expressions
   def conjP: Parser[Expr] =
     disjP ~ opt("&"~conjP) ^^ {
       case e1~Some(_~e2) => e1 && e2
       case e1~None       => e1
     }
   def disjP: Parser[Expr] =
     equivP ~ opt("|"~disjP) ^^ {
       case e1~Some(_~e2) => e1 || e2
       case e1~None       => e1
     }
   def equivP: Parser[Expr] =
     negP ~ opt("<->"~equivP) ^^ {
       case e1~Some(_~e2) => e1 || e2
       case e1~None       => e1
     } //|
//     "("~equivP~")" ^^ { case _~e~_ => e }
   def negP: Parser[Expr] =
     "!"~"("~conjP~")"    ^^ {case _~_~e~_ => Not(e)} |
     "!"~identifierOrBool ^^ {case _~e => Not(e)} |
     compP
   def compP: Parser[Expr] =
     rexpr ~ opt(bcontP) ^^ {
       case e ~ Some(co) => co(e)
       case e ~ None => e
     }
   def bcontP: Parser[Expr=>Expr] =
     "<=" ~ rlit ^^ { case _~e2 => (e1:Expr) => e1 <= e2 } |
     ">=" ~ rlit ^^ { case _~e2 => (e1:Expr) => e1 >= e2 } |
     "<"  ~ rlit ^^ { case _~e2 => (e1:Expr) => e1 < e2 }  |
     ">"  ~ rlit ^^ { case _~e2 => (e1:Expr) => e1 > e2 }  |
     "==" ~ rlit ^^ { case _~e2 => (e1:Expr) => e1 === e2 }

//   def litP: Parser[Expr] =
// //    booleanVal |
//     "!" ~ conjP        ^^ {case _ ~ e => Not(e)}         |
//     identifier~":"~"B" ^^ {case s~_~_ => Var(s,0) } |
//     identifierOrBool ^? ({
//         case be: Expr => be
//       },
//       ie => s"Integer not expected: $ie")       |
//     "(" ~ conjP ~ ")" ^^ {case _ ~ e ~ _ => e }
//

   // arithmetic expressions
   def rexpr: Parser[Expr] =
     rlit ~ rbop ~ rexpr ^^ {case l ~ op ~ r => op(l,r)} |
     rlit
   def rlit: Parser[Expr] =
     realP ^^ Val                            |
     identifierOrBool ~ opt(":"~"R") ^? ({
       case (ie:Expr)~_ => ie
     },{
       case be~_ => s"Boolean not expected: $be"
     }) |
 //  identifier~":"~"I" ^^ {case s~_~_=>Var(s) } |
 //    identifier ^^ Var                           |
     "(" ~ rexpr ~ ")" ^^ {case _ ~ e ~ _ => e }
//   def intP: Parser[Int] =
//     """[0-9]+""".r ^^ { s:String => s.toInt }
  def realP: Parser[Double] =
    """[0-9]+(\.([0-9]+))?""".r ^^ { s:String => s.toDouble }
   def rbop: Parser[(Expr,Expr)=>Expr] =
       "+"  ^^ {_ => Add } |
       "-"  ^^ {_ => Sub } |
       "*"  ^^ {_ => Mul } |
       "/"  ^^ {_ => Div }

}
