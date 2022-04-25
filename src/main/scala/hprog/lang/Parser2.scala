package hprog.lang

import cats.parse.{LocationMap, Parser => P, Parser0 => P0}
import cats.parse.Numbers._
import cats.syntax.all._
import P._
import cats.data.NonEmptyList
import cats.parse.Rfc5234.sp

import hprog.ast._
import hprog.ast.SymbolicExpr.SyExprVar
import hprog.common.ParserException
import hprog.frontend.Utils

object Parser2 {

  /**
    * Main function that parses a string.
    *
    * @param c string representing a program
    * @return Parse result (parsed(connector) or failure(error))
    */
  def parse(c: String): Either[String,Syntax] = pp(prog,c)

  /**
    * Main function that parses a string into a Condition.
    *
    * @param c string representing the condition
    * @return Parse result (parsed(cond) or failure(error))
    */
  def parseCond(c: String): Either[String,Cond] = pp(condP,c)


  /** Applies a parser to a string, and prettifies the error message */
  def pp[A](parser:P[A],str:String): Either[String,A] =
    parser.parseAll(str) match { //.fold(e=>prettyError(str,e), x=>x)
      case Left(e) => Left(prettyError(str, e))
      case Right(x) => Right(x)
    }
  /** Prettifies an error message */
  def prettyError(str:String,err:Error): String = {
    val loc = LocationMap(str)
    val pos = loc.toLineCol(err.failedAtOffset) match {
      case Some((x, y)) =>
        s"""at ($x,$y):
           |"${loc.getLine(x).getOrElse("-")}"
           |${("-"*(y + 1)) + "^\n"}""".stripMargin
      case _ => ""
    }
    s"${pos}expected: ${err.expected.toList.mkString(", ")}\noffsets: ${
      err.failedAtOffset
    };${err.offsets.toList.mkString(",")}"
  }


  def prog: P[Syntax] =
    command.surroundedBy(sps).map(stx =>
      Utils.isClosed(stx) match {
        case Left(msg) => throw new ParserException(msg)
        case Right(_) =>
          // println(s"parsed ${stx}")
          stx
      }
    )



  /** Parser for a sequence of spaces or comments */
  //// whitespaces
  val whitespace: P[Unit] = //P.charIn(" \t\r\n").void
                            charIn(" \t\r\n").void
  val comment: P[Unit] = string("//") *> P.charWhere(_!='\n').rep0.void
  val sps: P0[Unit] = (whitespace | comment).rep0.void

  /** letters and digits and _ */
  def alphaDigit: P[Char] =
    P.charIn('A' to 'Z') | P.charIn('a' to 'z') | P.charIn('0' to '9') | P.charIn('_')
  /** variable name (starting with lower cap)  */
  def varName: P[String] =
    (charIn('a' to 'z') ~ alphaDigit.rep0).string
  /** symbols */
  def symbols: P[String] = // symbols starting with "--" are meant for syntactic sugar of arrows, and ignored as sybmols of terms
    P.not(string("--")).with1 *>
      oneOf("+-><!%/*=|&".toList.map(char)).rep.string

  /** real number, e.g., 12 or 34.2 */
  def realP: P[Double] =
    (digits ~ (charIn('.')*>digits.map("."+_)).?)
      .map(x=>(x._1+x._2.getOrElse("")).toDouble)
  /** positive integer */
  def intP: P[Int] = digits.map(_.toInt)

  /** (Recursive) Parser for an linear expression */
  def linP: P[Lin] = P.recursive((linRec:P[Lin]) => {
    def lit: P[Lin] =
      char('(') *> linRec.surroundedBy(sps) <* char(')') |
      (char('-') ~ linRec).map(x => Mult(Value(-1),x._2)) |
      realP.map(Value.apply) |
      varName.map(Var.apply)

    def mult: P[(Lin, Lin) => Lin] =
      string("*").as((x:Lin,y:Lin) => (x,y) match {
        case (Value(x), l) => Mult(Value(x), l)
        case (l, Value(x)) => Mult(Value(x), l)
        case _ => sys.error(s"Multiplication [$x * $y] must have at least a fixed number.")
//        case _ => P.failWith(s"Multiplication [$x * $y] must have at least a fixed number.")
      })

    def plusminus: P[(Lin, Lin) => Lin] =
      string("+").as((x:Lin,y:Lin) => Add(x,y)) |
      string("-").as((x:Lin,y:Lin) => Add(x, Mult(Value(-1), y)))

    listSep(listSep(lit, mult), plusminus)
  })


  def durP: P[Dur] =
    (string("for")~sps) *> linP.map(For.apply) |
    (string("until")~(char('_')*>realP~(char(',')*>realP).?).? ~ sps ~ condP)
      .map(x => {
        val eps = x._1._1._2.map(_._1)
        val jump = x._1._1._2.flatMap(_._2)
        Until(x._2,eps,jump)
      })


  /** (Recursive) Parser for a boolean expression */
  def condP: P[Cond] = P.recursive( (bexprRec:P[Cond]) => {
    def lit: P[Cond] = P.recursive( (litR:P[Cond]) =>
      string("true").as(BVal(true)) |
      string("false").as(BVal(false)) |
      (char('!') *> litR).map(Not.apply) |
      ineq.backtrack |
      char('(') *> bexprRec <* char(')')
    )

    def op: P[(Lin, Lin) => Cond] =
    //      string("<=").as((x: Lin, y: Lin) => Or(LT(x, y), EQ(x, y))) |
    //        string(">=").as((x: Lin, y: Lin) => Or(GT(x, y), EQ(x, y))) |
      string("<=").as((x: Lin, y: Lin) => LE(x, y)) |
      string(">=").as((x: Lin, y: Lin) => GE(x, y)) |
      char('<').as((x:Lin,y:Lin) => LT(x,y)) |
      char('>').as((x:Lin,y:Lin) => GT(x,y)) |
      string("==").as((x:Lin,y:Lin) => EQ(x,y)) |
      string("!=").as((x:Lin,y:Lin) => Not(EQ(x,y)))

    def ineq =
      (linP ~ op.surroundedBy(sps) ~ linP).map(x => x._1._2(x._1._1, x._2))

    def or: P[(Cond, Cond) => Cond] =
      (string("||")|string("\\/")).map(_ => Or.apply)

    def and: P[(Cond, Cond) => Cond] =
      (string("&&")|string("/\\")).map(_ => And.apply)

    listSep(listSep(lit, and), or)
  })


  /** (Recursive) Parser for a command in the Lince's while language */
  def command: P[Syntax] = P.recursive((commRec:P[Syntax]) => {
    def basicCommand: P[Syntax] = P.recursive( (basicRec: P[Syntax]) => {
      def ite: P[ITE] =
        (string("if") ~ condP.surroundedBy(sps) ~
          string("then") ~ commBlock.surroundedBy(sps) ~
          string("else") ~ sps ~ commBlock)
          .map(x => ITE(x._1._1._1._1._1._2, x._1._1._1._2, x._2))

      def whilec: P[While] =
        (string("while") ~ condP.surroundedBy(sps) ~
          string("do") ~ sps ~ commBlock)
          .map(x => While(skipComm, Guard(x._1._1._1._2), x._2))

      def repeat: P[While] =
        (string("repeat") ~ intP.surroundedBy(sps) ~
          commBlock) //char('{') ~ commRec.surroundedBy(sps) ~ char('}'))
          .map(x => While(skipComm, Counter(x._1._2), x._2))

      def commBlock =
        char('{') *> commRec.surroundedBy(sps) <* char('}') |
          basicRec

      skip | waitc | ite | whilec | repeat | assign.backtrack | diffEqsP
    })

    def seqOp =
      char(';').as((x:Syntax,y:Syntax)=>x ~ y)

    listSep(basicCommand, seqOp)
  })

  val skipComm: Syntax = Atomic(Nil, DiffEqs(Nil, For(Value(0))))
  def skip: P[Syntax] =
    string("skip").as(skipComm)
  def waitc: P[Syntax] =
    (string("wait")~sps *> linP).map(l => Atomic(Nil,DiffEqs(Nil,For(l))))
  def assign: P[Syntax] = //: P[Assign] =
    (varName ~ string(":=").surroundedBy(sps) ~ linP)
      .map(x => Atomic(List(Assign(Var(x._1._1), x._2)),DiffEqs(Nil,For(Value(0)))))

  def diffEqsP: P[Syntax] =
    (diffEqsCoreP ~ sps ~ durP.?)
      .map(x => Atomic(Nil, x._1._1 & x._2.getOrElse(Forever)))

  def diffEqsCoreP: P[DiffEqs] =
    listSep(diffEqP.map(x=>DiffEqs(List(x),Forever)),
            char(',').as((x:DiffEqs,y:DiffEqs) => DiffEqs(x.eqs:::y.eqs,Forever)))

  def diffEqP: P[DiffEq] =
    (varName ~ char('\'') ~ char('=').surroundedBy(sps) ~ linP)
      .map(x => Var(x._1._1._1) ^= x._2)


  ////// auxiliary ////

  /** Non-empty list of elements with a binary operator */
  def listSep[A](elem:P[A],op:P[(A,A)=>A]): P[A] =
    (elem ~ (op.surroundedBy(sps).backtrack~elem).rep0)
      .map(x=> {
        val pairlist = x._2
        val first = x._1;
        pairlist.foldLeft(first)((rest, pair) => pair._1(rest, pair._2))
      })

}
