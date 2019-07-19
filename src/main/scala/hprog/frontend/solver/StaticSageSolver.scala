package hprog.frontend.solver

import hprog.ast.SageExpr.SExpr
import hprog.ast._
import hprog.backend.Show
import hprog.common.ParserException
import hprog.frontend.Semantics.{SageSolution, Warnings}
import hprog.frontend.{Eval, Utils}
import hprog.lang.SageParser


class StaticSageSolver extends Solver {

  type DiffCache = (SageSolution,String) // one SageExpr for each variable
  type ExprCache = (SExpr,String) // simplified version

  protected var cache:
    Map[List[DiffEq], DiffCache]  =  Map(Nil->(Map(),""))
  protected var cacheVal:
    Map[SExpr    , ExprCache]  =  Map()
  protected var cacheStr:    Map[String,DiffCache] = Map()
  protected var cacheStrVal: Map[String,ExprCache] = Map()
  protected var warnings: Warnings = Nil

  override def toString: String =
    s"""
       |diffEqs:
       |${cache.map(kv => " - "+
          kv._1.map(Show(_)).mkString(", ") + " ->\n"+showCache(kv._2)).mkString("\n")}
       |values:
       |${cacheVal.map(kv => " - "+
          Show(kv._1)+" ->\n    <"+Show(kv._2._1)+">\n    "+kv._2._2).mkString("\n")}
       |diffEqsStr:
       |${cacheStr.map(kv => " - "+
          kv._1 + " ->\n"+showCache(kv._2)).mkString("\n")}
       |valuesStr:
       |${cacheStrVal.map(kv => " - "+
          kv._1+" ->\n    <"+Show(kv._2._1)+">\n    "+kv._2._2).mkString("\n")}
     """.stripMargin

  protected def showCache(sols: DiffCache): String =
    "    "+
    sols._1.mkString("<",",",">")  + "\n    " + sols._2


  def getWarnings: Warnings = warnings

  def +=(w:(SExpr,String)): Unit = warnings ::= w

  /**
    * Throw an error if a system of equations was not precomputed
    * @param eqs system of equations
    */
  def +=(eqs:List[DiffEq]): Unit =
    if (!cache.contains(eqs) && !cacheStr.contains(Show(eqs)))
      throw new LiveSageSolver.SolvingException(s"Static solver failed: unknown equations '" +
        Show(eqs) +
//        eqs.map(Show(_)).mkString("&")+
        "' - known: "+cache.keys.map(eqs => Show(eqs)).mkString(" / ") +
        " - also known: "+cacheStr.keys.mkString(" / "))
  //      val instr = SageSolver.genSage(eqs)
  //      cache += (eqs -> (s"$path/sage -c $instr".!!))
  //      println(s"- adding ${eqs.map(Show(_)).mkString(",")} -> ${cache(eqs)}")

  /**
    * Throw an error if an expression was not precomputed
    * @param expr SageExpression to check if it is in cache
    */
  def +=(expr: SExpr): Unit =
    if (!cacheVal.contains(expr) && !cacheStrVal.contains(Show(expr))) {
      //      throw new LiveSageSolver.SolvingException(
      //        s"Static solver failed: unknown expr: $expr." +
      //          "' - known: "+cacheVal.keys.map(ex => Show(ex)).mkString(" / ") +
      //          " - also known: "+cacheStrVal.keys.mkString(" / "))
      val est = Eval(expr)
      println(s"static solver failed. Using estination $est instead.")
    }



  override def solveSymb(expr: SExpr): SExpr = {
    this += expr
    cacheVal.get(expr) match {
      case Some(c) => c._1
      case None => cacheStrVal.get(Show(expr)) match {
        case Some(value) => value._1
        case None => SVal(Eval(expr))
      }
    }
  }
  def solveSymb(eqs:List[DiffEq]): SageSolution = {
    this += eqs
    cache.get(eqs) match {
      case Some(c) => c._1
      case None => cacheStr(Show(eqs))._1
    }
  }



  def ++=(systems: List[List[DiffEq]]): Unit = {
    systems.foreach(+=)
//    val filtered = systems.filterNot(cache.contains)
//    if (filtered.nonEmpty)
//      throw new SageSolver.SolvingException(s"Static solver failed: unknown equations " +
//        filtered.map(_.map(Show(_)).mkString("&")).mkString(" and "))
  }

  def ++=(syntax:Syntax): Unit = {
    ++=(Utils.getDiffEqs(syntax))
  }


  /**
    * Import the reply from Sage from evaluating an expression
    */
  def importExpr(expr:SExpr, sageReply:String): Unit =
    if (!cacheVal.contains(expr))
      cacheVal += expr -> replyToExprCache(sageReply)

  def importExprStr(expr:String, sageReply: String): Unit =
    if (!cacheStrVal.contains(expr))
     cacheStrVal += expr -> replyToExprCache(sageReply)

  private def replyToExprCache(sageReply:String): ExprCache = {
    val resParsed = SageParser.parseExpr(sageReply)
    resParsed match {
      case SageParser.Success(newExpr, _) =>
        (Eval.update(newExpr,SVal(0),Map()),sageReply)
      case _: SageParser.NoSuccess =>
        throw new ParserException(s"Failed to parse Sage reply '$sageReply'.")
    }
  }


  /**
    * Import the reply from Sage from solving a system of equations
    */
  // e.g., add "x'=y, y'=2" and ["x(_t_)=sin(_t_)",...]
  def importDiffEqs(eqs:List[List[DiffEq]],
                    sageReply:Iterable[String]): Unit =
    for ((eqs, res) <- eqs.zip(sageReply)) {
      //debug(()=>s"- adding  ${eqs} -> $res")
      importDiffEqs(eqs,res)
    }

  def importDiffEqs (eqs:List[DiffEq], sageReply:String): Unit =
    cache = cache + (eqs ->
      replyToDiffCache(Solver.getVars(eqs).filterNot(_.startsWith("_")),sageReply))

  def importDiffEqs(eqs:String, sageReply:String): Unit =
    cacheStr = cacheStr + (eqs ->
      replyToDiffCache(findVars(eqs),sageReply))

  private def findVars(str: String) = {
    //    """Var\([a-zA-Z0-9_]*\)""".r.findAllIn(str).map(_.drop(4).dropRight(1)).toList
    val res1 = """[a-z][a-zA-Z0-9_]*\(0\)""".r.findAllIn(str).map(_.dropRight(3)).toList
    val res2 = """[a-z][a-zA-Z0-9_]*'""".r.findAllIn(str).map(_.dropRight(1)).toList
    val res = res1++res2
    //debug(()=>s"### finding variables in $str - found $res")
    res
  }



  def replyToDiffCache(vars:List[String], sageReply:String): DiffCache = {
    if (sageReply.nonEmpty) {
      val resParsed = SageParser.parse(sageReply) match {
        // single solution - name is not known from the answer of Sage
        case SageParser.Success(sol, _) if sol.keySet == Set("") =>
//          val vars = Solver.getVars(eqs).filterNot(_.startsWith("_"))
          vars match {
            case List(variable) => Map(variable -> sol(""))
            case _ => throw new ParserException(s"Failed to parse $sageReply - " +
              s"only one variable expected, but found [${vars.mkString(",")}].")
          }
        // if more than one variable exists, all is good
        case SageParser.Success(result, _) => result
        case _: SageParser.NoSuccess => throw new ParserException(s"Failed to parse '$sageReply'.")
      }
      (resParsed,sageReply)
    }
    else
      (Map(),"")
  }

  /**
    * Build a string with all replies from Sage, after sorting by key
    * @return
    */
  def exportAll: String =
    (cache-Nil)
//      .toList.sortWith((p1,p2)=>lt(p1._1,p2._1))
      .map(it => Show(it._1) +"§"+it._2._2).mkString("§§") +
    cacheStr
      .map(it => it._1+"§"+it._2._2).mkString("§§") +
    "§§§" +
    cacheVal
//      .toList.sortWith((e1,e2)=>lt(e1._1,e2._1))
      .map(it => Show(it._1)+"§"+it._2._2).mkString("§§") +
    cacheStrVal
      .map(it => it._1+"§"+it._2._2).mkString("§§") +
    "§§§" +
     warnings.map(kv => s"${Show(kv._1)}§${kv._2}").mkString("§§")


  //  private def lt(eqs1:List[DiffEq],eqs2:List[DiffEq]): Boolean =
//    eqs1.mkString < eqs2.mkString
//  private def lt(e1:SageExpr,e2:SageExpr): Boolean =
//    e1.toString < e2.toString

  /**
    * Loads replies compiled with "exportAll", knowing the equations and expressions
    */
  def importAll(replies: String): Unit = {
    replies.split("§§§",3) match {
      case Array(eqsRepl,exprRepl,warns) =>
        val eqsRepl2 = eqsRepl.split("§§")
        for (er <- eqsRepl2)
          er.split("§",2) match {
            case Array("") =>
            case Array(e,r) => importDiffEqs(e,r)
            case _ => throw new ParserException(s"Failed to parse reply '$er' for diff. eqs.")
          }

        val exprRepl2 = exprRepl.split("§§")
        for (er <- exprRepl2)
          er.split("§",2) match {
            case Array(e,r) => importExprStr(e,r)
            case _ => throw new ParserException(s"Failed to parse reply '$er' for sage expr.")
          }

        val warns2 = warns.split("§§")
        for (w <-warns2)
          w.split("§",2) match {
            case Array("") =>
            case Array(t,wn) => this += (Eval.update(hprog.DSL.parseExpr(t),SVal(0),Map()),wn)
            case _ => throw new ParserException(s"Failed to parse reply '$w' for a warning.")
          }
    }
  }

  protected def debug(s:()=>String): Unit = {
    //println("[Solver] " s())
  }

}



