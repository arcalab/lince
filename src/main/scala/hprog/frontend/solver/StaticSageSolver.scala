package hprog.frontend.solver

import hprog.ast.SymbolicExpr.{SyExpr, SyExprAll, SyExprTime}
import hprog.ast._
import Syntax._
import hprog.backend.Show
import hprog.common.ParserException
import hprog.frontend.CommonTypes.{SySolution, Valuation, Warnings}
import hprog.frontend.{Eval, Utils}
import hprog.lang.SageParser


class StaticSageSolver extends Solver {

  type DiffCache = (SySolution,String) // one SageExpr for each variable
  type ExprCache = (SyExprAll,String) // simplified version
  type BoolCache = (Boolean,String) // simplified version

  protected var cacheDE:
    Map[List[DiffEq], DiffCache]  =  Map(Nil->(Map(),""))
  protected var cacheVal:
    Map[SyExprAll   , ExprCache]  =  Map()
  protected var cacheBool:
    Map[(Cond,Valuation), BoolCache]  =  Map()
  protected var cacheStrDE:     Map[String,DiffCache] = Map()
  protected var cacheStrVal:  Map[String,ExprCache] = Map()
  protected var cacheStrBool: Map[String,BoolCache] = Map()
  protected var warnings: Warnings = Set()

  override def toString: String =
    s"""
       |diffEqs:
       |${cacheDE.map(kv => " - "+
          kv._1.map(Show(_)).mkString(", ") + " ->\n"+showCache(kv._2)).mkString("\n")}
       |values:
       |${cacheVal.map(kv => " - " +
          Show (kv._1) + " ->\n    <" + Show(kv._2._1) + ">\n    " + kv._2._2).mkString("\n")}
       |booleans:
       |${cacheBool.map(kv => " - "+
          Show(kv._1._1,kv._1._2)+" ->\n    <"+(kv._2._1)+">\n    "+kv._2._2).mkString("\n")}
       |diffEqsStr:
       |${cacheStrDE.map(kv => " - "+
          kv._1 + " ->\n"+showCache(kv._2)).mkString("\n")}
       |valuesStr:
       |${cacheStrVal.map(kv => " - "+
          kv._1+" ->\n    <"+Show(kv._2._1)+">\n    "+kv._2._2).mkString("\n")}
       |boolsStr:
       |${cacheStrBool.map(kv => " - " +
          kv._1 + " ->\n    <" + (kv._2._1) + ">\n    " + kv._2._2).mkString("\n")}
     """.stripMargin

  protected def showCache(sols: DiffCache): String =
    "    "+
    sols._1.mkString("<",",",">")  + "\n    " + sols._2


  def getWarnings: Warnings = warnings

  def addWarning(we:SyExpr, ws:String): Unit = warnings += we->ws
  def addWarnings(wrs:List[(SyExpr, String)]): Unit = warnings ++= wrs

  /**
    * Throw an error if a system of equations was not precomputed
    * @param eqs system of equations
    */
  def +=(eqs:List[DiffEq]): Unit =
    if (!cacheDE.contains(eqs) && !cacheStrDE.contains(Show(eqs)))
      throw new LiveSageSolver.SolvingException(s"Static solver failed: unknown equations '" +
        Show(eqs) +
//        eqs.map(Show(_)).mkString("&")+
        "' - known: "+cacheDE.keys.map(eqs => Show(eqs)).mkString(" / ") +
        " - also known: "+cacheStrDE.keys.mkString(" / "))
  //      val instr = SageSolver.genSage(eqs)
  //      cache += (eqs -> (s"$path/sage -c $instr".!!))
  //      println(s"- adding ${eqs.map(Show(_)).mkString(",")} -> ${cache(eqs)}")

  /**
    * Throw an error if an expression was not precomputed
    * @param expr SageExpression to check if it is in cache
    */
  def +=(expr: SyExprAll): Unit =
    if (!cacheVal.contains(expr) && !cacheStrVal.contains(Show(expr))) {
      val est = Eval(Utils.asSyExpr(expr)) // fails if expr is not a SyExpr
//      println(s"static solver failed for ${Show(expr)}. " +
//        s"Know only ${cacheStrVal.keys.map("'"+_+"'").mkString(", ")}. " +
//        s"Using estimation $est instead.")
      cacheStrVal += (Show(expr) -> ((SVal(est),"")))
    }


  /**
    * Throw an error if a condition was not precomputed
    * @param cond Condition to check if it is in cache
    * @param valua valuation to apply to condition
    */
  def +=(cond: Cond, valua: Valuation): Unit = {
    if (!cacheBool.contains(cond, valua) && !cacheStrBool.contains(Show(cond, valua))) {
      val est = Eval(valua.view.mapValues(Eval(_, 0)).toMap, cond)
//      println(s"static solver failed for '${Show(cond,valua)}'. " +
//      s"Know only ${cacheStrBool.keys.map("'"+_+"'").mkString(", ")}. " +
//      s"Using estimation $est instead.")
      cacheStrBool += Show(cond,valua) -> (est,"")
    }
  }


  override def solveSymb(expr: SyExprAll): SyExprAll = {
    this += expr
    cacheVal.get(expr) match {
      case Some(c) => c._1
      case None => cacheStrVal.get(Show(expr)) match {
        case Some(value) => value._1
        case None => SVal(Eval(Utils.asSyExpr(expr))) // fails if expr is not a SyExpr
      }
    }
  }
  override def solveSymb(eqs:List[DiffEq]): SySolution = {
    this += eqs
    cacheDE.get(eqs) match {
      case Some(c) => c._1
      case None => cacheStrDE(Show(eqs))._1
    }
  }

  override def solveSymb(cond: Cond, valua: Valuation): Boolean = {
    this += (cond,valua)
    cacheBool.get((cond,valua)) match {
      case Some(c) => c._1
      case None => cacheStrBool(Show(cond,valua))._1
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
  def importExpr(expr:SyExprAll, sageReply:String): Unit =
    if (!cacheVal.contains(expr))
      cacheVal += expr -> replyToExprCache(sageReply)

  def importExprStr(expr:String, sageReply: String): Unit =
    if (!cacheStrVal.contains(expr))
     cacheStrVal += expr -> replyToExprCache(sageReply)

  private def replyToExprCache(sageReply:String): ExprCache = {
    val resParsed = SageParser.parseExpr(sageReply)
    resParsed match {
      case SageParser.Success(newExpr, _) =>
        //val fixed = Utils.fixVars(newExpr)
        //(Eval.update(newExpr,SVal(0),Map()),sageReply)
        (newExpr,sageReply)
      case _: SageParser.NoSuccess =>
        throw new ParserException(s"Failed to parse Sage reply '$sageReply'.")
    }
  }

  /**
    * Import the reply from Sage from evaluating a boolean expression
    */
  def importBool(c:Cond, vl: Valuation, sageReply:String): Unit =
    if (!cacheBool.contains(c,vl))
      cacheBool += (c,vl) -> replyToBoolCache(sageReply)


  def importBoolStr(bool:String, sageReply: String): Unit =
    if (!cacheStrBool.contains(bool))
      cacheStrBool += bool -> replyToBoolCache(sageReply)

  private def replyToBoolCache(sageReply:String): BoolCache = {
    sageReply match {
      case "True" => (true,sageReply)
      case "False" => (false,sageReply)
      case _ =>
        throw new ParserException(s"Failed to parse Boolean reply '$sageReply'.")
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
    cacheDE = cacheDE + (eqs ->
      replyToDiffCache(Solver.getVars(eqs).filterNot(_.startsWith("_")),sageReply))

  def importDiffEqs(eqs:String, sageReply:String): Unit =
    cacheStrDE = cacheStrDE + (eqs ->
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
      val resParsed = SageParser.parseSol(sageReply) match {
        // single solution - name is not known from the answer of Sage
        case SageParser.Success(sol, _) if sol.keySet == Set("") =>
//          val vars = Solver.getVars(eqs).filterNot(_.startsWith("_"))
          vars match {
            case List(variable) => Map(variable -> Utils.fixVars(sol("")))
            case _ => throw new ParserException(s"Failed to parse $sageReply - " +
              s"only one variable expected, but found [${vars.mkString(",")}].")
          }
        // if more than one variable exists, all is good
        case SageParser.Success(result, _) =>
          result.view.mapValues(Utils.fixVars).toMap
        case _: SageParser.NoSuccess => throw new ParserException(s"Failed to parse '$sageReply'.")
      }
      (resParsed,sageReply)
    }
    else
      (Map(),"")
  }

///////////////////////////////////

  /**
    * Build a string with all replies from Sage, after sorting by key
    * @return
    */
  def exportAll: String =
    (cacheDE-Nil)
//      .toList.sortWith((p1,p2)=>lt(p1._1,p2._1))
      .map(it => Show(it._1) +"§"+it._2._2).mkString("§§") +
    cacheStrDE
      .map(it => it._1+"§"+it._2._2).mkString("§§") +
    "§§§" +
      cacheVal
        //      .toList.sortWith((e1,e2)=>lt(e1._1,e2._1))
        .map(it => Show(it._1)+"§"+it._2._2).mkString("§§") +
      cacheStrVal
        .map(it => it._1+"§"+it._2._2).mkString("§§") +
    "§§§" +
    cacheBool
//      .toList.sortWith((e1,e2)=>lt(e1._1,e2._1))
      .map(it => Show(it._1._1,it._1._2)+"§"+it._2._2).mkString("§§") +
    cacheStrBool
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
    replies.split("§§§",4) match {
      case Array(eqsRepl,exprRepl,boolRepl,warns) =>
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
            case Array("") =>
            case Array(e,r) => importExprStr(e,r)
            case _ => throw new ParserException(s"Failed to parse reply '$er' for sage expr.")
          }

        val boolRepl2 = boolRepl.split("§§")
        for (br <- boolRepl2)
          br.split("§",2) match {
            case Array("") =>
            case Array(b,r) => importBoolStr(b,r)
            case _ => throw new ParserException(s"Failed to parse reply '$br' for boolean.")
          }

        val warns2 = warns.split("§§")
        for (w <-warns2)
          w.split("§",2) match {
            case Array("") =>
            case Array(t,wn) => this addWarning (Eval.update(hprog.DSL.parseExpr(t)
                                                    ,SVal(0):SyExpr
                                                    ,Map():Valuation)
                                        ,wn)
            case _ => throw new ParserException(s"Failed to parse reply '$w' for a warning.")
          }
    }
  }

  protected def debug(s:()=>String): Unit = {
    println("[Solver] "+s())
  }

}



