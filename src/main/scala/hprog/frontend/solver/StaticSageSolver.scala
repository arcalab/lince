package hprog.frontend.solver

import hprog.ast._
import hprog.backend.Show
import hprog.common.ParserException
import hprog.frontend.Semantics.{SFunction, SageSolution, SolVars, Solution, Valuation}
import hprog.frontend.{Eval, Utils}
import hprog.lang.SageParser

import scala.sys.process._


class StaticSageSolver extends Solver {

  type DiffCache = (SageSolution,Iterable[String]) // one SageExpr for each variable
  type ExprCache = (SageExpr,String) // simplified version

  protected var cache:
    Map[List[DiffEq], (SageSolution, Iterable[String])] =
    Map(Nil->(Map(),Nil))
  protected var cacheVal:
    Map[SageExpr, (SageExpr, String)] =
    Map()

  /**
    * Throw an error if a system of equations was not precomputed
    * @param eqs system of equations
    */
  def +=(eqs:List[DiffEq]): Unit =
    if (!cache.contains(eqs))
      throw new SageSolver.SolvingException(s"Static solver failed: unknown equations " +
        eqs.map(Show(_)).mkString("&"))
  //      val instr = SageSolver.genSage(eqs)
  //      cache += (eqs -> (s"$path/sage -c $instr".!!))
  //      println(s"- adding ${eqs.map(Show(_)).mkString(",")} -> ${cache(eqs)}")

  /**
    * Throw an error if an expression was not precomputed
    * @param expr
    */
  def +=(expr: SageExpr): Unit =
    if (!cacheVal.contains(expr))
      throw new SageSolver.SolvingException(
        s"Static solver failed: unknown expr: $expr.")



  override def solveSymb(expr: SageExpr): SageExpr = {
    this += expr
    cacheVal(expr)._1
  }
  def solveSymb(eqs:List[DiffEq]): SageSolution = {
    this += eqs
    cache(eqs)._1
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
    * @param expr
    * @param sageReply
    */
  def importExpr(expr:SageExpr, sageReply:String): Unit =
    if (!cacheVal.contains(expr)) {
      val resParsed = SageParser.parseExpr(sageReply)
      resParsed match {
        case SageParser.Success(newExpr, _) =>
          cacheVal += expr -> (newExpr,sageReply)
        case _: SageParser.NoSuccess =>
          throw new ParserException(s"Failed to parse '$sageReply'.")
      }
    }

  /**
    * Import the reply from Sage from solving a system of equations
    * @param eqs
    * @param sageReply
    */
  // e.g., add "x'=y, y'=2" and ["x(_t_)=sin(_t_)",...]
  def importDiffEqs(eqs:List[List[DiffEq]],
                    sageReply:Iterable[String]): Unit =
    for ((eqs, res) <- eqs.zip(sageReply)) {
      //println(s"- adding  ${eqs} -> $res")
      importDiffEqs(eqs,res)
    }

  def importDiffEqs(eqs:List[DiffEq], sageReply:String): Unit = {
    if (sageReply.nonEmpty) {
      val resParsed = SageParser.parse(sageReply) match {
        // single solution - name is not known from the answer of Sage
        case SageParser.Success(SolVars(sol), _) if sol.keySet == Set("") =>
          val vars = Solver.getVars(eqs).filterNot(_.startsWith("_"))
          vars match {
            case List(variable) => Map(variable -> sol(""))
            case _ => throw new ParserException(s"Failed to parse $sageReply - " +
              s"only one variable expected, but found ${vars.mkString(",")}.")
          }
        // if more than one variable exists, all is good
        case SageParser.Success(result, _) => result.sol
        case _: SageParser.NoSuccess => throw new ParserException(s"Failed to parse '$sageReply'.")
      }
      cache = cache + (eqs -> (resParsed,List(sageReply)))
    }

  }


}



