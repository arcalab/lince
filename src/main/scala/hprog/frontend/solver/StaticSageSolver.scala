package hprog.frontend.solver

import hprog.ast._
import hprog.backend.Show
import hprog.common.ParserException
import hprog.frontend.Semantics.{SFunction, SageSolution, SolVars, Solution, Valuation}
import hprog.frontend.{Eval, Utils}
import hprog.lang.SageParser

import scala.sys.process._


class StaticSageSolver extends Solver {

  protected var cache =
    Map[List[DiffEq],(SageSolution,Iterable[String])](Nil->(Map(),Nil))
  protected var cacheVal =
    Map[SageExpr,(SageExpr,String)]()

  def ++=(systems: List[List[DiffEq]]): Unit = {
    systems.map(+=)
//    val filtered = systems.filterNot(cache.contains)
//    if (filtered.nonEmpty)
//      throw new SageSolver.SolvingException(s"Static solver failed: unknown equations " +
//        filtered.map(_.map(Show(_)).mkString("&")).mkString(" and "))
  }

//  addToCache(eqs, sageReply)
  def importSol(systems: List[List[DiffEq]],sageReply:Iterable[String]) =
    addToCache(systems,sageReply)

  def ++=(syntax:Syntax): Unit = {
    ++=(Utils.getDiffEqs(syntax))
  }

  /**
    * precompute a system of equations using a system call to Sage
    * @param eqs
    */
  def +=(eqs:List[DiffEq]): Unit =
    if (!cache.contains(eqs))
      throw new SageSolver.SolvingException(s"Static solver failed: unknown equations " +
        eqs.map(Show(_)).mkString("&"))
      //      val instr = SageSolver.genSage(eqs)
      //      cache += (eqs -> (s"$path/sage -c $instr".!!))
      //      println(s"- adding ${eqs.map(Show(_)).mkString(",")} -> ${cache(eqs)}")

  def solveSymb(eqs:List[DiffEq]): SageSolution = {
    this += eqs
    cache(eqs)._1
  }

  protected def addToCache(eqs:List[List[DiffEq]] ,sageReply:Iterable[String]): Unit =
    for ((eqs, res) <- eqs.zip(sageReply)) {
      //println(s"- adding  ${eqs} -> $res")
      if (res.nonEmpty) {
        val resParsed = SageParser.parse(res) match {
          // single solution - name is not known from the answer of Sage
          case SageParser.Success(SolVars(sol), _) if sol.keySet == Set("") =>
            val vars = Solver.getVars(eqs).filterNot(_.startsWith("_"))
            vars match {
              case List(variable) => Map(variable -> sol(""))
              case _ => throw new ParserException(s"Failed to parse $res - " +
                s"only one variable expected, but found ${vars.mkString(",")}.")
            }
          // if more than one variable exists, all is good
          case SageParser.Success(result, _) => result.sol
          case _: SageParser.NoSuccess => throw new ParserException(s"Failed to parse '$res'.")
        }
        cache += eqs -> resParsed
      }
    }

  override def evalFun(expr: SageExpr): SFunction = {
    cacheVal.get(expr) match {
      case Some(value) => (t:Double) => (v:Valuation) => Eval(value,t,v)
      case None =>
    }
  }

  override def evalVal(expr: SageExpr): Valuation = ???

  override def solveSymb(expr: SageExpr): SageExpr = ???
}



