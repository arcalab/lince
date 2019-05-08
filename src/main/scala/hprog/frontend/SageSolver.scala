package hprog.frontend

import hprog.ast._
import hprog.backend.Show
import hprog.common.ParserException
import hprog.frontend
import hprog.frontend.Semantics.{SolVars, Solution}
import hprog.lang.SageParser

import scala.sys.process._


class SageSolver(path:String) extends Solver {
  protected var cache = Map[List[DiffEq],Solution](Nil->Map())

  /**
    * Precompute and cache several system of equations with a single system call to Sage
    * @param systems systems of equations to be precomupted
    */
  def ++=(systems: List[List[DiffEq]]): Unit = {
    val filtered = systems.filterNot(cache.contains)
    if (filtered.nonEmpty) {
      val reply = SageSolver.callSageSolver(filtered,path)
      addToCache(filtered,reply)

//      val instructions = filtered.map(SageSolver.genSage).mkString("; print(\"§\"); ")
//      val results = s"$path/sage -c $instructions".!!
//      val parsed = results.split('§')
//      for ((eqs, res) <- filtered.zip(reply)) {
//        //println(s"- adding  ${eqs} -> $res")
//        val resParsed = SageParser.parse(res) match {
//          case SageParser.Success(SolVars(s,sol), _) if s.isEmpty && sol.keySet == Set("") =>
//            val vars = Solver.getVars(eqs).filterNot(_.startsWith("_"))
//            vars match {
//              case List(variable) => Map(variable -> sol(""))
//              case _ => throw new ParserException(s"Failed to parse $res - " +
//                s"only one variable expected, but found ${vars.mkString(",")}.")
//            }
//          case SageParser.Success(result, _) => result.sol
//          case _: SageParser.NoSuccess => throw new ParserException(s"Failed to parse $res")
//        }
//        cache += eqs -> resParsed
//      }
    }
  }

  def ++=(syntax:Syntax): Unit = {
    ++=(Utils.getDiffEqs(syntax))
  }

  /**
    * precompute a system of equations using a system call to Sage
    * @param eqs
    */
  def +=(eqs:List[DiffEq]): Unit = {
    if (!cache.contains(eqs)) {
//      val instr = SageSolver.genSage(eqs)
//      cache += (eqs -> (s"$path/sage -c $instr".!!))
//      println(s"- adding ${eqs.map(Show(_)).mkString(",")} -> ${cache(eqs)}")
      ++=(List(eqs))
    }
  }

  /** Gets a solution of a system of equations, using system calls to Sage,
    * checking first in its cache.
    * @param eqs System of equations to be retrived
    * @return Result from Sage
    */
  def get(eqs:List[DiffEq]): Solution = {
    this += eqs
    cache(eqs)
  }

  private def cached: Map[List[DiffEq],Solution] = cache - Nil


  protected def addToCache(eqs:List[List[DiffEq]] ,sageReply:Iterable[String]): Unit =
    for ((eqs, res) <- eqs.zip(sageReply)) {
      //println(s"- adding  ${eqs} -> $res")
      if (res.nonEmpty) {
        val resParsed = SageParser.parse(res) match {
          case SageParser.Success(SolVars(s, sol), _) if s.isEmpty && sol.keySet == Set("") =>
            val vars = Solver.getVars(eqs).filterNot(_.startsWith("_"))
            vars match {
              case List(variable) => Map(variable -> sol(""))
              case _ => throw new ParserException(s"Failed to parse $res - " +
                s"only one variable expected, but found ${vars.mkString(",")}.")
            }
          case SageParser.Success(result, _) => result.sol
          case _: SageParser.NoSuccess => throw new ParserException(s"Failed to parse '$res'.")
        }
        cache += eqs -> resParsed
      }
    }

}

object SageSolver {


  /**
    * Experimental: generating code to be sent to Sage
    * @param eqs
    * @return
    */
  def genSage(eqs:List[DiffEq]): String = {
    var res = "_t_ = var('_t_'); "
    val undefinedVars = Utils.getUsedVars(eqs) -- Utils.getDefVars(eqs)
    val eqs2 = eqs ::: undefinedVars.map(v => DiffEq(Var(v),Value(0))).toList

    for (e <- eqs2)
      res += s"${e.v.v} = function('${e.v.v}')(_t_); "
    for ((e,i) <- eqs2.zipWithIndex)
      res += s"_de${i}_ = diff(${e.v.v},_t_) == ${Show(e.e)}; "
    res += s"print(expand(desolve_system([${(for(i<-eqs2.indices)yield s"_de${i}_").mkString(",")}]," +
      s"[${eqs2.map(_.v.v).mkString(",")}])))"
    res
  }

  def callSageSolver(systems: List[List[DiffEq]], path: String, timeout:Int = 10): List[String] = {
    if (systems.filter(_.nonEmpty).nonEmpty) {
      //println(s"solving Sage with ${systems}")
      val instructions = systems.map(SageSolver.genSage).mkString("; print(\"§\"); ")

      //println(s"instructions to solve: ${instructions}")

      val stdout = new StringBuilder
      val stderr = new StringBuilder
      val status = s"timeout $timeout $path/sage -c $instructions" !
                   ProcessLogger(stdout append _, stderr append _)
      if (status == 0)
        stdout.split('§').toList
      else
        throw new frontend.SageSolver.SolvingException(stderr.toString)
    }
    else
      Nil
  }

  class SolvingException(s:String) extends RuntimeException(s)
}


class SageSolverStatic(eqs:List[List[DiffEq]], sageReply: Iterable[String]) extends SageSolver("") {

  override def ++=(systems: List[List[DiffEq]]): Unit = {
    val filtered = systems.filterNot(cache.contains)
    if (filtered.nonEmpty)
      throw new SageSolver.SolvingException(s"Static solver failed: unknown equations " +
        filtered.map(_.map(Show(_)).mkString("&")).mkString(" and ") )
  }
  addToCache(eqs,sageReply)
}

