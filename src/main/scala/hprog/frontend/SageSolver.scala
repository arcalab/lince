package hprog.frontend

import hprog.ast._
import hprog.common.ParserException
import hprog.frontend.Semantics.Solution
import hprog.lang.SageParser

import scala.sys.process._


class SageSolver(path:String) extends Solver {
  private var cache = Map[List[DiffEq],Solution](Nil->Map())

  /**
    * Precompute and cache several system of equations with a single system call to Sage
    * @param systems systems of equations to be precomupted
    */
  def ++=(systems: List[List[DiffEq]]): Unit = {
    val filtered = systems.filterNot(cache.contains)
    val instructions = filtered.map(SageSolver.genSage).mkString("; print(\"§\"); ")
    val results = s"$path/sage -c $instructions".!!
    val parsed = results.split('§')
    for ((eqs,res) <- filtered.zip(parsed)) {
      //println(s"- adding  ${eqs} -> $res")
      val resParsed = SageParser.parse(res) match {
        case SageParser.Success(result, _) => result.sol
        case _: SageParser.NoSuccess => throw new ParserException(s"Failed to parse $res")
      }
      cache += eqs -> resParsed
    }
  }

  def ++=(syntax:Syntax): Unit = {
    def getDiffEqs(prog:Syntax): List[List[DiffEq]]  = prog match {
      case d@DiffEqs(eqs, dur) => List(eqs)
      //    case Seq(Nil) => Nil
      case hprog.ast.Seq(p :: ps) =>
        getDiffEqs(p) ::: getDiffEqs(hprog.ast.Seq(ps))
      //    case Skip => Nil
      case ITE(ifP, thenP, elseP) =>
        getDiffEqs(thenP) ++ getDiffEqs(elseP)
      case While(c, doP) => getDiffEqs(doP)
      case _ => Nil
    }
    ++=(getDiffEqs(syntax))
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
}

object SageSolver {


  /**
    * Experimental: generating code to be sent to Sage
    * @param eqs
    * @return
    */
  def genSage(eqs:List[DiffEq]): String = {
    var res = "_t_ = var('_t_'); "
    for (e <- eqs)
      res += s"${e.v.v} = function('${e.v.v}')(_t_); "
    for ((e,i) <- eqs.zipWithIndex)
      res += s"_de${i}_ = diff(${e.v.v},_t_) == ${Show(e.e)}; "
    res += s"print(expand(desolve_system([${(for(i<-eqs.indices)yield s"_de${i}_").mkString(",")}]," +
      s"[${eqs.map(_.v.v).mkString(",")}])))"
    res
  }

}
