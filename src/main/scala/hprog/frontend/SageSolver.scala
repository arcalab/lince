package hprog.frontend

import hprog.ast.DiffEq
import sys.process._


class SageSolver(path:String) {
  type Solution = String // to change to something with more structure
  private var cache = Map[List[DiffEq],Solution](Nil->"")

  /**
    * Precompute and cache several system of equations with a single system call to Sage
    * @param systems systems of equations to be precomupted
    */
  def batch(systems: List[List[DiffEq]]): Unit = {
    val filtered = systems.filterNot(cache.contains)
    val instructions = filtered.map(SageSolver.genSage).mkString("; print(\"§\"); ")
    val results = s"$path/sage -c $instructions".!!
    val parsed = results.split('§')
    for ((eqs,res) <- filtered.zip(parsed)) {
      println(s"- adding (batch) ${eqs} -> $res")
      cache += eqs -> res
    }
  }

  /**
    * precompute a system of equations using a system call to Sage
    * @param eqs
    */
  def +=(eqs:List[DiffEq]): Unit = {
    if (!cache.contains(eqs)) {
      val instr = SageSolver.genSage(eqs)
      cache += eqs -> s"$path/sage -c $instr".!!
      println(s"- adding ${eqs.map(Show(_)).mkString(",")} -> ${cache(eqs)}")
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

  def cached: Map[List[DiffEq],Solution] = cache - Nil
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
