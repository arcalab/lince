package hprog.frontend.solver

import hprog.ast._
import hprog.backend.Show
import hprog.common.ParserException
import hprog.frontend.Semantics.{SageSolution, SolVars, Solution}
import hprog.frontend.{Eval, Utils}
import hprog.lang.SageParser

import scala.sys.process._


class SageSolver(path:String) extends StaticSageSolver {


  /**
    * precompute a system of equations using a system call to Sage
    * @param eqs
    */
  override def +=(eqs:List[DiffEq]): Unit =
    if (!cache.contains(eqs)) {
      val reply = SageSolver.callSageSolver(eqs, path)
      addToCache(filtered, reply)
    }

  //      val instr = SageSolver.genSage(eqs)
  //      cache += (eqs -> (s"$path/sage -c $instr".!!))
  //      println(s"- adding ${eqs.map(Show(_)).mkString(",")} -> ${cache(eqs)}")



  //  /**
//    * Precompute and cache several system of equations with a single system call to Sage
//    * @param systems systems of equations to be precomupted
//    */
//  override def ++=(systems: List[List[DiffEq]]): Unit = {
//    val filtered = systems.filterNot(cache.contains)
//    if (filtered.nonEmpty) {
//      val reply = SageSolver.callSageSolver(filtered,path)
//      addToCache(filtered,reply)

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


//  /** Gets a solution of a system of equations, using system calls to Sage,
//    * checking first in its cache.
//    * @param eqs System of equations to be retrived
//    * @return Result from Sage
//    */
//  def evalFun(eqs:List[DiffEq]): Solution = {
//    solveSymb(eqs)
//      .mapValues(expr => t => x => Eval(expr,t,x))
//  }


//  private def cached: Map[List[DiffEq],SageSolution] = cache - Nil


}


///////////////////////////////////////
////////// Static functions ///////////
///////////////////////////////////////
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


  // DEPRECATED - when calling all in one go. Now calling one eqs at a time
  def callSageSolver(systems: List[List[DiffEq]], path: String, timeout:Int = 10): List[String] = {
    if (systems.filter(_.nonEmpty).nonEmpty) {
      //println(s"solving Sage with ${systems}")
      val instructions = systems.map(SageSolver.genSage).mkString("; print(\"§\"); ")

      println(s"instructions to solve: ${instructions}")

      val stdout = new StringBuilder
      val stderr = new StringBuilder
      val status = s"timeout $timeout $path/sage -c $instructions" !
                   ProcessLogger(stdout append _, stderr append _)
      if (status == 0)
        stdout.split('§').toList
      else
        throw new SageSolver.SolvingException(stderr.toString)
    }
    else
      Nil
  }

  //// running sage the 2nd time
  private def genSage(c:Cond): String = c match {
    case BVal(b) => b.toString
    case And(c1, c2) => s"(${genSage(c1)} & ${genSage(c2)})"
    case Or(c1, c2) => s"(${genSage(c1)} | ${genSage(c2)})"
    case Not(c) =>s"not(${genSage(c)})"
    case EQ(v, l) => s"(__${v.v} == ${genSage(l)})"
    case GT(v, l) => s"(__${v.v} >  ${genSage(l)})"
    case LT(v, l) => s"(__${v.v} <  ${genSage(l)})"
    case GE(v, l) => s"(__${v.v} >= ${genSage(l)})"
    case LE(v, l) => s"(__${v.v} <= ${genSage(l)})"
  }
  private def genSage(lin: Lin): String = lin match {
    case Var(v) => v
    case Value(v) => v.toString
    case Add(l1, l2) => s"(${genSage(l1)} + ${genSage(l2)}"
    case Mult(v, l) => s"(${v.v} * ${genSage(l)}"
  }
  private def genSage(e: SageExpr): String = e match {
    case SVal(v) => v.toString
    case SArg => "_t_"
    case SVar(v) => "__"+v
    case SFun(f, args) => s"$f(${args.map(genSage).mkString(",")})"
    case SDiv(e1, e2) => s"(${genSage(e1)} / ${genSage(e2)})"
    case SMult(e1, e2) =>s"(${genSage(e1)} * ${genSage(e2)})"
    case SPow(e1, e2) => s"(${genSage(e1)} ^ ${genSage(e2)})"
    case SAdd(e1, e2) => s"(${genSage(e1)} + ${genSage(e2)})"
    case SSub(e1, e2) => s"(${genSage(e1)} - ${genSage(e2)})"
  }
  private def genSage(sol:SageSolution): String =
    sol.map(kv => s"__${kv._1} = ${genSage(kv._2)};").mkString
  def genSage(c:Cond,s:SageSolution): String =
    s"${genSage(s)} print(${genSage(c)})"





  class SolvingException(s:String) extends RuntimeException(s)
}



