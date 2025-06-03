package hprog.backend

import hprog.ast.SVal
import hprog.ast.Syntax.{Cond, Expr, Syntax, Value}
import hprog.common.ParserException
import hprog.frontend.{Deviator, Eval, Traj}
import hprog.frontend.solver.{RungeKutta, Solver}
import hprog.lang.Parser

object ProbChecker {


  def apply(query: String, prog: String, maxTime: Double, maxIterations: Int): String = {

    def getPRes[T](p:Parser.ParseResult[T]): T = p match {
      case Parser.Success(res, _) => res
      case Parser.Failure(msg, _) => throw new ParserException(msg)
      case Parser.Error(msg, _) => throw new ParserException(msg)
    }

    // 1: parse program
    val syntax = getPRes(Parser.parse(prog))

    // 2: split query into expression, sampling, and runs
    // IDEA -- query := "v>5 AT TIMES"
    // AT := "@[2..5]" "@[2..]" "@[2..5 by 2]" "@[2,3,4]"
    // TIMES := "x100" "@[2..]" "@[2..5 by 2]" "@[2,3,4]"
    // For now: everything mandatory with this order, no confidence interval, no estimation of runs, etc.
    val (q1,q23) = query
      .split('@')
//      .flatMap(_.split("x"))
      .map(_.trim) match {
        case Array(x1,x2) => (x1,x2)
        case x =>
          throw new ParserException(s"""Expected "<query> @ <interval> x <nb. runs>", but instead got "$query".""")
    }
    val (q2,q3) = q23
      .split('x')
      .map(_.trim) match {
        case Array(x2, x3) => (x2, x3)
        case x =>
          throw new ParserException(s"""Expected "<query> @ <interval> x <nb. runs>", but instead got "$query".""")
      }

    // 3: parse expression
    val cond: Cond = getPRes(Parser.parseAll(Parser.condP, q1))

    // 4: parse sampling (fails with variables)
    val (from,end1,by1) = getPRes(Parser.parseAll(Parser.rangeP,q2))
    val end = end1 min maxTime
    val by = by1.getOrElse((end-from)/20)

    // 5: parse number of runs
    val runs: Int = q3.toInt

    // 6: build bounds (error if no sampling)
    val bounds = (end, maxIterations)

    // number of success cases found
    var found = 0

    for (x <- 1 to runs) {
      // 6: build trajectory for each run
      val traj = new Traj(syntax, new RungeKutta, Deviator.dummy, bounds, None)// Some(x))
//      println("### Generated trajectory.")
//      println(s"### Creating random generator with seed ($x): ${traj.nextRand()}, ${traj.nextRand()}, ${traj.nextRand()}")
      val run = traj.evalBatch(SVal(from), SVal(end), SVal(by))
//      println(s"§§§ Finished run $x: ${run//.map(_._2)
//        .mkString(",")}")
      var continue = true
      for ((_,valuation) <- run if continue)  {
        if (Eval(Eval(valuation),cond)) {
          continue = false
          found += 1
        }
      }
    }

    s"P = ${math.round(found.toDouble/runs * 100)}  [$found/$runs]"
  }

//  def apply(query: String, syntax: Syntax, solver: Solver, dev: Deviator,
//            bounds: (Double, Int), var randomSeed: Option[Long] = None) {
//    ???
//  }

}
