package hprog.frontend

import hprog.ast
import hprog.ast.SymbolicExpr.{Pure, SyExpr}
import hprog.ast._
import hprog.backend.Show
import hprog.common.TimeOutOfBoundsException
import hprog.frontend.CommonTypes.{Point, SySolution, Valuation, Warnings}
import hprog.frontend.Traj._
import hprog.frontend.solver.Solver

import scala.collection.mutable

class Traj(syntax:Syntax, solver:Solver, dev: Deviator, bounds:(Double,Int)) {

  def eval(t:Double): Option[Point] =
    eval(SVal(t)).map(e => Eval(e._1))

  def eval(t:SyExpr,logger: Logger = new Logger()): Option[(Valuation,TimeClosure)] = {
//    val logger = new Logger()
    Traj.run(Time(t), syntax, Map())(solver, dev, logger) match {
      case RFound(x,tc) => Some(x,tc)
      case RInf =>
        throw new RuntimeException(s"got an infinite run when evaluating ${Show(syntax)} @ ${Show(t)}.")
      case REnd(at, x, _) =>
        at match {
          case Time(t) if Eval(t)==0 =>
            val x2 = solver.solveSymb(x)
            Some(x2,TimeClosure(Map(),SVal(0)))
          case Time(t2) =>
            throw new TimeOutOfBoundsException((Eval(t)-Eval(t2)).toString,Eval(t).toString)
          case _ =>
            throw new TimeOutOfBoundsException(Show(at),Eval(t).toString)
        }
      case _ =>
        None
    }
  }

//  def evalBatch(times:List[SyExpr]): List[(SyExpr,Valuation)] = {
  def evalBatch(from:SyExpr, to:SyExpr, step:SyExpr): List[(SyExpr,Valuation)] = {

    val fromv = Eval(from)
    val tov = Eval(solver.solveSymbExpr(to))
    val stepv = Eval(solver.solveSymbExpr(step))
//    if(times.isEmpty) return Nil
    if (fromv > tov) return Nil

    val logger = new Logger()
    //val times2 = times.map(solver.solveSymbExpr)
    //        println("batch: "+times2.mkString(","))
    Traj.run(Times(fromv,tov,stepv), syntax, Map())(solver, dev, logger) match {
      case RFoundMany(found) => found
      case REnd(_, _,found) =>
        //            println("REnd")
        // Nil
              found // but there may still missing times
      case Traj.RInf =>
        //            println("Inf")
        Nil
      case RFound(_,_) =>
        //            println("##"+RFound(x))
        Nil
    }
  }

  ///////////

  private lazy val fullRun: Run = {
    debug(()=>s"[traj] using bounds $bounds")
    val rn = Traj.run(Bound(bounds._2,SVal(bounds._1)),syntax,Map())(solver,dev,logger)

    Traj.debug(()=>"warnings: "+logger.getWarnings)
    Traj.debug(()=>"endss: "+logger.getEnds)
    Traj.debug(()=>"run: "+rn)
    Traj.debug(()=>"dur: "+logger.time)

//    val max: Double = rn match {
//      case REnd(at, x, found) => Eval(logger.time)
//      case _ => 10
//    }
//    val samples = if ((max)<=0)
//      List(SVal(0))
//    else
//      SVal(0) :: (1 to 9).toList.map(_=> SDiv(SSub(SVal(max),SVal(0)),SVal(10)))
//
//    // add values of the trace (line) of thr traj
//    println(" ## samples "+samples.mkString(","))
//    val sampleValues = evalBatch(samples)
//    println(" ## values: "+sampleValues.mkString(", "))

    rn
  }
  private lazy val logger = new Logger() // to be used by "fullRun" and friends

  private def afterFullRun[A](ret:()=>A): Option[A] = fullRun match {
    case RInf => None
    case REnd(_, _,_) => Some(ret())
    case RFound(x,_) =>
      throw new RuntimeException(s"stopped a full run of ${
        Show(syntax)} @ ${Show(logger.time)} - ${
        Show(x)}.")
    case RFoundMany(fs) =>
      throw new RuntimeException(s"stopped a full run of ${
        Show(syntax)} @ ${Show(logger.time)} - ${
        fs.map(kv=>Show(kv._1)+"->"+Show(kv._2)).mkString(", ")}.")
  }

  def doFullRun: Unit = {
//    bounds = Bound(b._2,b._1)
    afterFullRun(()=>())
  }

  def getDur: Option[SyExpr] =
    afterFullRun(()=>logger.time)
  def getInits: Option[Map[SyExpr, Valuation]] =
    afterFullRun(()=>logger.getInits)
  def getEnds: Option[Map[SyExpr, Valuation]] =
    afterFullRun(()=>logger.getEnds)
  def getNotes: Option[Set[(SyExpr, String)]] =
    afterFullRun(()=>logger.getNotes)
  def getWarnings: Option[Set[(SyExpr, String)]] =
    afterFullRun(()=>logger.getWarnings)
  def addWarnings(ws: Warnings): Unit =
    logger.addWarnings(ws)


  lazy val getVars: Set[String] =
    Utils.getFstDeclVars(syntax)
}


object Traj {

  sealed abstract class RunTarget {
    def isActive: Boolean = this match {
      case Bound(0, _) => false
      case Bound(_,SVal(0)) => false
      case _ => true
    }
  }
  case class Time(t:SyExpr)             extends RunTarget
  case class Times(from:Double,to:Double,step:Double)  extends RunTarget
  case class Bound(n:Int, timer:SyExpr)  extends RunTarget

  sealed abstract class Run {
    def ++(found:List[(SyExpr,Valuation)]): Run = this match {
      case REnd(at, x, found2) =>  REnd(at,x,found2:::found)
      case RFoundMany(found2) => RFoundMany(found2:::found)
      case run => run
    }
  }
  case object RInf                                  extends Run
  case class REnd(at: RunTarget, x: Valuation,
                  found:List[(SyExpr,Valuation)])   extends Run
  case class RFound(x: Valuation,tc:TimeClosure)    extends Run
  case class RFoundMany(found:List[(SyExpr,Valuation)])    extends Run

  case class TimeClosure(e:SySolution, t:SyExpr)

  class Logger() {
    private val inits    = mutable.Map[SyExpr, Valuation]()
    private val ends     = mutable.Map[SyExpr, Valuation]()
    private val notes    = mutable.Set[(SyExpr, String)]()
    private val warnings = mutable.Set[(SyExpr, String)]()
    var time: SyExpr = SVal(0)

    def +=(t: SyExpr)(implicit solver: Solver): Unit =
      time = solver.solveSymbExpr(SAdd(time, t))
    def init(x: Valuation): Unit = if (x.nonEmpty)    inits += time -> x
    def end(x: Valuation): Unit =  if (x.nonEmpty)     ends += time -> x
    def note(s: String): Unit =    if (s.nonEmpty)    notes += time -> s
    def warn(s: String): Unit =    if (s.nonEmpty) warnings += time -> s
    def warn(ts: Double=>String,delta:SyExpr): Unit = {
      val t = Utils.asSyExpr(time+delta)
      warnings += t -> ts(Eval(time)+Eval(delta))
    }

    def addWarnings(ws:Warnings): Unit = warnings ++= ws
    def getInits: Map[SyExpr, Valuation] = inits.toMap
    def getEnds: Map[SyExpr, Valuation] = ends.toMap
    def getNotes: Set[(SyExpr, String)] = notes.toSet
    def getWarnings: Set[(SyExpr, String)] = warnings.toSet
  }

  /**
    * Evolves a program syntax at a time r (or at most r iterations of while loops)).
    * @param r time to run or maximum number of while-iterations
    * @param syntax program to evolve
    * @param x current valuation
    * @param solver to solve symbolically equations and simplify expressions
    * @param dev to calculate deviations at if-statements
    * @param logger to remember the time that passed, boundary points, notes, and warnings.
    * @return a Run: a point found, the end of the program, or an infinite run.
    */
  def run(r: RunTarget, syntax: Syntax, x: Valuation)
         (implicit solver: Solver, dev: Deviator, logger: Logger)
  : Run = {
    val res = syntax match { //(TimeOrBound, Syntax, Valuation) = syntax match {
      // Rule Atom: atomic case - stop evolving and evaluate
      case a@Atomic(_, _) => runAtomicUntilEnd(r, a, x) //(r,syntax,x)
      // Rule Seq: evolve first part (non-atomic) of at sequence
      case ast.Seq(p, q) =>
        runSeq(r, p, q, x)
      // Rule ITE 1 and 2
      case ITE(ifP, thenP, elseP) =>
        runITE(r, ifP, thenP, elseP, x)
      // Rule While 1 and 2
      case While(pre, d, doP) =>
        runWhile(r, pre, d, doP, x)
    }
//    println(s"<<< got ${res}")
    res
  }

  //////////////
  // Rule Seq //
  //////////////
  private def runSeq(r: RunTarget, p: Syntax, q: Syntax, x: Valuation)
                    (implicit solver: Solver, dev: Deviator, logger: Logger)
  : Run = {
    run(r, p, x) match {
      case REnd(r2, x2, found2) =>
        if (r2.isActive) run(r2, q, x2) ++ found2
        else REnd(r2,x2,found2)
      case run => run
    }
  }
  //////////////////////
  // Rule ITE 1 and 2 //
  //////////////////////
  private def runITE(r: RunTarget, ifS: Cond, p: Syntax, q: Syntax, x: Valuation)
                    (implicit solver: Solver, dev: Deviator, logger: Logger)
  : Run = {
    val ifValue = solver.solveSymb(ifS, x)

    // adding warnings and notes (if bounded computations)
    if (r.isInstanceOf[Bound]) logITE(ifValue, ifS, Eval(x), dev, logger)

    if (ifValue) run(r, p, x)
    else run(r, q, x)
  }

  /////////////////////////
  // Rules While 1 and 2 //
  /////////////////////////
  private def runWhile(r: RunTarget, pre: Syntax, b: LoopGuard, q: Syntax, x: Valuation)
                      (implicit solver: Solver, dev: Deviator, logger: Logger)
  : Run = { // (TimeOrBound, Syntax, Valuation) = {
    pre match {
      // Rule While-2
      case preAtomic: Atomic =>
        b match {
          // counter for "repeat" instructions
          case Counter(0) => run(r, pre, x)
          case Counter(i) => run(r, ast.Seq(pre, While(q, Counter(i - 1), q)), x)
          // guards for traditional while loops
          case Guard(c) =>
            runAtomicUntilEnd(r, preAtomic, x) match {
//              case RFound(t2, x2) =>
              case REnd(r2,x2,found2) =>
                r2 match {
                  case Bound(z,till) if z <= 0 => // no need to check for "till" - it will set "z" to zero if finished
                    run(Bound(0,till), skip, x2)
                  case Bound(n,till) =>
                    runITE(Bound(n - 1,till), c, While(q, b, q), skip, x2) ++ found2
                  case _ =>
                    runITE(r2, c, While(q, b, q), skip, x2) ++ found2
                }
              case run => run
            }
        }
      // Rule While-1
      case _ =>
        run(r, pre, x) match {
          case REnd(r2, x2, found) =>
            runWhile(r2, skip, b, q, x2) ++ found
          case run => run
        }
    }
  }

  ////////////////////////////////////////////////////////////////////////
  ////////////////////////////////////////////////////////////////////////


  // Function that calls the solver
  @scala.annotation.tailrec
  private def runAtomicUntilEnd(rb: RunTarget, at: Atomic, x: Valuation)
                               (implicit solver: Solver, logger: Logger)
  : Run = {
    at.de.dur match {
      // special case: (0 duration - log 0-time event (if some valuation))
      case For(Value(0)) =>
        val delta = Utils.toValuation(at.as,x)
        val x2 = x++delta
        if (delta.nonEmpty) logger.init(x2)
        REnd(rb,x2,Nil)
      // Rule 2 or 3 with a fixed deadline
      case For(d) =>
        rb match {
          // Rule 1+2
          case Time(time) =>
            runAtomicWithTime(time,at,d,x,true) // set log=false if warnings are not important

          // variation of atomic-time rules
          case times:Times =>
            runAtomicWithTimes(times, at, d, x, Nil)

          // variation of rule 3 (for time = inf, with bounded loops)
          case b:Bound =>
            runAtomicWithBounds(b,at,d,x)
        }

      // Rule 2 (specific case)
      case Forever =>
        RInf

      // Numerically estimate duration. Experimental - only works for very specific cases.
      case u:Until =>
        val x2 = x ++ Utils.toValuation(at.as,x) // update x with as
        val durEstimation = Solver.estimateDur(u, at.de.eqs, x2, solver) match {
          case Some((d,ws)) =>
            for (w<-ws) logger.warn(w,SVal(d))
            For(Value(d))
          case None => Forever
        }
        runAtomicUntilEnd(rb, Atomic(at.as, DiffEqs(at.de.eqs, durEstimation)), x)
    }
  }


  private def runAtomicWithTime(time: SyExpr, at:Atomic, dur:Lin, x:Valuation,log:Boolean = false)
                               (implicit solver:Solver, logger: Logger): Run = {
    val phi = solver.solveSymb(at.de.eqs)
    val x2 = x ++ Utils.toValuation(at.as,x) // update x with as

    debug(()=>s"running $at @ ${Eval(time)} (${Show(time)}) for $dur on $x2.")
    val realTime = Eval(time)
    val durSy = Eval.lin2sage(dur)
    val durSy2 = Eval.updInput(durSy,x2)
    val durVal = Eval(durSy2) max 0
    debug(()=>s"$dur ~~> $durSy ~~> $durSy2 ~~> $durVal.")

    // Rule Atom-1 (time < dur)
    if (realTime < durVal) {
      val x3 = x2 ++ Eval.update(phi, time, x2) // update x with phi
//      val tc = x2.mapValues(e => Eval.updInputFun(e,phi))

      val tc = Eval.updInputFun(x2,phi).view.mapValues(solver.solveSymb).toMap
      //if (log) logger += time
      RFound(x3.view.mapValues(solver.solveSymbExpr).toMap,TimeClosure(tc,time))
    }
    // Rule Atom-2 (time >= dur)
    else {
      val x3 = solver.solveSymb(x2 ++ Eval.update(phi, SVal(durVal), x2)) // update x with phi
      val r2 = solver.solveSymbExpr(SSub(time, SVal(durVal))) // new simplified time
      if (log) logger += SVal(durVal)
      REnd(Time(r2), x3, Nil)
    }


  }


  //////////////////////////////////////////////////////////////////
  /// Useful extensions to the core semantics:                   ///
  ///  - batch processing of multiple time values in one pass    ///
  ///  - Running until the end (with no value to be searched)    ///
  ///  - store calculations by an if-then-else to be displayed.  ///
  //////////////////////////////////////////////////////////////////

  @scala.annotation.tailrec
  private def runAtomicWithTimes(times:Times, at:Atomic, durLin:Lin, x:Valuation,
                                 found:List[(SyExpr,Valuation)])
                                (implicit  logger: Logger, solver: Solver): Run= {
    debug(()=>s"RunAtomicTimes @ ${Show(times)} - ${Show(at)} for ${Show(durLin)}")
    times match {
      case Times(from,to,_) if to<= (Eval(logger.time)+from) =>
//      case Nil =>
        debug(()=>s"time to stop ($to > ${Eval(logger.time)+from})")
        RFoundMany(found)
      //        REnd(times, x, found)

//      case time::rest =>
      case Times(from,to,step) =>
        debug(()=>s"continuing")
        val time = from
        runAtomicWithTime(SVal(time), at, durLin, x, log = true) match {
          case RFound(x2,_) =>
            // time2 is the global time when the element was found.
            //val time2 = solver.solveSymbExpr(SAdd(logger.time, time))
            val realTime = SVal(Eval(logger.time) + time)
            //println(s"found next @${Eval(time2)} -> ${Show(x2)}")
            val found2 = (realTime->x2) :: found
            val realNext = from + step
            debug(()=>s"atomc run found @ $time (real: $realTime, real next: $realNext)")
            if (realNext >= to)
//            rest match {
//              case Nil =>
                        RFoundMany(found2) //++ List(time2 -> x2)
//              case hd :: tl =>
            else {
                // rest2 updates the next time value to include the time spent to find x2
//                val rest2 = solver.solveSymbExpr(SAdd(hd, time)) :: tl
                runAtomicWithTimes(Times(from+step,to,step),at,durLin,x,found2)
            }

          // update end point to be an updated list ot times
          case e@REnd(Time(t), x2,found2) =>
            debug(()=>s"atomc run ended - $e")
            REnd(Times(Eval(t),to,step), x2,found:::found2)

          case r => r
        }
    }
  }


  private def runAtomicWithBounds(b:Bound,at:Atomic,durLin:Lin,x:Valuation)
                                 (implicit solver: Solver, logger: Logger): Run = {
    val phi = solver.solveSymb(at.de.eqs)
    val x2 = x ++ Utils.toValuation(at.as,x) // update x with as

    logger.note(Show.pp(phi,x2))

    debug(()=>s"running $at bounded $b for $durLin on $x2.")
    val durSy = Eval.lin2sage(durLin)
    debug(()=>s" - $durSy")
    val durSy2 = Eval.updInput(durSy,x2)
    debug(()=>s" - $durSy2")
    val durValue = solver.solveSymbExpr(durSy2)

    // manually comparing values that should be simplified already
    val stop = Eval(durValue) >= Eval(b.timer)

    if (stop) {
      val x3 = x2++ solver.solveSymb(Eval.update(phi, b.timer, x2)) // update x with phi after b.timer durtion
      logger.init(x2)
      logger += b.timer
      logger.end(x3)
      REnd(Bound(0,SVal(0)), x3, Nil)
    }
    else {
      val newTimer = solver.solveSymbExpr(SSub(b.timer,durSy2))
      val x3 = x2++ solver.solveSymb(Eval.update(phi, durSy2, x2)) // update x with phi after b.timer durtion
      logger.init(x2)
      logger += durSy2
      logger.end(x3)
      REnd(Bound(b.n,newTimer),x3,Nil)
    }


//    val durMin = Eval(durSy2) max 0
//    debug(()=>s" - $durMin")
//    val dur = durMin min b.timer
//    debug(()=>s" - $dur")

//    val x3 = x2++ solver.solveSymb(Eval.update(phi, SVal(dur), x2)) // update x with phi
//    logger.init(x2)
//    logger += SVal(dur)
//    logger.end(x3)
//    REnd(Bound(b.n,b.SSub(timer,dur)), x3, Nil)
  }

  // AUxiliar function to log warnings and notes
  private def logITE(b: Boolean, ifS: Cond, x: Point, dev: Deviator, logger: Traj.Logger): Unit = {
    {
      val notIf = if (b) Not(ifS) else ifS
      val cls = dev.closest(x, notIf)
      //debug(()=>s"%%% closest point:\n - $cls\n ~ $x")
      cls match {
        case Some(p2) =>
          if (x != p2)
            logger.warn(s"Perturbation by ${
              Distance.dist(x, p2)
            }</br>when testing ${
              Show(ifS)
            }</br>with:</br>${
              p2.map(kv => s"${kv._1}:${kv._2}").mkString("</br>")
            }")
          else
            logger.warn(s"Perturbation found by any small delta</br>when testing ${Show(ifS)}.")
        case None =>
      }
      logger.note(s"${Show(ifS)}? $b")
    }
  }

  private val skip = Atomic(Nil, DiffEqs(Nil, For(Value(0))))

  private def debug(str: () => String): Unit = {
    //println("[Traj] "+str())
  }


}
