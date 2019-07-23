package hprog.frontend

import hprog.ast.SageExpr.{SExpr, SExprT}
import hprog.ast.{SArg, SSub, SVal}
import hprog.backend.Show
import hprog.frontend.Semantics.{Notes, Point, SageSolution, TimeSolution, Warnings}
import hprog.frontend.solver.Solver

/**
  *
  * @tparam X type of the value at a given time - e.g., [[hprog.frontend.Semantics.Valuation]].
  */
trait Traj[X] {
  // duration
  val dur: Option[SExpr]
  def apply(t:SExpr)(implicit s:Solver): X      // symbolic values at time instant t
  def apply(t:Double): Point // real     values at time instant t

  val fun: SageSolution = Map() // only for pretty printing...
  def fun(t:SExpr)(implicit s:Solver): TimeSolution // only for debugging...

  val inits:Map[SExpr,X] = Map() // starting points of sub-trajectories
  val ends: Map[SExpr,X] = Map() // ending   points of sub-trajectories
  val notes: Notes = Nil         // text notes to add at trajectory points.
  val warnings: Warnings = Nil   // warning messages to add at trajectory points.

  def addWarnings(w:List[(SExpr,String)]): Traj[X] = {
    val t = this
    new Traj[X] {
      override val warnings: List[(SExpr, String)] = t.warnings ++ w
      /////// rest is the same
      override val dur: Option[SExpr]   = t.dur
      override def apply(e:SExpr)(implicit s:Solver): X  = t.apply(e)
      override def apply(x:Double): Point  = t.apply(x)

      override val fun: SageSolution = t.fun
      override def fun(e:SExpr)(implicit s:Solver): TimeSolution = t.fun(e)
      override val inits:Map[SExpr,X] = t.inits
      override val ends: Map[SExpr,X] = t.ends
      override val notes: List[(SExpr,String)] = t.notes
    }
  }

  def addNotes(n:List[(SExpr,String)]): Traj[X] = {
    val t = this
    new Traj[X] {
      override val notes: List[(SExpr,String)] = t.notes ++ n
      /////// rest is the same
      override val dur: Option[SExpr] = t.dur
      override def apply(e:SExpr)(implicit s:Solver): X  = t.apply(e)
      override def apply(x:Double): Point  = t.apply(x)

      override val fun: SageSolution = t.fun
      override def fun(e:SExpr)(implicit s:Solver): TimeSolution = t.fun(e)
      override val inits:Map[SExpr,X] = t.inits
      override val ends: Map[SExpr,X] = t.ends
      override val warnings: List[(SExpr, String)] = t.warnings
      }
  }
}


object Traj {
  def join[X](t1: Traj[X], t2: Traj[X]): Traj[X] = {
    //println(s"joining ${Show(t1.fun)} with ${Show(t2.fun)}")

    val res = t1.dur match {
      case None => t1
      case Some(dur1) => new Traj[X] {
        //println(s"joined ${Show(t1.fun)} with ${Show(t2.fun)} after ${dur1}")

        override val fun: SageSolution =
          t2.fun.map(kv => (kv._1 + "#", kv._2)) ++ t1.fun
        override val inits: Map[SExpr, X] =
          t1.inits ++ (if (dur1 == SVal(0)) t2.inits
          else t2.inits.map(p => add(p._1, dur1) -> p._2))
        override val ends: Map[SExpr, X] =
          t1.ends ++ (if (dur1 == SVal(0)) t2.ends
          else t2.ends.map(p => add(p._1, dur1) -> p._2))
        override val notes: List[(SExpr, String)] =
          t1.notes ++ (if (dur1 == SVal(0)) t2.notes
          else t2.notes.map(p => (add(p._1, dur1), p._2)))
        //appendN(t1.notes, t2.notes.map(p => (p._1 + dur1) -> p._2))
        override val warnings: List[(SExpr, String)] =
          t1.warnings ++ (if (dur1 == SVal(0)) t2.warnings
          else t2.warnings.map(p => (add(p._1, dur1), p._2)))

        override val dur: Option[SExpr] =
          for (dur2 <- t2.dur) yield add(dur1, dur2)


        override def fun(t:SExpr)(implicit s:Solver): TimeSolution =
          if (Eval(t) < Eval(dur1)) t1.fun(t)
          else t2.fun(SSub(t, dur1)).mapValues(e=>Eval.updTimeT( SSub(SArg(),dur1), e))

        override def apply(t: SExpr)(implicit s: Solver): X =
          if (Eval(t) < Eval(dur1)) { // Need solver?
            //println(s"[Traj] L ${Show(t1.fun)}")
            t1(t)
          }
          else {
            //println(s"[Traj] R ${Show(t2.fun)}")
            t2(s.solveSymb(SSub(t, dur1)))
          }
        override def apply(t: Double): Point =
          if (t < Eval(dur1)) t1(t)
          else t2(t - Eval(dur1))

        private def add(e1: SExpr, e2: SExpr) =
          SVal(Eval(e1) + Eval(e2)) // approximating notes location
      }
    }
    //println(s"got ${Show(res.fun)}")
    res
  }
}

trait Prog[X] {
  def traj(init:X): Traj[X]
  def ++(other:Prog[X])(implicit s:Solver): Prog[X] = Prog.join(this,other)(s)
}


object Prog {

  def join[X](p1:Prog[X],p2:Prog[X])(implicit s:Solver): Prog[X] = (input: X) => {
    val t1 = p1.traj(input)
    t1.dur match {
      // Traj1 runs forever
      case None => t1
      // Traj1 ends at dur1
      case Some(dur1) =>
        val input2: X = t1(dur1) // returns new valuation for what t1 knows
        //println(s"... t1 using input ${input}")
        //println(s"... joining ${Show(t1.fun((SVal(0.2))))} at ${Show(dur1)} where ${input2}")
        Traj.join(t1, p2.traj(input2))
    }
  }
}

