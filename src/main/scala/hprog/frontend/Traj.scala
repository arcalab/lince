package hprog.frontend

/**
  *
  * @tparam X type of the value at a given time - e.g., [[hprog.frontend.Semantics.Valuation]].
  */
trait Traj[X] {
  // duration
  val dur: Option[Double]
  def apply(t:Double): X  // values at point t
  val inits:Map[Double,X] = Map() // starting points of sub-trajectories
  val ends: Map[Double,X] = Map() // ending   points of sub-trajectories
  val notes: Map[Double,String] = Map() // text notes to add at trajectory points.
  val warnings: Map[Double,Set[String]] = Map() // Warnings at trajectory points.
}

//object Traj {
//  // Empty trajectory
//  def empty[A,MA<:Traversable[A]](implicit nil:MA): Traj[A,MA] = new Traj[A,MA] {
//    override val dur: Option[Double] = Some(0)
//    override def apply(t: Double): MA = nil
//  }
//}


//trait Prog2[]

trait Prog[X] {
  def traj(init:X): Traj[X]
  def ++(other:Prog[X]): Prog[X] = Prog.join(this,other)
}


object Prog {

  def join[X](p1:Prog[X],p2:Prog[X]): Prog[X] = (input: X) => {
      val t1 = p1.traj(input)
      t1.dur match {
        // Traj1 runs forever
        case None =>
          t1
        // Traj1 ends at dur1
        case Some(dur1) => new Traj[X] {
          val init2: X = t1(dur1) // returns new valuation for what t1 knows
          val t2: Traj[X] = p2.traj(init2)

          override val inits: Map[Double, X] =
            t1.inits ++ t2.inits.map(p => (p._1+dur1)->p._2)
          override val ends:  Map[Double, X] =
            t1.ends  ++ t2.ends.map(p => (p._1+dur1)->p._2)
          override val notes: Map[Double, String] =
            append(t1.notes,t2.notes.map(p => (p._1+dur1)->p._2))
          override val warnings: Map[Double, Set[String]] =
            appendW(t1.warnings,t2.warnings.map(p => (p._1+dur1)->p._2))

          override val dur: Option[Double] =
            for (dur2 <- t2.dur) yield dur1 + dur2

          override def apply(t: Double): X =
            if (t < dur1) t1(t)
            else t2(t - dur1)
        }
      }
    }
  private def append(m1: Map[Double, String], m2: Map[Double, String]) = {
    var res = m1
    for ((t, s) <- m2)
      res.get(t) match {
        case Some(value) => res += t -> (value + "<br>" + s)
        case None =>res += t->s
      }
    res
  }
  private def appendW(m1: Map[Double, Set[String]], m2: Map[Double, Set[String]]) = {
    var res = m1
    for ((t, s) <- m2)
      res.get(t) match {
        case Some(value) => res += t -> (value ++ s)
        case None =>res += t->s
      }
    res
  }
}

//trait Carrier[+A] extends Traversable[A]
//object Traj {
//  def join
//}
