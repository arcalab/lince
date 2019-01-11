package hprog.frontend

/**
  *
  * @tparam A
  */
trait Traj[X] {
  // duration
  val dur: Option[Double]
  def apply(t:Double): X
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

          override val dur: Option[Double] =
            for (dur2 <- t2.dur) yield dur1 + dur2

          override def apply(t: Double): X =
            if (t < dur1) t1(t)
            else t2(t - dur1)
        }
      }
    }
}

//trait Carrier[+A] extends Traversable[A]
//object Traj {
//  def join
//}
