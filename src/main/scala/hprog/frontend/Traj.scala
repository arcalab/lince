package hprog.frontend

import hprog.ast.{BVal, Cond}
import hprog.backend.Show

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
  def warnings(pre:Cond): Map[Double,Set[String]] = Map() // Warnings at trajectory points under pre-conditions.
//  val pre:  Cond = BVal(true)  // pre-condition - what is assumed to hold initially
  val post: Cond = BVal(true) // post-condition - what is guaranteed by the trajectory

  def addWarnings(w: Cond=>Map[Double,Set[String]]): Traj[X] = {
    val t = this
    new Traj[X] {
      override val dur: Option[Double] = t.dur
      override def apply(x:Double): X  = t.apply(x)
      override val inits:Map[Double,X] = t.inits
      override val ends: Map[Double,X] = t.ends
      override val notes: Map[Double,String] = t.notes
      override val post: Cond = t.post
      // copy all but 'warnings'
      override def warnings(pre:Cond): Map[Double,Set[String]] =
        Prog.appendW(t.warnings(pre),w(pre))
    }
//    t
  }

  def addNotes(ns:Map[Double,String]): Traj[X] = {
    val t = this
    new Traj[X] {
      override val dur: Option[Double] = t.dur
      override def apply(x:Double): X  = t.apply(x)
      override val inits:Map[Double,X] = t.inits
      override val ends: Map[Double,X] = t.ends
      override def warnings(pre:Cond): Map[Double,Set[String]] = t.warnings(pre)
      override val post: Cond = t.post
      // copy all but 'notes'
      override val notes: Map[Double,String] =
        Prog.appendN(t.notes,ns)
    }
//    t
  }
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
//  def mapPre(f:Cond => Cond): Prog[X] = {
//    val parent = this
//    new Prog[X] {
//      override def traj(input: X): Traj[X] = {
//        val oldTraj = parent.traj(input)
//
//        new Traj[X] {
//          override val dur: Option[Double] = oldTraj.dur
//
//          override def apply(t: Double): X = oldTraj.apply(t)
//
//          override val inits: Map[Double, X] = oldTraj.inits
//          override val ends: Map[Double, X] = oldTraj.ends
//          override val notes: Map[Double, String] = oldTraj.notes
//          // update warnings
//          override val warnings: Map[Double, Set[String]] = oldTraj.warnings
//          override val pre: Cond = f(oldTraj.pre)
//          // update post-conditions
//          override val post: Cond =
//            if (dur == Some(0.0)) pre && oldTraj.post
//            else oldTraj.post
//          //        override val post: Cond = parent.traj(input).post
//          //        override val pre: Cond = parent.traj(input).pre
//        }
//      }
//    }
//  }
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
          val input2: X = t1(dur1) // returns new valuation for what t1 knows
          // adding t1.post to p2 pre-conditions
          // val t2: Traj[X] = p2.mapPre(t1.post && _).traj(input2)
          val t2: Traj[X] = p2.traj(input2)

          // println(s"joined after ${dur1} with conditions ${Show(t1.post)}")


          override val inits: Map[Double, X] =
            t1.inits ++ t2.inits.map(p => (p._1+dur1)->p._2)
          override val ends:  Map[Double, X] =
            t1.ends  ++ t2.ends.map(p => (p._1+dur1)->p._2)
          override val notes: Map[Double, String] =
            appendN(t1.notes,t2.notes.map(p => (p._1+dur1)->p._2))
          override def warnings(pre: Cond): Map[Double, Set[String]] =
            appendW(t1.warnings(pre),t2.warnings(t1.post).map(p => (p._1+dur1)->p._2))
          //override val pre: Cond  = t1.pre
          override val post: Cond = t2.post

          override val dur: Option[Double] =
            for (dur2 <- t2.dur) yield dur1 + dur2

          override def apply(t: Double): X =
            if (t < dur1) t1(t)
            else t2(t - dur1)
        }
      }
    }
  def appendN(m1: Map[Double, String], m2: Map[Double, String]): Map[Double, String] = {
    var res = m1
    for ((t, s) <- m2)
      res.get(t) match {
        case Some(value) => res += t -> (value + "</br>" + s)
        case None =>res += t->s
      }
    res
  }
  def appendW(m1: Map[Double, Set[String]], m2: Map[Double, Set[String]]): Map[Double, Set[String]] = {
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
