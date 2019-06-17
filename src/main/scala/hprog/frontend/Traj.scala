package hprog.frontend

import hprog.ast.{BVal, Cond}
import hprog.backend.Show
import hprog.frontend.Semantics.{SageSolution, Warnings}

/**
  *
  * @tparam X type of the value at a given time - e.g., [[hprog.frontend.Semantics.Valuation]].
  */
trait Traj[X] {
  // duration
  val dur: Option[Double]
  def apply(t:Double): X  // values at point t
  val symbolic: Option[SageSolution] // symbolic expression

  val inits:Map[Double,X] = Map() // starting points of sub-trajectories
  val ends: Map[Double,X] = Map() // ending   points of sub-trajectories
  val notes: Map[Double,String] = Map() // text notes to add at trajectory points.
  def warnings(pre:Option[SageSolution]): Warnings = Map() // Warnings at trajectory points under pre-conditions.
//  val pre:  Cond = BVal(true)  // pre-condition - what is assumed to hold initially
  val post: Cond = BVal(true) // post-condition - what is guaranteed by the trajectory

  def addWarnings(w: Option[SageSolution]=>Warnings): Traj[X] = {
    val t = this
    new Traj[X] {
      override def warnings(pre:Option[SageSolution]): Warnings =
        Prog.appendW(t.warnings(pre),w(pre))
      // rest is the same
      override val dur: Option[Double] = t.dur
      override def apply(x:Double): X  = t.apply(x)
      override val symbolic: Option[SageSolution] = t.symbolic
      override val inits:Map[Double,X] = t.inits
      override val ends: Map[Double,X] = t.ends
      override val notes: Map[Double,String] = t.notes
      override val post: Cond = t.post

    }
//    t
  }

  def addNotes(ns:Map[Double,String]): Traj[X] = {
    val t = this
    new Traj[X] {
      override val notes: Map[Double,String] =
        Prog.appendN(t.notes,ns)
      // rest is the same
      override val dur: Option[Double] = t.dur
      override def apply(x:Double): X  = t.apply(x)
      override val symbolic: Option[SageSolution] = t.symbolic
      override val inits:Map[Double,X] = t.inits
      override val ends: Map[Double,X] = t.ends
      override def warnings(pre:Option[SageSolution]): Warnings = t.warnings(pre)
      override val post: Cond = t.post
    }

//    t
  }
  def addSymbolic(r:Double ,s:Option[SageSolution]): Traj[X] = {
    val t = this
    new Traj[X] {
      override val symbolic: Option[SageSolution] = (t.symbolic,s) match {
        case (Some(s1),Some(s2)) => Some(Eval.compose(r,s1,s2))
        case _ => None
      }
      // rest is the same
      override val dur: Option[Double] = t.dur
      override def apply(x:Double): X  = t.apply(x)
      override val inits:Map[Double,X] = t.inits
      override val ends: Map[Double,X] = t.ends
      override def warnings(pre:Option[SageSolution]): Warnings = t.warnings(pre)
      override val post: Cond = t.post
      override val notes: Map[Double,String] = t.notes
    }
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
          val t2: Traj[X] = p2
            .traj(input2)
//            .addSymbolic(dur1,t1.symbolic)

          // println(s"joined after ${dur1} with conditions ${Show(t1.post)}")


          override val inits: Map[Double, X] =
            t1.inits ++ t2.inits.map(p => (p._1+dur1)->p._2)
          override val ends:  Map[Double, X] =
            t1.ends  ++ t2.ends.map(p => (p._1+dur1)->p._2)
          override val notes: Map[Double, String] =
            appendN(t1.notes,t2.notes.map(p => (p._1+dur1)->p._2))
          override def warnings(pre: Option[SageSolution]): Warnings =
            appendW(t1.warnings(pre)
                   ,t2.warnings(Eval.compose(dur1,pre,t1.symbolic)
                                    .map(sol=>sol.mapValues(exp=>Eval.addTime(dur1,exp))))
                      .map(p => (p._1+dur1)->p._2))
          //override val pre: Cond  = t1.pre
          override val post: Cond = t2.post

          override val dur: Option[Double] =
            for (dur2 <- t2.dur) yield dur1 + dur2

          override val symbolic: Option[SageSolution] =
            Eval.compose(dur1,t1.symbolic,t2.symbolic)
            .map(sol => sol.mapValues(expr => Eval.shiftTime(dur1,expr)))

          override def apply(t: Double): X =
            if (t < dur1) t1(t)
            else t2(t - dur1)
        }
      }
    }
  def appendN(m1: Map[Double, String], m2: Map[Double, String]): Map[Double, String] =
    mergeMap(m1,m2,(_:String)+"</br>"+(_:String))

  def appendW(m1: Warnings, m2: Warnings): Warnings = {
    def join(p1:(Set[String],Set[(Cond,SageSolution)]),
             p2:(Set[String],Set[(Cond,SageSolution)])): (Set[String],Set[(Cond,SageSolution)]) =
      ((p1._1)++p2._1,p1._2++p2._2)
    mergeMap(m1,m2,join)
  }

  def mergeMap[A,B](m1:Map[A,B],m2:Map[A,B],f:(B,B)=>B): Map[A,B] = {
    var res = m1
    for ((t, s) <- m2)
      res.get(t) match {
        case Some(value) => res += t -> f(value,s)
        case None => res += t -> s
      }
    res
  }
}

//trait Carrier[+A] extends Traversable[A]
//object Traj {
//  def join
//}
