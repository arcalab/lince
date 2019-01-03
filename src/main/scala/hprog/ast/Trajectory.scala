package hprog.ast

import Trajectory.{list2map, map2list}
import hprog.frontend.Eval
import hprog.frontend.Solver.solve

abstract class Trajectory() {
  val sup:Option[Double]
  val vars: List[String]
  def function(init:List[Double],t:Double): List[Double]


  def apply(t:Double): Map[String,Double] =
    functionM(Map(),t)

  def apply(init:Iterable[(String,Double)],t:Double): Map[String,Double] =
    functionM(init.toMap,t)

  def apply(init:(String,Double)*)(t:Double): Map[String,Double] =
    functionM(init.toMap,t)

  def apply(init:(String,Int)*)(t:Int): Map[String,Double] =
    functionM(init.toMap.mapValues(_.toDouble),t.toDouble)

  override def toString: String = s"Trajectory($sup,${vars.mkString(",")},_)"

  /**
    *
    * @return auxiliary function using maps instead of lists
    */
  def functionM(init:Map[String,Double],t:Double): Map[String,Double] = {
//    println(s"fun / vars=${vars.mkString(",")}; init=${init.mkString(",")}; map2list=${map2list(vars,init).mkString(",")}; f(m2l)=${function(map2list(vars,init),t).mkString(",")}, f=$toString")
    list2map(vars, function(map2list(vars, init), t))
  }

  /**
    * Sequential composition of trajectories
    * @param that other trajectory to compose
    * @return composed trajectories
    */
  def ++(that:Trajectory): Trajectory = Trajectory.join(this,that)

  def until(newSup:Option[Double]): Trajectory = {
    val tr = this
    new Trajectory {
      override val sup: Option[Double] = newSup
      override val vars: List[String] = tr.vars
      override def function(i: List[Double], t: Double): List[Double] = tr.function(i,t)
      override def functionM(i:Map[String,Double],t:Double): Map[String,Double] = tr.functionM(i,t)
    }
  }
}

object Trajectory {
  /*
    * A function receives initial values of variables, and given a time t returns the updated values of the variables
    */
//  type Fun = Map[String,Double] => Double => Map[String,Double]

  def list2map[A](vars:Iterable[String],l:Iterable[A]): Map[String,A] =
//    (for ((v,i) <- vars.zipWithIndex) yield (v,l.toList(i))).toMap
    (vars zip l).toMap
  def map2list(vars:Iterable[String],m:Map[String,Double]): List[Double] =
    for ( v  <-  vars.toList) yield m.getOrElse(v,0.0)

  def buildFromList(supx:Option[Double],varsx: List[String],
            functionx: List[Double] => Double => List[Double]): Trajectory =
    new Trajectory {
      override val sup: Option[Double] = supx
      override val vars: List[String] = varsx

//      override def function(init:Map[String,Double],t:Double): Map[String,Double] =
//        list2map(vars,functionx(map2list(vars,init))(t))
      override def function(init:List[Double],t: Double): List[Double] = functionx(init)(t)
    }

  def buildFromMap(supx:Option[Double],varsx: List[String],
            functionx:  Map[String,Double] => Double => Map[String,Double]): Trajectory =
    new Trajectory {
      override val sup: Option[Double] = supx
      override val vars: List[String] = varsx

      override def functionM(init:Map[String,Double],t:Double): Map[String,Double] =
        functionx(init)(t)
      override def function(init:List[Double],t: Double): List[Double] =
        map2list(vars,functionx(list2map(varsx,init))(t))
    }


  /**
    * Sequential composition of two trajectories
    * @param t1 first trajectory
    * @param t2 second trajectory
    * @return new trajectory, combining both
    */
  def join(t1: Trajectory, t2: Trajectory): Trajectory = new Trajectory {
    override val sup: Option[Double] =
      for (s1 <- t1.sup; s2 <- t2.sup) yield s1+s2
    override val vars: List[String] = (t1.vars ++ t2.vars).distinct

    /**
      * Call function from t1 or t2, after adjusting the list to the new set of variables
      * @param init starting point
      * @param t point in time
      * @return point reached
      */
    override def function(init: List[Double], t: Double): List[Double] = t1.sup match {
      case Some(value) =>
        val init1 = refine(init,t1.vars,vars)
        if (t<value) {
//          println(s"entering $t1")
//          println(s"adjusting input ${init.mkString(",")} to ${init1.mkString(",")}")
//          println(s"got ${t1.function(init1,t)}")
          val res = t1.function(init1,t)
          expand(res,t1.vars,init,vars)
        }
        else {
          val res1 = t1.function(init1,value)
          val init2 = expand(res1,t1.vars,init,vars)
//          println(s"moving to $t2")
//          println(s"adjusting ${res1.mkString(",")} to ${ init2.mkString(",")}")
          val res = t2.function(refine(init2,t2.vars,vars),t-value)
//          println(s"got $res")
          expand(res,t2.vars,init,vars)
        }
      case None => t1.function(refine(init,t1.vars,vars),t)
    }
    private def refine(point:List[Double], someVars:List[String], allVars:List[String]): List[Double] = {
      if (point.size != allVars.size)
        throw new RuntimeException(s"point ${point.mkString("[",",","]")} does not match variables"+
                                   s" in ${allVars.mkString("[",",","]")}")
      val pointMap= (allVars zip point).toMap
      for (v <- someVars) yield pointMap(v)
    }
    private def expand(point:List[Double], someVars:List[String], init:List[Double], allVars:List[String]): List[Double] = {
      if (point.size != someVars.size)
        throw new RuntimeException(s"point ${point.mkString("[",",","]")} does not match variables"+
          s" in ${allVars.mkString("[",",","]")}")
      val someMap = (someVars zip point).toMap
      val allMap  = (allVars zip init).toMap
      val updated = allMap ++ someMap
      val res = for (v <- allVars) yield updated(v)
      println(s"adjust2: from $someMap to $res")
      res
    }
  }

  def empty: Trajectory = new Trajectory {
    override val sup: Option[Double] = Some(0.0)
    override val vars: List[String] = Nil

    override def function(init: List[Double], t: Double): List[Double] = init
  }


  /**
    * Traverse the program, starting from some variable assignment, building a trajectory
    * @param state
    * @param prog
    * @return
    */
  def hprogToTraj(state:Map[String,Double],prog:Prog): (Trajectory,Map[String,Double]) = prog match {
    case Assign(v, e) =>
      val newv = Eval(state.withDefaultValue(0),e).get // if an unknown variable is found, assume 0 (always succeeds)
      val f = (l:List[Double])=>(t:Double) => List(newv)
      val tr = Trajectory.buildFromList(Some(0.0),List(v.v),f)
      println(s"# updated state: ${v.v} -> $newv")
      (tr,state + (v.v -> newv))

    case d@DiffEqs(eqs, dur) =>
      val tr = solve(state,d)
      tr.sup match {
        case Some(value) =>
          println(s"# calculating traj from : ${state.mkString(", ")} FOR $value.")
          println(s"# updated state: ${tr(state,value).mkString(", ")}.")
          (tr,state ++ tr.apply(state,value) )// update state with tr at end value
        case None => (tr,state)
      }
    case Seq(Nil) => (Trajectory.empty,state)
    case Seq(p::Nil) => hprogToTraj(state,p)
    case Seq(p::ps) =>
      val (t1,st1) = hprogToTraj(state,p)
      t1.sup match {
        case Some(value) =>
          val (t2,st2) = hprogToTraj(st1,Seq(ps))
          (t1 ++ t2,st2)
        case None => (t1,st1)
      }
    case Skip => (Trajectory.empty,state)
    case ITE(ifP, thenP, elseP) => throw new RuntimeException("ITE not supported yet")
    case While(c, doP) => throw new RuntimeException("WHILE not supported yet")
  }


}