package hprog.frontend

import hprog.ast._

object Semantics {

  /**
    *
    */
  type Vars      = String
  /**
    * Maps names of variables or trajectories to their value at a given time
    */
  type Valuation = Map[Vars,Double]

  def syntaxToValuation(syntax:Syntax): Prog[Valuation] = syntax match {
    case a:Assign    => assignmentToValuation(a)
    case d:DiffEqs   => diffEqsToValuation(d)
    case Seq(Nil)    => syntaxToValuation(Skip)
    case Seq(p::Nil) => syntaxToValuation(p)
    case Seq(p::ps)  => syntaxToValuation(p) ++ syntaxToValuation(Seq(ps))
    case Skip        => skipToValuation()
    case ite:ITE     => iteToValuation(ite)
    case whil:While  => whileToValuation(whil)
  }

  def assignmentToValuation(assign: Assign): Prog[Valuation] = input => {
      val Assign(x,exp) = assign
      val newValue: Double = Eval(input, exp)
      val newValuation: Valuation = input + (x.v -> newValue)

      new Traj[Valuation] {
        override val dur: Option[Double] = Some(0)
        override def apply(t: Double): Valuation = newValuation
      }

  }

  def diffEqsToValuation(diffEqs: DiffEqs): Prog[Valuation] = input => {
    val DiffEqs(eqs,dur) = diffEqs

    val sol = callSolver(input,eqs)
    def guard(c: Cond): Double => Boolean =
      t => Eval(sol(t),c)
    val durValue = Solver.solveDur(dur,input,guard)

    new Traj[Valuation] {
      override val dur: Option[Double] = durValue

      override def apply(t: Double): Valuation = input ++ sol(t)
    }
  }

  /**
    * Experimental: generating code to be sent to Sage
    * @param eqs
    * @return
    */
  def genSage(eqs:List[DiffEq]): String = {
    var res = "t = var('t')\n"
    for (e <- eqs)
      res += s"${e.v.v} = function('${e.v.v}')(t)\n"
    for ((e,i) <- eqs.zipWithIndex)
      res += s"de$i = diff(${e.v.v},t) == ${Show(e.e)}\n"
    res += s"print(expand(desolve_system([${(for(i<-0 until eqs.size)yield "de"+i).mkString(",")}]," +
           s"[${eqs.map(_.v.v).mkString(",")}])))"
    res
  }

  private def callSolver(input:Valuation, eqs:List[DiffEq]): Double => Valuation = {
    val (vars,mtx): (List[String],List[List[Double]]) = Solver.getMatrix(eqs)
    val sol1: (List[Double],Double) => List[Double] = Solver.solveTaylorManual(mtx)

//    println(s"## calling solver" +
//      s"\neqs:\n  ${eqs.mkString("\n  ")}" +
//      s"\ninput: ${input.map(p=>s"${p._1}->${p._2}").mkString(", ")}" +
//      s"\nvars: ${vars.mkString(",")}" +
//      s"\nmtx:\n  ${mtx.map(_.mkString("\t")).mkString("\n  ")}")
    println("## Sage\n"+genSage(eqs))
    //println(s"calling solver for ${input} and ${eqs.map(Show(_)).mkString(",")}")
    def sol(t:Double): Valuation = {
      // "input" should have all variables but no "" - this should be assigned to 0
      def getDummy(v:String): Double = (vars.indexOf(v),vars.indexOf("_"+v)) match {
        case (_,-1) => 0.0
        case (i,j)  =>
//          println(s"dummy($v) = ${mtx(i)(j)}")
          //mtx(i)(j)
          1
      }
      val dummies = vars.map(v => ("_"+v) -> getDummy(v))
      val input2  = vars.map(input ++ dummies)
//      println("input with dummies: "+input2.mkString(","))
      val list = sol1(input2, t)
      (vars zip list).toMap -- vars.map("_"+_)
    }
    sol
  }

  def skipToValuation(): Prog[Valuation] = input => {
    new Traj[Valuation] {
      override val dur: Option[Double] = Some(0)
      override def apply(t: Double): Valuation = input
    }
  }

  def iteToValuation(ite: ITE): Prog[Valuation] = input => {
    val ITE(ifS, thenS, elseS) = ite

    if (Eval(input, ifS))
      syntaxToValuation(thenS).traj(input)
    else
      syntaxToValuation(elseS).traj(input)
  }

  def whileToValuation(whileStx: While): Prog[Valuation] = {
//    val While(c,doP) = whileStx
    whileStx match {
      case While(Guard(c),doP) => syntaxToValuation(ITE(c,doP ~ whileStx, Skip))
      case While(Counter(0),doP) => syntaxToValuation(Skip)
      case While(Counter(i),doP) => syntaxToValuation(doP ~ While(Counter(i-1),doP))
    }

  }



  //      // Evaulate "e" under the current valuation
//      val newv = Eval(current.withDefaultValue(0),e).get
//      val f = (l:List[Double])=>(t:Double) => List(newv)
//      val tr = Trajectory.buildFromList(Some(0.0),List(v.v),f)
//      println(s"# updated state: ${v.v} -> $newv")
//      (tr,current + (v.v -> newv))
//
//    case d@DiffEqs(eqs, dur) =>
//      val tr = solve(current,d)
//      println(s"# solved traj.: $tr")
//      tr.sup match {
//        case Some(value) =>
//          //          println(s"# calculated traj from: ${state.mkString(", ")} FOR $value.")
//          println(s"# updated state: ${tr(current,value).mkString(", ")}.")
//          (tr,current ++ tr.apply(current,value) )// update state with tr at end value
//        case None => (tr,current)
//      }
//    case Seq(Nil) => (Trajectory.empty,current)
//    case Seq(p::Nil) => hprogToTraj(current,p)
//    case Seq(p::ps) =>
//      val (t1,st1) = hprogToTraj(current,p)
//      t1.sup match {
//        case Some(value) =>
//          val (t2,st2) = hprogToTraj(st1,Seq(ps))
//          (t1 ++ t2,st2)
//        case None => (t1,st1)
//      }
//    case Skip => (Trajectory.empty,current)
//    case ITE(ifP, thenP, elseP) => throw new RuntimeException("ITE not supported yet")
//    case While(c, doP) => throw new RuntimeException("WHILE not supported yet")
//  }
}
