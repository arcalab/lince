package hprog.frontend

import hprog.ast._
import hprog.backend.Show
import hprog.lang.SageParser

object Semantics {

  /**
    *
    */
  type Vars      = String
  /** Maps names of variables or trajectories to their value at a given time */
  type Valuation = Map[Vars,Double]
  /** Maps variables to their semantic functions */
  type Solution  = Map[String,SFunction]
  /** Function, given a time value and an initial valuation */
  type SFunction = Double => Valuation => Double
  /** Solution and variables for a system of equations */
  case class SolVars(vars:Set[String],sol: Solution) {
    def ++(that:SolVars): SolVars = {
      SolVars( vars ++ that.vars , sol ++ that.sol )
    }
  }



  def syntaxToValuation(syntax:Syntax,
                        solver: Solver = new SageSolver("/home/jose/Applications/SageMath")): Prog[Valuation] = {
//    val solver = new SageSolver("/home/jose/Applications/SageMath")
    solver.++=(syntax)
    syntaxToValuationAux(syntax,solver)
  }

  def syntaxToValuationTaylor(syntax:Syntax): Prog[Valuation] = {
    val solver = new Solver{
      override def ++=(systems: List[List[DiffEq]]): Unit = {}
      override def ++=(syntax: Syntax): Unit = {}
      override def +=(eqs: List[DiffEq]): Unit = {}
      override def get(eqs: List[DiffEq]): Solution = {
        val vars = Solver.getVars(eqs).filterNot(_.startsWith("_"))
        vars.map(v=> v -> ( (t:Double) => (init:Valuation) =>
          callTaylorSolver(init,eqs)(t)(v)
        )).toMap
      }
    }
    solver.++=(syntax)
    syntaxToValuationAux(syntax,solver)
  }

  private def syntaxToValuationAux(syntax:Syntax,sol:Solver): Prog[Valuation] = syntax match {
    case a:Assign    => assignmentToValuation(a)
    case d:DiffEqs   => diffEqsToValuation(d,sol)
    case Seq(Nil)    => syntaxToValuationAux(Skip,sol)
    case Seq(p::Nil) => syntaxToValuationAux(p,sol)
    case Seq(p::ps)  => syntaxToValuationAux(p,sol) ++ syntaxToValuationAux(Seq(ps),sol)
    case Skip        => skipToValuation()
    case ite:ITE     => iteToValuation(ite,sol)
    case whil:While  => whileToValuation(whil,sol)
  }

  def assignmentToValuation(assign: Assign): Prog[Valuation] = input => {
      val Assign(x,exp) = assign
      val newValue: Double = Eval(input, exp)
      val newValuation: Valuation = input + (x.v -> newValue)

      new Traj[Valuation] {
        override val dur: Option[Double] = Some(0)
        override def apply(t: Double): Valuation = newValuation
        override val inits: Map[Double, Valuation] = Map(0.0 -> newValuation)
      }

  }

  def diffEqsToValuation(diffEqs: DiffEqs, solver: Solver): Prog[Valuation] = input => {
    val DiffEqs(eqs,dur) = diffEqs

//    val sol = callSageSolver(input,eqs,sol)
    val sol = solver.get(eqs) // maps variables to their solutions (function from t/ctx to value)
    def guard(c: Cond): Double => Boolean =
      t => Eval(sol.mapValues(fun=>fun(t)(input)),c)
    val durValue = Solver.solveDur(dur,input,guard)

    new Traj[Valuation] {
      override val dur: Option[Double] = durValue
      override val inits: Map[Double, Valuation] = Map(0.0 -> input)
      override val ends: Map[Double, Valuation] = durValue match {
        case Some(value) => Map(value -> apply(value))
        case None => Map()
      }

      override def apply(t: Double): Valuation = input ++ sol.mapValues(fun=>fun(t)(input))
    }
  }


  private def callTaylorSolver(input:Valuation, eqs:List[DiffEq]): Double => Valuation = {
    val (vars,mtx): (List[String],List[List[Double]]) = Solver.getMatrix(eqs)
    val sol1: (List[Double],Double) => List[Double] = Solver.solveTaylorManual(mtx)

//    println(s"## calling solver" +
//      s"\neqs:\n  ${eqs.mkString("\n  ")}" +
//      s"\ninput: ${input.map(p=>s"${p._1}->${p._2}").mkString(", ")}" +
//      s"\nvars: ${vars.mkString(",")}" +
//      s"\nmtx:\n  ${mtx.map(_.mkString("\t")).mkString("\n  ")}")
    //println("## Sage\n"+genSage(eqs))
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

  def iteToValuation(ite: ITE,sol: Solver): Prog[Valuation] = input => {
    val ITE(ifS, thenS, elseS) = ite
    def note(txt:String): Prog[Valuation] = input => {
      new Traj[Valuation] {
        override val dur: Option[Double] = Some(0)
        override def apply(t: Double): Valuation = input
        override val notes: Map[Double, String] = Map(0.0->txt)
      }
    }
    if (Eval(input, ifS))
      note(s"${Show(ifS)}? True").++(syntaxToValuationAux(thenS,sol)).traj(input)
    else
      note(s"${Show(ifS)}? False").++(syntaxToValuationAux(elseS,sol)).traj(input)
  }

  def whileToValuation(whileStx: While, sol: Solver): Prog[Valuation] = {
//    val While(c,doP) = whileStx
    whileStx match {
      case While(Guard(c),doP) => syntaxToValuationAux(ITE(c,doP ~ whileStx, Skip),sol)
      case While(Counter(0),doP) => syntaxToValuationAux(Skip,sol)
      case While(Counter(i),doP) => syntaxToValuationAux(doP ~ While(Counter(i-1),doP),sol)
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
