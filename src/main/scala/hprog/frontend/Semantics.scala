package hprog.frontend

import hprog.ast._
import hprog.backend.Show
import hprog.frontend.solver.{SimpleSolver, Solver}
import hprog.lang.SageParser

object Semantics {

  /**
    *
    */
  type Vars      = String
  /** Maps names of variables or trajectories to their value at a given time */
  type Valuation = Map[Vars,Double]

  /** Maps variables to the symbolic expression of its semantics */
  type SageSolution  = Map[String,SageExpr]
  /** Solution and variables for a system of equations */
  case class SolVars(sol: SageSolution) //{
//    def ++(that:SolVars): SolVars = {
//      SolVars( vars ++ that.vars , sol ++ that.sol )
//    }
//  }

  type Messages = Set[String]
  type ToVerify = Set[(Cond,SageSolution)]
  type Warnings = Map[Double,(Messages,ToVerify)]

  /** Maps variables to their semantic functions */
  type Solution  = Map[String,SFunction]
  /** Function, given a time value and an initial valuation */
  type SFunction = Double => Valuation => Double


  def syntaxToValuation(syntax:Syntax,
                        solver: Solver , // = new SageSolver("/home/jose/Applications/SageMath"),
                        dev: Deviator): Prog[Valuation] = {
//    val solver = new SageSolver("/home/jose/Applications/SageMath")
    solver.++=(syntax)
    syntaxToValuationAux(syntax,solver,dev,100)
  }

  def syntaxToValuationTaylor(syntax:Syntax,dev:Deviator=Deviator.dummy): Prog[Valuation] = {
    val solver = new SimpleSolver
//        Solver{
//      override def ++=(systems: List[List[DiffEq]]): Unit = {}
//      override def ++=(syntax: Syntax): Unit = {}
//      override def +=(eqs: List[DiffEq]): Unit = {}
//      override def get(eqs: List[DiffEq]): Solution = {
//        val vars = Solver.getVars(eqs).filterNot(_.startsWith("_"))
//        vars.map(v=> v -> ( (t:Double) => (init:Valuation) =>
//          callTaylorSolver(init,eqs)(t)(v)
//        )).toMap
//      }
//      override def getSage(eqs: List[DiffEq]): SageSolution = Map()
//    }
    solver.++=(syntax)
    syntaxToValuationAux(syntax,solver,dev,100)
  }

  private def syntaxToValuationAux(syntax:Syntax,sol:Solver,dev:Deviator,bound:Int): Prog[Valuation] = {
    //println(s"current bound: $bound for ${Show(syntax)}")
    if (bound<=0) notes("Reached limit of checks of conditions in while loops.")
    else syntax match {
      case a: Assign => assignmentToValuation(a)
      case d: DiffEqs => diffEqsToValuation(d, sol)
      case Seq(Nil) => syntaxToValuationAux(Skip, sol, dev,bound)
      case Seq(p :: Nil) => syntaxToValuationAux(p, sol, dev,bound)
      case Seq(p :: ps) => syntaxToValuationAux(p, sol, dev,bound) ++ syntaxToValuationAux(Seq(ps), sol, dev,bound)
      case Skip => skipToValuation()
      case ite: ITE => iteToValuation(ite, sol, dev, bound)
      case whil: While => whileToValuation(whil, sol, dev, bound)
    }
  }

  def assignmentToValuation(assign: Assign): Prog[Valuation] = input => {
      val Assign(x,exp) = assign
      val newValue: Double = Eval(input, exp)
      val newValuation: Valuation = input + (x.v -> newValue)

      new Traj[Valuation] {
        override val dur: Option[Double] = Some(0)
        override val symbolic: Option[SageSolution] =
          Some(Eval.baseSage(input.keys) ++ Map(x.v -> Eval.lin2sage(exp)))
        override def apply(t: Double): Valuation = newValuation
        override val inits: Map[Double, Valuation] = Map(0.0 -> newValuation)
      }

  }

  def diffEqsToValuation(diffEqs: DiffEqs, solver: Solver): Prog[Valuation] = input => {
    val DiffEqs(eqs,durCond) = diffEqs

//    val sol = callSageSolver(input,eqs,sol)
    val sol = solver.evalFun(eqs) // maps variables to their solutions (function from t/ctx to value)
    def guard(c: Cond): Double => Boolean =
      t => Eval(sol.mapValues(fun=>fun(t)(input)),c)
    val durValue = Solver.solveDur(durCond,input,guard) // give value or do jumps searching for duration
    val symbolicValue = Some(Eval.baseSage(input.keys) ++ solver.solveSymb(eqs))

    new Traj[Valuation] {
      override val dur: Option[Double] = durValue
      override val symbolic: Option[SageSolution] = symbolicValue
      override val inits: Map[Double, Valuation] = Map(0.0 -> input)
      override val ends: Map[Double, Valuation] = durValue match {
        case Some(value) => Map(value -> apply(value))
        case None => Map()
      }
      override val post: Cond = durCond match {
        case Until(c:Cond) => c
        case _ => BVal(true)
      }

      override def apply(t: Double): Valuation = input ++ sol.mapValues(fun=>fun(t)(input))
    }
  }


//  private def callTaylorSolver(input:Valuation, eqs:List[DiffEq]): Double => Valuation = {
//    val (vars,mtx): (List[String],List[List[Double]]) = Solver.getMatrix(eqs)
//    val sol1: (List[Double],Double) => List[Double] = Solver.solveTaylorManual(mtx)
//
////    println(s"## calling solver" +
////      s"\neqs:\n  ${eqs.mkString("\n  ")}" +
////      s"\ninput: ${input.map(p=>s"${p._1}->${p._2}").mkString(", ")}" +
////      s"\nvars: ${vars.mkString(",")}" +
////      s"\nmtx:\n  ${mtx.map(_.mkString("\t")).mkString("\n  ")}")
//    //println("## Sage\n"+genSage(eqs))
//    //println(s"calling solver for ${input} and ${eqs.map(Show(_)).mkString(",")}")
//    def sol(t:Double): Valuation = {
//      // "input" should have all variables but no "" - this should be assigned to 0
//      def getDummy(v:String): Double = (vars.indexOf(v),vars.indexOf("_"+v)) match {
//        case (_,-1) => 0.0
//        case (i,j)  =>
////          println(s"dummy($v) = ${mtx(i)(j)}")
//          //mtx(i)(j)
//          1
//      }
//      val dummies = vars.map(v => ("_"+v) -> getDummy(v))
//      val input2  = vars.map(input ++ dummies)
////      println("input with dummies: "+input2.mkString(","))
//      val list = sol1(input2, t)
//      (vars zip list).toMap -- vars.map("_"+_)
//    }
//    sol
//  }

  def skipToValuation(): Prog[Valuation] = input => {
    new Traj[Valuation] {
      override val dur: Option[Double] = Some(0)
      override val symbolic: Option[SageSolution] = Some(Eval.baseSage(input.keys))
      override def apply(t: Double): Valuation = input
    }
  }

  def iteToValuation(ite: ITE,sol: Solver, dev:Deviator, bound:Int): Prog[Valuation] = input => {
    val ITE(ifS, thenS, elseS) = ite
    val ifValue = Eval(input,ifS)

//    def warnings(pre:Cond): Map[Double,Set[String]] =
//      if (Utils.getDomain(ifS).isEmpty) Map(0.0 -> Set(s"Failed to find an non-ambiguous domain for ${Show(ifS)}"))
//      else deviate(input,ifS,pre,eps) match {
//        case Some(str) => Map(0.0 -> Set(str))
//        case _ => Map()
//      }
    // when asking for warnings, this will be called with the current symbolic value
    def warnings(symb:Option[SageSolution]): Warnings = {
      val notIf = if (ifValue) Not(ifS) else ifS
      dev.closest(input, notIf) match {
        case Some(p2) =>
          if (input!=p2) Map(0.0 -> Set(s"Perturbation by ${Distance.dist(input,p2)}</br>when testing ${Show(ifS)}</br>with:</br>${
            p2.map(kv=>s"${kv._1}:${kv._2}").mkString("</br>")}"))
          else Map(0.0 -> Set(s"Perturbation found by any small delta</br>when testing ${Show(ifS)}."))
        case None => Map()
      }
      ///////// OVERRIDING
      val msg = s"Iftrue? $ifValue</br>IfStm: ${ifS}</br>Symb. fun: $symb</br>Symb. res: ${
        symb.get.mapValues(exp => Eval(exp,0.0,Map()))}"
      val toCheck: Set[(Cond,SageSolution)] = symb match {
        case Some(sol) => Set((if(ifValue)ifS else Not(ifS)
                              ,sol.mapValues(e=>Eval.setTime(0.0,e))))
        case None => Set()
      }
      Map(0.0 -> (Set(msg) , toCheck) )
    }


    if (ifValue)
      syntaxToValuationAux(thenS,sol,dev,bound).traj(input)
        .addNotes(Map(0.0->s"${Show(ifS)}? True"))
        .addWarnings(warnings)
    else
      syntaxToValuationAux(elseS,sol,dev,bound).traj(input)
        .addNotes(Map(0.0->s"${Show(ifS)}? False"))
        .addWarnings(warnings)
  }

  def whileToValuation(whileStx:While, sol:Solver, dev:Deviator, bound:Int): Prog[Valuation] = {
//    val While(c,doP) = whileStx
    whileStx match {
      case While(Guard(c),doP) => syntaxToValuationAux(ITE(c,doP ~ whileStx, Skip),sol,dev,bound-1)
      case While(Counter(0),doP) => syntaxToValuationAux(Skip,sol,dev,bound)
      case While(Counter(i),doP) => syntaxToValuationAux(doP ~ While(Counter(i-1),doP),sol,dev,bound)
    }

  }

//  private def deviate(input:Valuation,pred:Cond,pre:Cond,eps:Double): Option[String] = {
//    if (eps==0.0) None
//    else {
//      val vars = Utils.getVars(pred)
//      //println(s"verifying deviations in\n  ${Show(pred)}\n  when\n   ${Show(pre)}\n   for $input")
//      val ref = Eval(input, pred) // reference value - Boolean
//      runCombinations(input, pred, pre, eps, ref, vars, vars.toList)
//    }
//  }
//
//  private def runCombinations(input:Valuation,pred:Cond,pre:Cond,eps:Double
//                             ,ref:Boolean,allVars:Set[String],vars:List[String])
//                             : Option[String] = vars match {
//    case (hd::tl) =>
//      val inp2 = input + (hd -> (input(hd)+eps))
//      runCombinations(inp2,pred,pre,eps,ref,allVars,tl) match {
//        case None =>
//          val inp3 = input + (hd -> (input(hd)-eps))
//          runCombinations(inp3,pred,pre,eps,ref,allVars,tl)
//        case res => res
//      }
//    case Nil =>
//      //if (!Eval(input,pre))
//      //  println(s"OUT OF BOUNDS - skipping ${Show(pred)} since ${Show(pre)} does not hold for ${input}")
//      //else
//      //  println(s"Not skipping ${Show(pred)} since ${Show(pre)} holds for ${input}")
//      if (!Eval(input,pre)  ||  (Eval(input,pred) == ref)) None
//      else Some(s"Deviation found for </br> ${Show(pred)} </br>when</br> ${
//        input
//          .filter(p=>allVars contains (p._1))
//          .map(p=>s"${p._1}=${p._2}")
//          .mkString(",")
//    }")
//  }

  // Leftovers - now only used to inform that a trace was trimmed.
  private def notes(str: String): Prog[Valuation] = _ => {
    val t = new Traj[Valuation] {
      override val dur: Option[Double] = Some(0.0)
      override def apply(t: Double): Valuation = Map()

      override val symbolic: Option[SageSolution] = None
    }
    t.addNotes(Map(0.0->str))
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
