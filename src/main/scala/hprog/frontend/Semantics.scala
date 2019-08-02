package hprog.frontend

import hprog.ast.SageExpr.{SExpr, SExprFun, SExprT}
import hprog.ast._
import hprog.backend.Show
import hprog.frontend.solver.{SimpleSolver, Solver, StaticSageSolver}

object Semantics {

  // symbolic level
  type Valuation = Map[String,SExpr] // note: SageExpr without variables nor argument
  /** Maps variables to the symbolic expression (function) of its semantics */
  type SageSolution  = Map[String,SExprFun]
  type TimeSolution = Map[String,SExprT]

  // Reals level
  type Point = Map[String,Double]
  /** Maps variables to their semantic functions */
  type Solution  = Map[String,SFunction]
  /** Function, given a time value and an initial valuation */
  type SFunction = Double => Point => Double


  type Warnings = List[(SExpr,String)]
  type Notes    = Warnings


  trait TrajValuation extends Traj[Valuation] {
//    override def apply(t: Double): Point = {
//      val res = apply(SVal(t)).mapValues(Eval(_,t,Map()))
//      debug(()=>s"'${Show(fun)}'($t) = $res")
//      res
//    }
  }

  def syntaxToValuation(syntax:Syntax,
                        solver: StaticSageSolver, // = new SageSolver("/home/jose/Applications/SageMath"),
                        dev: Deviator): Prog[Valuation] = {
    solver.++=(syntax)
    syntaxToValuationAux(syntax,solver,dev,100)
  }

  def syntaxToValuationTaylor(syntax:Syntax,dev:Deviator=Deviator.dummy): Prog[Valuation] = {
    val solver = new SimpleSolver
    syntaxToValuationAux(syntax,solver,dev,100)
  }

  private def syntaxToValuationAux(syntax:Syntax,sol:Solver,dev:Deviator,bound:Int): Prog[Valuation] = {
    //println(s"current bound: $bound for ${Show(syntax)}")
    if (bound<=0) notes("Reached limit of checks of conditions in while loops.")
    else syntax match {
      case a: Assign     => assignmentToValuation(a,sol)
      case d: DiffEqs    => diffEqsToValuation(d, sol)
      case Seq(Nil)      => syntaxToValuationAux(Skip, sol, dev,bound)
      case Seq(p :: Nil) => syntaxToValuationAux(p, sol, dev,bound)
      case Seq(p :: ps)  => syntaxToValuationAux(p, sol, dev,bound).++(
                            syntaxToValuationAux(Seq(ps), sol, dev,bound))(sol)
      case Skip          => skipToValuation()
      case ite: ITE      => iteToValuation(ite, sol, dev, bound)
      case whil: While   => whileToValuation(whil, sol, dev, bound)
    }
  }

  // Assignments
  def assignmentToValuation(assign: Assign, sol: Solver): Prog[Valuation] = input => {
    val Assign(x,exp) = assign
    debug(()=>s"& evaluating ${Show(exp)} under $input")
    val e1 = Eval.lin2sage(exp)
    val newExpr: SExpr = Eval.updInput(e1,input)
    debug(()=>s"& added input: ${Show(newExpr)}")
    val newValue: SExpr = sol.solveSymb(newExpr) // evaluating expression with Sage
    val newValuation: Valuation = input + (x.v -> newValue)
    debug(()=>s"& done: added ${Show(newValue)}")

    new TrajValuation {
      override val dur: Option[SExpr] = Some(SVal(0))
      override def fun(t: SExpr)(implicit s:Solver): TimeSolution = newValuation
      override def apply(t: SExpr)(implicit s:Solver): Valuation = newValuation
      override def apply(t: Double): Point = newValuation.mapValues(Eval(_))
      override val inits: Map[SExpr, Valuation] = Map(SVal(0) -> newValuation)
    }
  }

  // Differential equations
  def diffEqsToValuation(diffEqs: DiffEqs, solver: Solver): Prog[Valuation] = input => {
    val DiffEqs(eqs,durCond) = diffEqs

    val sol = solver.evalFun(eqs) // maps variables to their solutions (function from t/ctx to value)
    def guard(c: Cond): Double => Boolean =
      t => Eval(sol.mapValues(fun=>fun(t)(Eval(input))),c)
    val durValue = Solver.solveDur(durCond,input,guard) // give value or do jumps searching for duration
    debug(()=>s"Solving duration for ${Show(eqs)} & ${Show(diffEqs.dur)} - ${durValue}")
    debug(()=>s"knowing ${Show(input)}")

    // calculate solution for diff eqs using the solver (wrt "t" and initial point)
    debug(()=>s"&& solving ${eqs.size} eq(s): ${Show(eqs)}")
    val symbSol = solver.solveSymb(eqs)

    debug(()=>s"## updating solution for '${Show(symbSol)}'" +
      s"\n## using input ${input.mkString(", ")}" +
      s"\n## got (no simplification) ${
        symbSol.mapValues(expr => Eval.updInputFun(expr,input))
          .mkString(" & ")}")

    val t = new TrajValuation {
      override val dur: Option[SExpr] = durValue.map(SVal)
      override val fun: SageSolution = symbSol
      override val inits: Map[SExpr, Valuation] = Map(SVal(0.0) -> input)
      override val ends: Map[SExpr, Valuation] = dur match {
        case Some(value) => Map(value -> apply(value)(solver))
        case None => Map()
      }

      override def fun(t: SExpr)(implicit s:Solver): TimeSolution =
        input  ++  symbSol.mapValues(expr => {
          val addedInput = Eval.updInputFun(expr,input)
          addedInput
        })

      override def apply(t: SExpr)(implicit s:Solver): Valuation = {
        val newVal = input ++ symbSol.mapValues(expr => {
          val addedTime = Eval.updTime(t, expr)
          val addedInput = Eval.updInput(addedTime, input)
          val res = solver.solveSymb(addedInput)
          debug(()=>s"&& solving expr (from eqs ${Show(eqs)}) @ ${Show(t)}:\n   - ${
            Show(addedTime)}\n   - ${
            Show(addedInput)}\n   - ${
            Show(res)
          }")
          res
        })
        newVal
      }

      override def apply(t: Double): Point =
        input.mapValues(Eval(_))  ++  symbSol.mapValues(expr => {
          debug(()=>s"&& calculating from eqs ${
            Show(eqs)}.")
          debug(()=>s"&& updating ${Show(expr)} @ $t")
          val expr1 = Eval.updTime(SVal(t),expr)
          debug(()=>s"&& updating ${Show(expr1)} with $input")
          val expr2 = Eval.updInput(expr1,input)
          val res = Eval(expr2)
          debug(()=>s"done: $res")
          res
        })
    }
    debug(()=>s"my duration: ${durValue}///${t.dur}")
    t
  }


  def skipToValuation(): Prog[Valuation] = input => {
    new TrajValuation {
      override val dur: Option[SExpr] = Some(SVal(0.0))

      //      override val symbolic: Option[SageSolution] = Some(Eval.baseSage(input.keys))
      override def fun(t: SExpr)(implicit s: Solver): TimeSolution = input
      override def apply(t: SExpr)(implicit s:Solver): Valuation = input
      override def apply(t: Double): Point = Eval(input)
    }
  }

  def iteToValuation(ite: ITE,sol: Solver, dev:Deviator, bound:Int): Prog[Valuation] = input => {
    val ITE(ifS, thenS, elseS) = ite

    val inputPoint = input.mapValues(Eval(_,0)) // needed for searching for deviations

    //val ifValue = Eval(inputPoint,ifS) // approximation (assuming input is simplified)
    val ifValue = sol.solveSymb(ifS,input)


    debug(()=>s"%%% ITE ${Show(ifS)} @ ${inputPoint.mkString(",")} - $ifValue")

    val warnings: Warnings = {
      val notIf = if (ifValue) Not(ifS) else ifS
      val cls = dev.closest(inputPoint, notIf)
      debug(()=>s"%%% closest point: $cls")
      cls match {
        case Some(p2) =>
          if (Eval(input)!=p2)
            List(SVal(0) -> s"Perturbation by ${
              Distance.dist(inputPoint,p2)}</br>when testing ${
              Show(ifS)}</br>with:</br>${
              p2.map(kv=>s"${kv._1}:${kv._2}").mkString("</br>")}")
          else
            List(SVal(0) -> s"Perturbation found by any small delta</br>when testing ${Show(ifS)}.")
        case None => Nil
      }
      ///////// OVERRIDING
//      val msg = s"Iftrue? $ifValue</br>IfStm: ${ifS}</br>Symb. fun: $symb</br>Symb. res: ${
//        symb.get.mapValues(exp => Eval(exp,0.0,Map()))}"
//      val toCheck: Set[(Cond,SageSolution)] = symb match {
//        case Some(sol) => Set((if(ifValue)ifS else Not(ifS)
//                              ,sol.mapValues(e=>Eval.setTime(0.0,e))))
//        case None => Set()
//      }
//      Map(0.0 -> (Set(msg) , toCheck) )
    }

    if (ifValue)
      syntaxToValuationAux(thenS,sol,dev,bound).traj(input)
        .addNotes(List(SVal(0)->s"${Show(ifS)}? True"))
        .addWarnings(warnings)
    else
      syntaxToValuationAux(elseS,sol,dev,bound).traj(input)
        .addNotes(List(SVal(0)->s"${Show(ifS)}? False"))
        .addWarnings(warnings)
  }

  def whileToValuation(whileStx:While, sol:Solver, dev:Deviator, bound:Int): Prog[Valuation] = {
//    val While(c,doP) = whileStx
    whileStx match {
      case While(Guard(c),doP) => syntaxToValuationAux(ITE(c,doP ~ whileStx, Skip),sol,dev,bound-1)
      case While(Counter(0),_) => syntaxToValuationAux(Skip,sol,dev,bound)
      case While(Counter(i),doP) => syntaxToValuationAux(doP ~ While(Counter(i-1),doP),sol,dev,bound)
    }

  }

  // Leftovers - now only used to inform that a trace was trimmed.
  private def notes(str: String): Prog[Valuation] = _ => {
    val t = new TrajValuation {
      override val dur: Option[SExpr] = Some(SVal(0))
      override def fun(t: SExpr)(implicit s:Solver): TimeSolution= Map()
      override def apply(t: SExpr)(implicit s:Solver): Valuation = Map()
      override def apply(t: Double): Point = Map()
    }
    t.addNotes(List(SVal(0)->str))
  }

  private def debug(s:()=>String): Unit = {
    //println("[Sem] "+s())
  }
}
