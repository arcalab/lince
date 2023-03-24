package hprog.frontend

import hprog.ast.SymbolicExpr.{SyExpr, SyExprAll, SyExprTime, SyExprVar}






object CommonTypes {

  // symbolic level
  type Valuation      = Map[String,SyExpr] // note: SageExpr without variables nor argument
  /** Maps variables to the symbolic expression (function) of its semantics */
  type SySolution     = Map[String,SyExprAll]
  type SySolutionTime = Map[String,SyExprTime]
  type SySolutionVar  = Map[String,SyExprVar]

  // Reals level
  type Point = Map[String,Double]
  //  /** Function, given a time value and an initial valuation */
  type SFunction = Double => Point => Double
  /** Maps variables to their semantic functions */
  type Solution  = Map[String,SFunction]

  // extra information for trajectories
  type Warnings = Set[(SyExpr,String)]
  type Notes    = Set[(SyExpr,String)]






















//
//
//
//
//
//
//  trait TrajValuation extends Traj[Valuation] {
////    override def apply(t: Double): Point = {
////      val res = apply(SVal(t)).mapValues(Eval(_,t,Map()))
////      debug(()=>s"'${Show(fun)}'($t) = $res")
////      res
////    }
//  }
//
//  def syntaxToValuation(syntax:Syntax,
//                        solver: StaticSageSolver, // = new SageSolver("/home/jose/Applications/SageMath"),
//                        dev: Deviator): Prog[Valuation] = {
//    solver ++= syntax // load diff eqs to the solver
//    val free = Utils.getFreeVars(syntax)
//    if (free.nonEmpty)
//      throw new ParserException(s"Progam not closed. Free variables found: ${free.mkString(",")}")
//    syntaxToValuationBounded(syntax,solver,dev,100)
//  }
//
//  def syntaxToValuationTaylor(syntax:Syntax,dev:Deviator=Deviator.dummy): Prog[Valuation] = {
//    val solver = new SimpleSolver
//    syntaxToValuationBounded(syntax,solver,dev,100)
//  }
//
//
//  private def syntaxToValuationBounded(syntax:Syntax,sol:Solver,dev:Deviator,bound:Int): Prog[Valuation] = {
//    //println(s"current bound: $bound for ${Show(syntax)}")
//    if (bound<=0) notes("Reached limit of checks of conditions in while loops.")
//    else syntax match {
//      case Atomic(as, de) => atomicToValuation2(as,de,sol,dev,bound)
//      case Seq(Atomic(as, de),p) => seqAtToValuation2(as,de,p,sol,dev,bound)
//      case While(pre, d, doP) => whileToValuation2(pre,d,doP,sol,dev,bound)
//      case ITE(ifP, thenP, elseP) => iteToValuation2(ifP,thenP,elseP,sol,dev,bound)
//      case Seq(p, q) => seqToValuation2(p,q,sol,dev,bound)
//      //      case a: Assign     => assignmentToValuation(a,sol)
//      //      case d: DiffEqs    => diffEqsToValuation(d, sol)
//      //      case Seq(Nil)      => syntaxToValuationBounded(Skip, sol, dev,bound)
//      //      case Seq(p :: Nil) => syntaxToValuationBounded(p, sol, dev,bound)
//      //      case Seq(p :: ps)  => syntaxToValuationBounded(p, sol, dev,bound).++(
//      //                            syntaxToValuationBounded(Seq(ps), sol, dev,bound))(sol)
//      //      case Skip          => skipToValuation()
//      //      case ite: ITE      => iteToValuation(ite, sol, dev, bound)
//      //      case whil: While   => whileToValuation(whil, sol, dev, bound)
//    }
//  }
//
//  private def applyAssignments(as:List[Assign],s:Solver,input:Valuation): Valuation = {
//    var newValuation: Valuation = input
//    for (a <- as) {
//      val Assign(x,exp) = a
//      debug(()=>s"& evaluating ${Show(exp)} under $input")
//      val e1 = Eval.lin2sage(exp)
//      val newExpr: SyExpr = Eval.updInput(e1,input)
//      debug(()=>s"& added input: ${Show(newExpr)}")
//      val newValue: SyExpr = s.solveSymb(newExpr) // evaluating expression with Sage
//      newValuation += (x.v -> newValue)
//      debug(()=>s"& done: added ${Show(newValue)}")
//    }
//    newValuation
//  }
//
//  def atomicToValuation2(as: List[Assign], de: DiffEqs, s: Solver, dev: Deviator, bound: Int)
//      : Prog[Valuation] = input0 => {
//    val input = applyAssignments(as,s,input0)
//
//    val DiffEqs(eqs,durCond) = de
//
//    val durValue = durCond match {
//      case For(t) => Some(t.v)
//      case Forever => None
//      case Until(c) => // experimental/numerical
//        val sol = s.evalFun(eqs) // maps variables to their solutions (function from t/ctx to value)
//        def guard(c: Cond): Double => Boolean =
//          t => Eval(sol.mapValues(fun=>fun(t)(Eval(input))),c)
//        val durV = Solver.searchCond(c,guard) // give value or do jumps searching for duration
//        debug(()=>s"Solving duration for ${Show(eqs)} & ${Show(de.dur)} - ${durV}")
//        debug(()=>s"knowing ${Show(input)}")
//        durV
//    }
//
//    // calculate solution for diff eqs using the solver (wrt "t" and initial point)
//    debug(()=>s"&& solving ${eqs.size} eq(s): ${Show(eqs)}")
//    val phi = s.solveSymb(eqs)
//
//    debug(()=>s"## updating solution for '${Show(phi)}'" +
//      s"\n## using input ${input.mkString(", ")}" +
//      s"\n## got (no simplification) ${
//        phi.mapValues(expr => Eval.updInputFun(expr,input))
//          .mkString(" & ")}")
//
//    debug(()=>s"my duration: ${durValue}")
//
//    new TrajValuation {
//      override val dur: Option[SyExpr] = durValue.map(SVal)
////      override val fun: SageSolution = phi
//      override val inits: Map[SyExpr, Valuation] = Map(SVal(0.0) -> input)
//      override val ends: Map[SyExpr, Valuation] = dur match {
//        case Some(value) => Map(value -> apply(value)(s))
//        case None => Map()
//      }
//
//      override def fun(t: SyExpr)(implicit s:Solver): SySolutionTime =
//        input  ++  updInputFun(input,phi)
//
//      /// Core or rules SOLVE
//      override def apply(t: SyExpr)(implicit s:Solver): Valuation = {
//        val newVal = input ++
//           solveValues(s,
//             updInput(input,
//               updTime(t,phi)))
//
//        debug(()=>s"&& solved expr (from eqs ${Show(eqs)}) @ ${Show(t)}:")
//        phi.mapValues(expr =>
//          debug(()=>s"\n   - ${
//            Show(expr)
//          }")
//        )
//
//        newVal
//      }
//
////      override def apply(t: Double): Point =
////        input.mapValues(Eval(_))  ++  phi.mapValues(expr => {
////          debug(()=>s"&& calculating from eqs ${
////            Show(eqs)}.")
////          debug(()=>s"&& updating ${Show(expr)} @ $t")
////          val expr1 = Eval.updTime(SVal(t),expr)
////          debug(()=>s"&& updating ${Show(expr1)} with $input")
////          val expr2 = Eval.updInput(expr1,input)
////          val res = Eval(expr2)
////          debug(()=>s"done: $res")
////          res
////        })
//    }
//  }
//
//  def seqAtToValuation2(as: List[Assign], de: DiffEqs, p: Syntax, s: Solver, dev: Deviator, bound: Int)
//      : Prog[Valuation] = input => {
//    val newVal = applyAssignments(as,s,input)
//
//  }
//
//
//
//
//
//
//
//
//
//
//
//
//
//
//
//
//
//
//
//
//
//
//
//
//
//
//
//
//  // Assignments
//  def assignmentToValuation(assign: Assign, sol: Solver): Prog[Valuation] = input => {
//    val Assign(x,exp) = assign
//    debug(()=>s"& evaluating ${Show(exp)} under $input")
//    val e1 = Eval.lin2sage(exp)
//    val newExpr: SyExpr = Eval.updInput(e1,input)
//    debug(()=>s"& added input: ${Show(newExpr)}")
//    val newValue: SyExpr = sol.solveSymb(newExpr) // evaluating expression with Sage
//    val newValuation: Valuation = input + (x.v -> newValue)
//    debug(()=>s"& done: added ${Show(newValue)}")
//
//    new TrajValuation {
//      override val dur: Option[SyExpr] = Some(SVal(0))
//      override def fun(t: SyExpr)(implicit s:Solver): SySolutionTime = newValuation
//      override def apply(t: SyExpr)(implicit s:Solver): Valuation = newValuation
//      override def apply(t: Double): Point = newValuation.mapValues(Eval(_))
//      override val inits: Map[SyExpr, Valuation] = Map(SVal(0) -> newValuation)
//    }
//  }
//
//  // Differential equations
//  def diffEqsToValuation(diffEqs: DiffEqs, solver: Solver): Prog[Valuation] = input => {
//    val DiffEqs(eqs,durCond) = diffEqs
//
//    val sol = solver.evalFun(eqs) // maps variables to their solutions (function from t/ctx to value)
//    def guard(c: Cond): Double => Boolean =
//      t => Eval(sol.mapValues(fun=>fun(t)(Eval(input))),c)
//    val durValue = Solver.searchCond(durCond,input,guard) // give value or do jumps searching for duration
//    debug(()=>s"Solving duration for ${Show(eqs)} & ${Show(diffEqs.dur)} - ${durValue}")
//    debug(()=>s"knowing ${Show(input)}")
//
//    // calculate solution for diff eqs using the solver (wrt "t" and initial point)
//    debug(()=>s"&& solving ${eqs.size} eq(s): ${Show(eqs)}")
//    val symbSol = solver.solveSymb(eqs)
//
//    debug(()=>s"## updating solution for '${Show(symbSol)}'" +
//      s"\n## using input ${input.mkString(", ")}" +
//      s"\n## got (no simplification) ${
//        symbSol.mapValues(expr => Eval.updInputFun(expr,input))
//          .mkString(" & ")}")
//
//    val t = new TrajValuation {
//      override val dur: Option[SyExpr] = durValue.map(SVal)
//      override val fun: SySolution = symbSol
//      override val inits: Map[SyExpr, Valuation] = Map(SVal(0.0) -> input)
//      override val ends: Map[SyExpr, Valuation] = dur match {
//        case Some(value) => Map(value -> apply(value)(solver))
//        case None => Map()
//      }
//
//      override def fun(t: SyExpr)(implicit s:Solver): SySolutionTime =
//        input  ++  symbSol.mapValues(expr => {
//          val addedInput = Eval.updInputFun(expr,input)
//          addedInput
//        })
//
//      override def apply(t: SyExpr)(implicit s:Solver): Valuation = {
//        val newVal = input ++ symbSol.mapValues(expr => {
//          val addedTime = Eval.updTime(t, expr)
//          val addedInput = Eval.updInput(addedTime, input)
//          val res = solver.solveSymb(addedInput)
//          debug(()=>s"&& solving expr (from eqs ${Show(eqs)}) @ ${Show(t)}:\n   - ${
//            Show(addedTime)}\n   - ${
//            Show(addedInput)}\n   - ${
//            Show(res)
//          }")
//          res
//        })
//        newVal
//      }
//
//      override def apply(t: Double): Point =
//        input.mapValues(Eval(_))  ++  symbSol.mapValues(expr => {
//          debug(()=>s"&& calculating from eqs ${
//            Show(eqs)}.")
//          debug(()=>s"&& updating ${Show(expr)} @ $t")
//          val expr1 = Eval.updTime(SVal(t),expr)
//          debug(()=>s"&& updating ${Show(expr1)} with $input")
//          val expr2 = Eval.updInput(expr1,input)
//          val res = Eval(expr2)
//          debug(()=>s"done: $res")
//          res
//        })
//    }
//    debug(()=>s"my duration: ${durValue}///${t.dur}")
//    t
//  }
//
//
//  def skipToValuation(): Prog[Valuation] = input => {
//    new TrajValuation {
//      override val dur: Option[SyExpr] = Some(SVal(0.0))
//
//      //      override val symbolic: Option[SageSolution] = Some(Eval.baseSage(input.keys))
//      override def fun(t: SyExpr)(implicit s: Solver): SySolutionTime = input
//      override def apply(t: SyExpr)(implicit s:Solver): Valuation = input
//      override def apply(t: Double): Point = Eval(input)
//    }
//  }
//
//  def iteToValuation(ite: ITE,sol: Solver, dev:Deviator, bound:Int): Prog[Valuation] = input => {
//    val ITE(ifS, thenS, elseS) = ite
//
//    val inputPoint = input.mapValues(Eval(_,0)) // needed for searching for deviations
//
//    //val ifValue = Eval(inputPoint,ifS) // approximation (assuming input is simplified)
//    val ifValue = sol.solveSymb(ifS,input)
//
//
//    debug(()=>s"%%% ITE ${Show(ifS)} @ ${inputPoint.mkString(",")} - $ifValue")
//
//    val warnings: Warnings = {
//      val notIf = if (ifValue) Not(ifS) else ifS
//      val cls = dev.closest(inputPoint, notIf)
//      debug(()=>s"%%% closest point: $cls")
//      cls match {
//        case Some(p2) =>
//          if (Eval(input)!=p2)
//            List(SVal(0) -> s"Perturbation by ${
//              Distance.dist(inputPoint,p2)}</br>when testing ${
//              Show(ifS)}</br>with:</br>${
//              p2.map(kv=>s"${kv._1}:${kv._2}").mkString("</br>")}")
//          else
//            List(SVal(0) -> s"Perturbation found by any small delta</br>when testing ${Show(ifS)}.")
//        case None => Nil
//      }
//      ///////// OVERRIDING
////      val msg = s"Iftrue? $ifValue</br>IfStm: ${ifS}</br>Symb. fun: $symb</br>Symb. res: ${
////        symb.get.mapValues(exp => Eval(exp,0.0,Map()))}"
////      val toCheck: Set[(Cond,SageSolution)] = symb match {
////        case Some(sol) => Set((if(ifValue)ifS else Not(ifS)
////                              ,sol.mapValues(e=>Eval.setTime(0.0,e))))
////        case None => Set()
////      }
////      Map(0.0 -> (Set(msg) , toCheck) )
//    }
//
//    if (ifValue)
//      syntaxToValuationBounded(thenS,sol,dev,bound).traj(input)
//        .addNotes(List(SVal(0)->s"${Show(ifS)}? True"))
//        .addWarnings(warnings)
//    else
//      syntaxToValuationBounded(elseS,sol,dev,bound).traj(input)
//        .addNotes(List(SVal(0)->s"${Show(ifS)}? False"))
//        .addWarnings(warnings)
//  }
//
//  def whileToValuation(whileStx:While, sol:Solver, dev:Deviator, bound:Int): Prog[Valuation] = {
////    val While(c,doP) = whileStx
//    whileStx match {
//      case While(Guard(c),doP) => syntaxToValuationBounded(ITE(c,doP ~ whileStx, Skip),sol,dev,bound-1)
//      case While(Counter(0),_) => syntaxToValuationBounded(Skip,sol,dev,bound)
//      case While(Counter(i),doP) => syntaxToValuationBounded(doP ~ While(Counter(i-1),doP),sol,dev,bound)
//    }
//
//  }
//
//  // Leftovers - now only used to inform that a trace was trimmed.
//  private def notes(str: String): Prog[Valuation] = _ => {
//    val t = new TrajValuation {
//      override val dur: Option[SyExpr] = Some(SVal(0))
//      override def fun(t: SyExpr)(implicit s:Solver): SySolutionTime= Map()
//      override def apply(t: SyExpr)(implicit s:Solver): Valuation = Map()
//      override def apply(t: Double): Point = Map()
//    }
//    t.addNotes(List(SVal(0)->str))
//  }
//
//  private def debug(s:()=>String): Unit = {
//    //println("[Sem] "+s())
//  }
}
