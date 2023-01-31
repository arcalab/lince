package hprog.frontend

import hprog.ast.SymbolicExpr.{SyExpr, SyExprAll, SyExprVar}
import hprog.ast._
import Syntax._
import hprog.backend.Show
import hprog.frontend.CommonTypes.Valuation




// Scrip with auxiliar functions 



object Utils {

  /////////////////////////////////////////////////////////////////////////////////////////////////////

  // take content of type syntax and return content of type List[List[DiffEq]]
  // Basically what this function does is to remove all differential equations from a program
  def getDiffEqs(prog:Syntax): List[List[DiffEq]]  = prog match {
    case Atomic(_, DiffEqs(eqs,_)) => List(eqs)
    case Seq(p, q) => getDiffEqs(p) ::: getDiffEqs(q) // joins the two lists
    case ITE(_, thenP, elseP) => getDiffEqs(thenP) ::: getDiffEqs(elseP)
    case While(pre, _, doP) => getDiffEqs(pre) ::: getDiffEqs(doP)
  }

///////////////////////////////////////////////////////////////////////////////////////////////////////



// New
def extractAssigments(prog:Syntax):List[Assign] = prog match {
  
  case Atomic(as,_) => {
  	return as
  }
  case Seq(Atomic(as,_),q) => {
  	var ac=as++extractAssigments(q)
  	return ac
  }
  case Seq(p,q) =>{
  
   return extractAssigments(p) ++ extractAssigments(q)
  }
  case While(pre,c,p) => {
  	return extractAssigments(pre)
  
  }
  case _ =>  List()


}




////// New /////// 
//verify if the free varibles had already been declarated before being used.
  def assigmentsVerify(prog:Syntax): Set[String] = prog match {
    
    case Seq(p,q) => {
      var as=extractAssigments(p) ++ extractAssigments(q)
      var declVar= as.map(_.v.v).toList //list of declarated variables in atomic
      var aux=0
      var aux2=1
      var zz:Set[String]=Set()
      for (i <- as){
 
        var z=getVars(i.e)   // Set of used variables in the atribution of other veriable in atomic
        // remotion of existing variables in Z that have been declareted 
        for (j <- 0 until (aux) by 1){
          z -= declVar(j)
        }
        
        zz=zz++z
        aux=aux+1
        
        }
       
       return zz	
    
    }
    case Atomic(as,_)=>{
      var declVar= as.map(_.v.v).toList //list of declarated variables in atomic
      var aux=0
      var aux2=1
      var zz:Set[String]=Set()
      
      for (i <- as){
 
        var z=getVars(i.e)   // Set of used variables in the atribution of other veriable in atomic
        // remotion of existing variables in Z that have been declareted 
        for (j <- 0 until (aux) by 1){
          z -= declVar(j)
        }

        // if Z stayed empty, it is because the variables had already been declareted, if not no        
        
        zz=zz++z
        aux=aux+1
        
        }
       
       return zz
    }   
    case ITE(ifP,thenP,elseP) =>getVars(ifP)++assigmentsVerify(thenP)++assigmentsVerify(elseP) //probably this case does not have any effect because it is obligatory the declaration of variables above the instructions 
    case While(pre,c,p) => assigmentsVerify(pre)  
  }





///////////////////////////////////////////////////////////////////////////////////////////////////////
  /**
    * Collect the free variables, following the "free variable rules" (Lince paper)
    * @param prog where to search for the first free variables
    * @return the free variables of the first atomic expression
    */

  // What this function does is to get p:=3+v and get Set(v), but it only cares about the first set of atomic
  def getFstFreeVars(prog:Syntax): Set[String] = prog match {
    case Atomic(as, _) => as.toSet.flatMap((a:Assign)=>getVars(a.e))
    case Seq(p, _) => getFstFreeVars(p)
    case ITE(ifP, thenP, elseP) => getVars(ifP) ++ getFstFreeVars(thenP) ++ getFstFreeVars(elseP)
    case While(pre, _, _) => getFstFreeVars(pre)
  }

////////////////////////////////////////////////////////////////////////////////////////////////////////





// Verify if exists free variables  already been declarated before being used, and also if they are used variables that are not declareted
  def isClosed(prog:Syntax): Either[String,Unit] = {
    val declVarTHEN = getFstDeclVarsTHEN(prog) //make a set with the firsts declareted variables (THEN)
     
    val declVarELSE = getFstDeclVarsELSE(prog) //make a set with the firsts declareted variables (ELSE)
    
    val usedVarsTHEN = getUsedVarsTHEN(prog)   //make a set with the firsts used variables
    
    val usedVarsELSE = getUsedVarsELSE(prog)  //make a set with the firsts used variables
    
    val asVerify=assigmentsVerify(prog) //make a set of free variables that not had been declareted before of ther invocation.
    
    if (asVerify.nonEmpty) //Verify if exist free variables that not had been declareted before, if exist i print it.
      Left(s"Initial declaration has free variables that were not declared: ${asVerify.mkString(", ")}")
    else if (!usedVarsTHEN.forall(declVarTHEN)) 
      Left(s"Variable(s) not declared: ${((usedVarsTHEN -- declVarTHEN)++(usedVarsELSE-- declVarELSE)).mkString(", ")}")
    else if (!usedVarsELSE.forall(declVarELSE))
       Left(s"Variable(s) not declared: ${((usedVarsTHEN -- declVarTHEN)++(usedVarsELSE-- declVarELSE)).mkString(", ")}")
    else
      Right(())
  }















////////////////////////////////////////////////////////////////////////////////////////////////////////

  // take a list of differential equations and remove the variables with the tilde on top! (ex: p'=1+x returns set(p))
  def getDefVars(eqs: List[DiffEq]): Set[String] =
    eqs.map(_.v.v).toSet

////////////////////////////////////////////////////////////////////////////////////////////////////////










///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

//New
  // The function getUsedVars is defined for List[DiffEq],Syntax,DiffEqs and Dur, returning the variables used there
  def getUsedVarsTHEN(eqs: List[DiffEq]): Set[String] =
    eqs.flatMap(eq => getVars(eq.e)+eq.v.v).toSet  // New

  def getUsedVarsTHEN(prog:Syntax): Set[String] = prog match {
    case Atomic(as, de) => as.toSet.flatMap((a:Assign)=>getVars(a.e)+a.v.v) ++ getUsedVarsTHEN(de)
    case Seq(p, q) => getUsedVarsTHEN(p) ++ getUsedVarsTHEN(q)
    case ITE(ifP, thenP, _) => getVars(ifP) ++ getUsedVarsTHEN(thenP) 
    case While(pre, d, doP) => getUsedVarsTHEN(pre) ++ getVars(d) ++ getUsedVarsTHEN(doP)
  }

  def getUsedVarsTHEN(eqs: DiffEqs): Set[String] =
    getUsedVarsTHEN(eqs.eqs) ++ getUsedVarsTHEN(eqs.dur)

  def getUsedVarsTHEN(dur: Dur): Set[String] = dur match {
    case Until(c,_,_) =>getVars(c)
    case For(nl) => getVars(nl) //New  
    case _ => Set()
  }

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////



//New
 def getUsedVarsELSE(eqs: List[DiffEq]): Set[String] =
    eqs.flatMap(eq => getVars(eq.e)+eq.v.v).toSet  //New

  def getUsedVarsELSE(prog:Syntax): Set[String] = prog match {
    case Atomic(as, de) => as.toSet.flatMap((a:Assign)=>getVars(a.e)+a.v.v) ++ getUsedVarsELSE(de)
    case Seq(p, q) => getUsedVarsELSE(p) ++ getUsedVarsELSE(q)
    case ITE(ifP, _, elseP) => getVars(ifP) ++ getUsedVarsELSE(elseP)
    case While(pre, d, doP) => getUsedVarsELSE(pre) ++ getVars(d) ++ getUsedVarsELSE(doP)
  }

  def getUsedVarsELSE(eqs: DiffEqs): Set[String] =
    getUsedVarsELSE(eqs.eqs) ++ getUsedVarsELSE(eqs.dur)

  def getUsedVarsELSE(dur: Dur): Set[String] = dur match {
    case Until(c,_,_) =>getVars(c)
    case For(nl) => getVars(nl) //New 
    case _ => Set()
  }








////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


// New
  // Take a program and remove the first declared variables (from the first atomic)
  //@scala.annotation.tailrec
  def getFstDeclVarsTHEN(prog:Syntax): Set[String] = prog match {
    
    case Seq(p,q) => {
      var as=extractAssigments(Seq(p,q))
      var asSet=as.map(_.v.v).toSet
      
      return asSet
      }
    case Atomic(a, _)     => a.map(_.v.v).toSet // creates an set of the first variables declared
   
    
      
    case ITE(_, thenP, _) => getFstDeclVarsTHEN(thenP) 
    
    case While(pre, _, _) => getFstDeclVarsTHEN(pre)
  }

/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////



//New
  def getFstDeclVarsELSE(prog:Syntax): Set[String] = prog match {
    case Atomic(a, _)     => a.map(_.v.v).toSet // creates an set of the first variables declared
   
    case Seq(p,q) => {
      var as=extractAssigments(p) ++ extractAssigments(q)
      var asSet=as.map(_.v.v).toSet
      return asSet
      }
    case ITE(_, _, elseP) =>getFstDeclVarsELSE(elseP) 
    case While(pre, _, _) => getFstDeclVarsELSE(pre)
  }









/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// create sets with the variables used there
  def getVars(guard: LoopGuard): Set[String] = guard match {
    case Counter(_) => Set()
    case Guard(c) => getVars(c)
  }

  //new
  def getVars(lin: Lin): Set[String] = lin match {
    case Var(v) => Set(v)
    case Value(_) => Set()
    case Add(l1, l2) => getVars(l1) ++ getVars(l2)
    case Mult(l1, l2) => getVars(l1) ++ getVars(l2)
  }

    // new
  def getVars(notlin: NotLin): Set[String] = notlin match {
    case VarNotLin(v) => Set(v)
    case ValueNotLin(_) => Set()
    case AddNotLin(l1, l2) => getVars(l1) ++ getVars(l2)
    case MultNotLin(l1, l2) => getVars(l1) ++ getVars(l2)
    case DivNotLin(l1,l2) => getVars(l1) ++ getVars(l2)
    case ResNotLin(l1,l2) => getVars(l1) ++ getVars(l2)  
    case PowNotLin(l1,l2) => getVars(l1) ++ getVars(l2)
    case FuncNotLin(s,list) => getVarsAux(list)
  }


  def getVarsAux(list:List[NotLin]): Set[String] = list match {
    case List() => Set()
    case n::List() => getVars(n)
    case n::ns => getVars(n) ++ getVarsAux(ns)
  }

  def getVars(cond: Cond): Set[String] = cond match {
    case BVal(_)    => Set()
    case And(c1,c2) => getVars(c1) ++ getVars(c2)
    case Or(c1,c2)  => getVars(c1) ++ getVars(c2)
    case Not(c)     => getVars(c)
    case EQ(l1,l2)    => getVars(l1) ++ getVars(l2)
    case GT(l1,l2)    => getVars(l1) ++ getVars(l2)
    case LT(l1,l2)    => getVars(l1) ++ getVars(l2)
    case GE(l1,l2)    => getVars(l1) ++ getVars(l2)
    case LE(l1,l2)    => getVars(l1) ++ getVars(l2)
    
  }

/////////////////////////////////////////////////////////////////////////////////////////////////////////////







///// New

/////////////////////////////////////////////////////////////////////////////////

// create lists with the variables used there
  def getVarsList(guard: LoopGuard): List[String] = guard match {
    case Counter(_) => List()
    case Guard(c) => getVarsList(c)
  }

  // new
  def getVarsList(lin: Lin): List[String] = lin match {
    case Var(v) => List(v)
    case Value(_) => List()
    case Add(l1, l2) => getVarsList(l1) ++ getVarsList(l2)
    case Mult(l1, l2) => getVarsList(l1) ++ getVarsList(l2)
  }

    // new
  def getVarsList(notlin: NotLin): List[String] = notlin match {
    case VarNotLin(v) => List(v)
    case ValueNotLin(_) => List()
    case AddNotLin(l1, l2) => getVarsList(l1) ++ getVarsList(l2)
    case MultNotLin(l1, l2) => getVarsList(l1) ++ getVarsList(l2)
    case DivNotLin(l1,l2) => getVarsList(l1) ++ getVarsList(l2)
    case ResNotLin(l1,l2) => getVarsList(l1) ++ getVarsList(l2) 
    case PowNotLin(l1,l2) => getVarsList(l1) ++ getVarsList(l2)
    case FuncNotLin(s,list) => getVarsListAux(list)
    
  }

  def getVarsListAux(list:List[NotLin]): List[String] = list match {
    case List() => List()
    case n::List() => getVarsList(n)
    case n::ns => getVarsList(n) ++ getVarsListAux(ns)
  }  

  def getVarsList(cond: Cond): List[String] = cond match {
    case BVal(_)    => List()
    case And(c1,c2) => getVarsList(c1) ++ getVarsList(c2)
    case Or(c1,c2)  => getVarsList(c1) ++ getVarsList(c2)
    case Not(c)     => getVarsList(c)
    case EQ(l1,l2)    => getVarsList(l1) ++ getVarsList(l2)
    case GT(l1,l2)    => getVarsList(l1) ++ getVarsList(l2)
    case LT(l1,l2)    => getVarsList(l1) ++ getVarsList(l2)
    case GE(l1,l2)    => getVarsList(l1) ++ getVarsList(l2)
    case LE(l1,l2)    => getVarsList(l1) ++ getVarsList(l2)
    
  }


/////////////////////////////////////////////////////////////////////////////////

















////////////////////////////////////////////////////////////////////////////////

/** Convert a list of assignments to a Valuation, i.e., to a Map[String,SyExpr]. */
  def toValuation(as:List[Assign],prev:Valuation): Valuation = {
    as.map(kv => kv.v.v -> Eval.notlin2sage(kv.e))
      .toMap
      .view.mapValues(e => exprVarToExpr(e,prev)).toMap
  }

///////////////////////////////////////////////////////////////////////////////







//////////////////////////////////////////////////////////////////////////////////////

  /** Convert a SyExprVar variable to a SyExpr variable. */
  def exprVarToExpr(e:SyExprVar,prev:Valuation): SyExpr = e match {
    case SVal(v) => SVal(v) // because SVal is already an extension of SyExprVar
    case SVar(v) => prev(v) // v is a string, used as a key to the PREVious values (SyExpr) stored in prev.
    case SFun(f, args:List[SyExprVar]) => SFun(f,args.map(exprVarToExpr(_,prev)))
    case SDiv(e1, e2) => SDiv( exprVarToExpr(e1,prev),exprVarToExpr(e2,prev))
    case SRes(e1, e2) => SRes( exprVarToExpr(e1,prev),exprVarToExpr(e2,prev))
    case SMult(e1, e2)=> SMult(exprVarToExpr(e1,prev),exprVarToExpr(e2,prev))
    case SPow(e1, e2) => SPow( exprVarToExpr(e1,prev),exprVarToExpr(e2,prev))
    case SAdd(e1, e2) => SAdd( exprVarToExpr(e1,prev),exprVarToExpr(e2,prev))
    case SSub(e1, e2) => SSub( exprVarToExpr(e1,prev),exprVarToExpr(e2,prev))
  }

/////////////////////////////////////////////////////////////////////////////////////




  def asSyExpr(e:SyExprAll): SyExpr = e match {
    case e2: SyExpr @ unchecked => e2
    // bottom case never caught, since erasure will make SyExprAll = SyExpr.
    // (Runtime error will be different.)
    case _ => throw new RuntimeException(s"Failed to interpret ${Show(e)} as a simple expression.")
  }

  def asSyExprVar(e:SyExprAll): SyExprVar = e match {
    case e2: SyExprVar @ unchecked => e2
    // bottom case never caught, since erasure will make SyExprAll = SyExpr.
    // (Runtime error will be different.)
    case _ => throw new RuntimeException(s"Failed to interpret ${Show(e)} as a simple expression with variables.")
  }




  //////
  // inferring open domains...
  //////




  def fixVars(e:SyExprAll): SyExprAll = e match {
    case SVal(_) => e
    case SVar(_) => e
    case SArg() => e
    case SFun(f, List(SVal(0))) => SVar(f)
    case SFun(f, args) => SFun(f,args.map(fixVars))
    case SDiv(e1, e2) => SDiv( fixVars(e1),fixVars(e2))
    case SRes(e1, e2) => SRes( fixVars(e1),fixVars(e2))
    case SMult(e1, e2)=> SMult(fixVars(e1),fixVars(e2))
    case SPow(e1, e2) => SPow( fixVars(e1),fixVars(e2))
    case SAdd(e1, e2) => SAdd( fixVars(e1),fixVars(e2))
    case SSub(e1, e2) => SSub( fixVars(e1),fixVars(e2))
  }



//  type Domains = Set[Domain] // possible domains (disjunction)
  type Domain = Map[String,VarDomain] // one domain to a set of variables
  sealed abstract class VarDomain
  case object All extends VarDomain
  case class Hole(to:Point, from:Point) extends VarDomain
  sealed abstract class Point
  case object Inf            extends Point
  case class Open(t:Double)  extends Point
  case class Close(t:Double) extends Point

  def andD(d1:VarDomain,d2:VarDomain): Option[VarDomain] = (d1,d2) match {
    case (All,_) => Some(d2)
    case (_,All) => Some(d1)
    case (h1:Hole,h2:Hole) =>
      val to = minTo(h1.to,h2.to)
      val from = maxFrom(h1.from,h2.from)
      val compat = isLess(h1.to,h2.from) && isLess(h2.to,h1.from)
      if (compat)
        (getP(to),getP(from)) match {
          case (Some(t1), Some(t2)) if t1>t2 => Some(All)
          case (Some(t1), Some(t2)) if t1==t2 =>
            if (to == Close(t1) || from == Close(t1)) Some(All)
            else Some(Hole(to,from))
          case _ => Some(Hole(to,from))
        }
      else None
  }

  def isLess(p1: Point, p2: Point): Boolean = (getP(p1),getP(p2)) match {
    case (None,_) => true
    case (_,None) => true
    case (Some(t1),Some(t2)) => (t1 < t2) || (p1==Open(t1) && p2==Open(t1))
  }

  def minTo(p1:Point, p2:Point): Point = (getP(p1),getP(p2)) match {
    case (None,_) => Inf
    case (_,None) => Inf
    case (Some(t1),Some(t2)) =>
      if (t1<t2) p1
      else if (t2<t1) p2
      else if (p1==Close(t1) && p2==Close(t2)) p1
      else Open(t1)
  }
  def maxTo(p1:Point, p2:Point): Point = (getP(p1),getP(p2)) match {
    case (None,_) => p2
    case (_,None) => p1
    case (Some(t1),Some(t2)) =>
      if (t1>t2) p1
      else if (t1<t2) p2
      else if (p1==Open(t1) && p2==Open(t2)) p1
      else Close(t1)
  }
  def maxFrom(p1:Point, p2:Point): Point = invP(minTo(invP(p1),invP(p2)))
  def minFrom(p1:Point, p2:Point): Point = invP(maxTo(invP(p1),invP(p2)))
  def invP(point: Utils.Point): Point = point match {
    case Inf => Inf
    case Open(t) => Open(-t)
    case Close(t) => Close(-t)
  }
  def getP(point: Point): Option[Double] = point match {
    case Inf      => None
    case Open(t)  => Some(t)
    case Close(t) => Some(t)
  }

  def orD(d1:VarDomain,d2:VarDomain): VarDomain = (d1,d2) match {
    case (All,_) => All
    case (_,All) => All
    case (Hole(to1,from1),Hole(to2,from2)) =>
      if (!isLess(to1,from2) || !isLess(to2,from2)) All
      else Hole(maxTo(to1,to2),minFrom(from1,from2))
  }


  def andD(d1:Domain,d2:Domain): Option[Domain] = {
    var res = d1
    for ((k,v) <- d2) {
      res += k -> (d1.get(k) match { //andD(d1.getOrElse(k,All),v)
        case Some(vd) => andD(vd, v) match {
          case Some(vDom) => vDom
          case None => return None
        }
        case _ => v
      })
    }
    Some(res)
  }

  def orD(d1:Domain,d2:Domain): Domain = {
    var res = d1
    for ((k,v) <- d2) {
      res += k -> (d1.get(k) match { //orD(d1.getOrElse(k,Hole(Inf,Inf)),v)
        case Some(vd) => orD(vd,v)
        case _ => v
      })
    }
    res
  }

  def notD(d:VarDomain): Option[VarDomain] = d match {
    case Hole(Open(t),Inf) => Some(Hole(Inf,Close(t)))
    case Hole(Close(t),Inf) => Some(Hole(Inf,Open(t)))
    case Hole(Inf,Open(t)) => Some(Hole(Close(t),Inf))
    case Hole(Inf,Close(t)) => Some(Hole(Open(t),Inf))
    case Hole(Inf,Inf) => Some(All)
    case All => Some(Hole(Inf,Inf))
    case _ => None
  }
  def notD(d:Domain): Option[Domain] = {
    val res: Domain = for ((k, v) <- d) yield notD(v) match {
      case Some(value) => k->value
      case None        => return None
    }
    Some(res)
  }


  // domains = Set() -> no open domains found
  // domains = Set(Map()) -> One open domain with no restrictions found.
  private def getDomainAux(cond:Cond): Option[Domain] = cond match {
    case BVal(true) =>  Some(Map()) //Set(Map())
    case BVal(false) =>  None
    case And(c1, c2) =>
      for (d1<-getDomain(c1); d2<-getDomain(c2); d12 <- andD(d1,d2)) yield d12
    case Or(c1, c2) =>
      for (d1<-getDomain(c1); d2<-getDomain(c2)) yield orD(d1,d2)
    case Not(c) => getDomain(c).flatMap(notD)
    case GT(VarNotLin(v), ValueNotLin(d)) => Some(Map(v->Hole(Inf,Open(d))))
    case GE(VarNotLin(v), ValueNotLin(d)) => Some(Map(v->Hole(Inf,Close(d))))
    case LT(VarNotLin(v), ValueNotLin(d)) => Some(Map(v->Hole(Open(d),Inf)))
    case LE(VarNotLin(v), ValueNotLin(d)) => Some(Map(v->Hole(Close(d),Inf)))
    case _ => None
  }

  def getDomain(cond: Cond): Option[Domain] =
    getDomainAux(cond).flatMap(filterOneSides)

  def isOneSide(vd:VarDomain): Boolean = vd match {
    case All => true
    case Hole(Inf,_) => true
    case Hole(_,Inf) => true
    case _ => false
  }

  def filterOneSides(d:Domain): Option[Domain] = {
    val allOneSide = d.view.mapValues(isOneSide).forall(_._2)
    if (allOneSide) Some(d) else None
  }
}
