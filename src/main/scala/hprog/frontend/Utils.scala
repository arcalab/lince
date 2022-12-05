package hprog.frontend

import hprog.ast.SymbolicExpr.{SyExpr, SyExprAll, SyExprVar}
import hprog.ast._
import Syntax._
import hprog.backend.Show
import hprog.frontend.CommonTypes.Valuation




// Este script serve para verificar se variáveis já foram declaradas antes de serem
// chamadas e cenas assim!



object Utils {

  /////////////////////////////////////////////////////////////////////////////////////////////////////

  // pego em conteúdo do tipo syntaxNL e retorno conteúdo do tipo List[List[DiffEq]]
  // No fundo o que  faz é retirar todas as equações diferenciais de um código
  def getDiffEqs(prog:Syntax): List[List[DiffEq]]  = prog match {
    case Atomic(_, DiffEqs(eqs,_)) => List(eqs)
    case Seq(p, q) => getDiffEqs(p) ::: getDiffEqs(q) // adiciona o resultado da direita ao inicio da lista da esquerda
    case ITE(_, thenP, elseP) => getDiffEqs(thenP) ::: getDiffEqs(elseP)
    case While(pre, _, doP) => getDiffEqs(pre) ::: getDiffEqs(doP)
  }

///////////////////////////////////////////////////////////////////////////////////////////////////////




////// NOVO ///////

// verifica se as variáveis livres já tinham sido declaradas antes de serem usadas
  def assigmentsVerify(prog:Syntax): Boolean = prog match {
    case Atomic(as,_)=>{
      var declVar= as.map(_.v.v).toList // lista  das variáveis declaradas
      var aux=0
      var aux2=1
      for (i <- as){
        var z=getVars(i.e)   // variáveis utilizadas na atribuição de outra variável

        //  removo as variaveis que existem em z que foram declaradas antes
        for (j <- 0 until (aux) by 1){
          z -= declVar(j)
        }

        // se o z ficou vazio, então é porque as variáveis tinham sido declaradas antes, senão não
        if(z.size==0){
          aux2=aux2
        } else {
          aux2=0
        }

        aux=aux+1

      }

      if (aux2==0) {
        return false
      } else {
        return true
      }

    }
  case Seq(p,_) => assigmentsVerify(p)
  case ITE(ifP,thenP,elseP) => (getVars(ifP).size==0) && assigmentsVerify(thenP) && assigmentsVerify(elseP)
  case While(pre,_,_) => assigmentsVerify(pre)

  }





///////////////////////////////////////////////////////////////////////////////////////////////////////
  /**
    * Collect the free variables, following the "free variable rules" (Lince paper)
    * @param prog where to search for the first free variables
    * @return the free variables of the first atomic expression
    */

  // O que faz é tipo chegar a p:=3+v e ficar com Set(v), mas apenas lhe interessa as do primiero atomico
  def getFstFreeVars(prog:Syntax): Set[String] = prog match {
    case Atomic(as, _) => as.toSet.flatMap((a:Assign)=>getVars(a.e))
    case Seq(p, _) => getFstFreeVars(p)
    case ITE(ifP, thenP, elseP) => getVars(ifP) ++ getFstFreeVars(thenP) ++ getFstFreeVars(elseP)
    case While(pre, _, _) => getFstFreeVars(pre)
  }

////////////////////////////////////////////////////////////////////////////////////////////////////////










////////////////////////////////////////////////////////////////////////////////////////////////////////

// verifica se há variáveis livres e também se são utilizadas variáveis que não são declaradas
  def isClosed(prog:Syntax): Either[String,Unit] = {
    val declVar = getFstDeclVars(prog) //cria um conjunto com a primeiras variaveis declaradas
    println("Variáveis declaradas:",declVar)
    val initFreeVars = getFstFreeVars(prog) // cria outro conjunto com as primeiras variáveis livres
    println("Variáveis livres:",initFreeVars)
    val usedVars = getUsedVars(prog) // cria um conjunto com todas as variáveis utilizadas
    println("Variáveis utilizadas:",usedVars)
    if (initFreeVars.nonEmpty) // verifico se existem variáveis livres, se existirem imprimo
      Left(s"Initial declaration has free variables: ${initFreeVars.mkString(", ")}")
    else if (!usedVars.forall(declVar)) // verifico se são utilizadas variáveis que não são declaradas 
      Left(s"Variable(s) not declared: ${(usedVars -- declVar).mkString(", ")}")
    else
      Right(())
  }


////////////////////////////////////////////////////////////////////////////////////////////////////////









////////////////////////////////////////////////////////////////////////////////////////////////////////

  // pega numa lista de equações diferenciais e retira as variáveis com o til em cima! (ex: p'=1+x retorna set(p))
  def getDefVars(eqs: List[DiffEq]): Set[String] =
    eqs.map(_.v.v).toSet

////////////////////////////////////////////////////////////////////////////////////////////////////////










///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  // a função getUsedVars é defenida para List[DiffEq],Syntax,DiffEqs e Dur, retornado as variáveis utilizadas lá
  def getUsedVars(eqs: List[DiffEq]): Set[String] =
    eqs.flatMap(eq => getVars(eq.e)).toSet

  def getUsedVars(prog:Syntax): Set[String] = prog match {
    case Atomic(as, de) => as.toSet.flatMap((a:Assign)=>getVars(a.e)+a.v.v) ++ getUsedVars(de)
    case Seq(p, q) => getUsedVars(p) ++ getUsedVars(q)
    case ITE(ifP, thenP, elseP) => getVars(ifP) ++ getUsedVars(thenP) ++ getUsedVars(elseP)
    case While(pre, d, doP) => getUsedVars(pre) ++ getVars(d) ++ getUsedVars(doP)
  }

  def getUsedVars(eqs: DiffEqs): Set[String] =
    getUsedVars(eqs.eqs) ++ getUsedVars(eqs.dur)

  def getUsedVars(dur: Dur): Set[String] = dur match {
    case Until(c,_,_) =>getVars(c)
    case For(nl) => getVars(nl) // NOVO!!!!!!!!!!!  
    case _ => Set()
  }

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////









////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  // Pega num programa e retira as primeiras variáveis declaradas (do primiero atómico)
  @scala.annotation.tailrec
  def getFstDeclVars(prog:Syntax): Set[String] = prog match {
    case Atomic(a, _)     => a.map(_.v.v).toSet // cria um conjunto com as primeiras variáveis declaradas
    case Seq(p, _)        => getFstDeclVars(p)
    case ITE(_, thenP, _) => getFstDeclVars(thenP) // PORQUE APENAS SE USOU O THEN E NÃO O ELSE?
    case While(pre, _, _) => getFstDeclVars(pre)
  }

/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////














/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// criam conjuntos com as variáveis aí utilizadas
  def getVars(guard: LoopGuard): Set[String] = guard match {
    case Counter(_) => Set()
    case Guard(c) => getVars(c)
  }

  // ALTEREI PARA ESTAR DE ACORDO
  def getVars(lin: Lin): Set[String] = lin match {
    case Var(v) => Set(v)
    case Value(_) => Set()
    case Add(l1, l2) => getVars(l1) ++ getVars(l2)
    case Mult(l1, l2) => getVars(l1) ++ getVars(l2)
    /*
    case Div(l1,l2) => getVars(l1) ++ getVars(l2)
    case Res(l1,l2) => getVars(l1) ++ getVars(l2)
    case Sin(l) => getVars(l)
    case Cos(l) => getVars(l) 
    case Tan(l) => getVars(l)  
    case Pow(l1,l2) => getVars(l1) ++ getVars(l2)
    case Sqrt(l1,l2) => getVars(l1) ++ getVars(l2)
    */
  }

    // ACRESCENTEI ESTA:
  def getVars(notlin: NotLin): Set[String] = notlin match {
    case VarNotLin(v) => Set(v)
    case ValueNotLin(_) => Set()
    case AddNotLin(l1, l2) => getVars(l1) ++ getVars(l2)
    case MultNotLin(l1, l2) => getVars(l1) ++ getVars(l2)
    case DivNotLin(l1,l2) => getVars(l1) ++ getVars(l2)
    case ResNotLin(l1,l2) => getVars(l1) ++ getVars(l2)
    case SinNotLin(l) => getVars(l)
    case CosNotLin(l) => getVars(l) 
    case TanNotLin(l) => getVars(l)  
    case PowNotLin(l1,l2) => getVars(l1) ++ getVars(l2)
   // case SqrtNotLin(l1,l2) => getVars(l1) ++ getVars(l2)
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
    //case IsoletedCond(l) => getVars(l) // ACRESCENTEI ISTO!
  }

/////////////////////////////////////////////////////////////////////////////////////////////////////////////







///// NOVO 

/////////////////////////////////////////////////////////////////////////////////

// criam conjuntos com as variáveis aí utilizadas
  def getVarsList(guard: LoopGuard): List[String] = guard match {
    case Counter(_) => List()
    case Guard(c) => getVarsList(c)
  }

  // ALTEREI PARA ESTAR DE ACORDO
  def getVarsList(lin: Lin): List[String] = lin match {
    case Var(v) => List(v)
    case Value(_) => List()
    case Add(l1, l2) => getVarsList(l1) ++ getVarsList(l2)
    case Mult(l1, l2) => getVarsList(l1) ++ getVarsList(l2)
    /*
    case Div(l1,l2) => getVars(l1) ++ getVars(l2)
    case Res(l1,l2) => getVars(l1) ++ getVars(l2)
    case Sin(l) => getVars(l)
    case Cos(l) => getVars(l) 
    case Tan(l) => getVars(l)  
    case Pow(l1,l2) => getVars(l1) ++ getVars(l2)
    case Sqrt(l1,l2) => getVars(l1) ++ getVars(l2)
    */
  }

    // ACRESCENTEI ESTA:
  def getVarsList(notlin: NotLin): List[String] = notlin match {
    case VarNotLin(v) => List(v)
    case ValueNotLin(_) => List()
    case AddNotLin(l1, l2) => getVarsList(l1) ++ getVarsList(l2)
    case MultNotLin(l1, l2) => getVarsList(l1) ++ getVarsList(l2)
    case DivNotLin(l1,l2) => getVarsList(l1) ++ getVarsList(l2)
    case ResNotLin(l1,l2) => getVarsList(l1) ++ getVarsList(l2)
    case SinNotLin(l) => getVarsList(l)
    case CosNotLin(l) => getVarsList(l) 
    case TanNotLin(l) => getVarsList(l)  
    case PowNotLin(l1,l2) => getVarsList(l1) ++ getVarsList(l2)
    //case SqrtNotLin(l1,l2) => getVarsList(l1) ++ getVarsList(l2)
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
    //case IsoletedCond(l) => getVars(l) // ACRESCENTEI ISTO!
  }


/////////////////////////////////////////////////////////////////////////////////

















////////////////////////////////////////////////////////////////////////////////

// Esta função é responsável por passar uma lista de assign para um Map[string->SyExpr], ou seja uma nova representação
  def toValuation(as:List[Assign],prev:Valuation): Valuation = {
    val vvv=as.map(kv => kv.v.v -> Eval.notlin2sage(kv.e))
    println("prev:",prev)
    println("vvv:",vvv)
    println("vvv.toMap:",vvv.toMap)
    println("vvv.toMap.viei.mapValues(e =>exprVarToExpr(e,prev)).toMap:",vvv.toMap.view.mapValues(e => exprVarToExpr(e,prev)).toMap)
    as.map(kv => kv.v.v -> Eval.notlin2sage(kv.e))
      .toMap
      .view.mapValues(e => exprVarToExpr(e,prev)).toMap
    
  }

///////////////////////////////////////////////////////////////////////////////







//////////////////////////////////////////////////////////////////////////////////////

// Função responsável por passar uma variável do tipo SyExprVar para o tipo SyExpr
  def exprVarToExpr(e:SyExprVar,prev:Valuation): SyExpr = e match {
    case SVal(v) => SVal(v) // pois o SVal já é uma extensão do SyExprVar
    case SVar(v) => prev(v) // v é uma string, o que se fez foi ir ao prev e utilizar essa string como key e extrair o SyExpr respetctivo
    case SFun(f, args:List[SyExprVar]) => SFun(f,args.map(exprVarToExpr(_,prev)))
    case SDiv(e1, e2) => SDiv( exprVarToExpr(e1,prev),exprVarToExpr(e2,prev))
    case SRes(e1, e2) => SRes( exprVarToExpr(e1,prev),exprVarToExpr(e2,prev))
    case SMult(e1, e2)=> SMult(exprVarToExpr(e1,prev),exprVarToExpr(e2,prev))
    case SPow(e1, e2) => SPow( exprVarToExpr(e1,prev),exprVarToExpr(e2,prev))
    case SAdd(e1, e2) => SAdd( exprVarToExpr(e1,prev),exprVarToExpr(e2,prev))
    case SSub(e1, e2) => SSub( exprVarToExpr(e1,prev),exprVarToExpr(e2,prev))
  }

/////////////////////////////////////////////////////////////////////////////////////









// Não entendi a finalidade...
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
//          case (None, _) => Some(Hole(Inf, from))
//          case (_, None) => Some(Hole(to, Inf))
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
//      (for (d1<-getDomains(c1); d2<-getDomains(c2)) yield andD(d1,d2))
//        .filter(_.isDefined)
//        .map(_.get)
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
