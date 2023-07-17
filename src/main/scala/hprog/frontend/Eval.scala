package hprog.frontend

import hprog.ast.SymbolicExpr.{SyExpr, SyExprAll, SyExprTime, SyExprVar}
import hprog.ast._
import Syntax._
import hprog.backend.Show
import hprog.frontend.CommonTypes.{Point, SySolution, SySolutionTime, SySolutionVar, Valuation, Solution,ValuationNotLin}
import hprog.frontend.solver.Solver
import hprog.frontend.Utils
import scala.math._
import scala.sys.error

object Eval {



//////////////////////////////////////////////////////////////////////////////

/*
  /** Evaluation of a linear expression. */
  def apply(state:Point, lin: Lin): Double = lin match {
    case Var(v) => state(v)
    case Value(v) => v
    case Add(l1, l2) => apply(state,l1) + apply(state,l2)
    case Mult(l1,l2)  => apply(state,l1)  * apply(state,l2)
   
  }
*/

def multOfPi(number: Double): Boolean = {
  val eps = 1e-8 // Define a small value for tolerance
  val res = abs(number % math.Pi) // Calculate the remainder
  // Check if the remainder is within the tolerance range
  return res < eps || abs(res - math.Pi) < eps
}

def multOfPiOn2(number: Double): Boolean = {
  val eps = 1e-8 // Define a small value for tolerance
  val res = abs((number+math.Pi/2) % math.Pi) // Calculate the remainder
  // Check if the remainder is within the tolerance range
  return res < eps || abs(res - math.Pi) < eps
}

  /** Evaluation of a non-linear expression. */
  def apply(state:Point, notlin: NotLin): Double = {
    val res = notlin match {
            case Var(v) => state(v)
            case Value(v) => v
            case Add(l1, l2) => apply(state,l1) + apply(state,l2)
            case Mult(l1,l2)  => apply(state,l1)  * apply(state,l2)
            case Div(l1,l2)  => {if (apply(state,l2)==0) {return throw new RuntimeException(s"Error: the divisor of the division '${Show.applyV(notlin)}' is zero.")}
                                 else {return (apply(state,l1) / apply(state,l2))}
                                }
            case Res(l1,l2)  => {if (apply(state,l2)==0) {return throw new RuntimeException(s"Error: the divisor of the remainder '${Show.applyV(notlin)}' is zero.")}
                                 else {return (apply(state,l1) % apply(state,l2))}
                                }
<<<<<<< HEAD
=======
            //case Pow(l1,l2)  => pow(apply(state,l1),apply(state,l2))
>>>>>>> 44bafb4c6dddf0bd489867f824528c771bea5703
            case Func(s,list) => (s,list) match {
              case ("PI",Nil) => math.Pi
              case ("E",Nil) => math.E
              case ("max",v1::v2::Nil) => math.max(apply(state,v1), apply(state,v2))
              case ("min",v1::v2::Nil) => math.min(apply(state,v1), apply(state,v2))
<<<<<<< HEAD
              case ("pow",v1::v2::Nil) => {if(apply(state,v1)==0 && apply(state,v2)<0) return throw new RuntimeException(s"Error: The power of zero is undefined for a negative exponent: '${Show.applyV(notlin)}'.")
                                           else pow(apply(state,v1),apply(state,v2))
              }
              case ("exp",v::Nil) => math.exp(apply(state,v))
              case ("sin",v::Nil) => {if (multOfPi(apply(state,v))) {return 0}

                                      else {return math.sin(apply(state,v))}

              }
              case ("cos",v::Nil) => {if (multOfPiOn2(apply(state,v))) {return 0}

                                      else {return math.cos(apply(state,v))}

              }
              case ("tan",v::Nil) =>{if (multOfPi(apply(state,v))) {return 0}

                                      else {return math.tan(apply(state,v))}

              }
=======
              case ("pow",v1::v2::Nil) => pow(apply(state,v1),apply(state,v2))
              case ("exp",v::Nil) => math.exp(apply(state,v))
              case ("sin",v::Nil) => math.sin(apply(state,v))
              case ("cos",v::Nil) => math.cos(apply(state,v))
              case ("tan",v::Nil) => math.tan(apply(state,v))
>>>>>>> 44bafb4c6dddf0bd489867f824528c771bea5703
              case ("arcsin",v::Nil) => {
                if ((math.asin(apply(state,v))).isNaN) return throw new RuntimeException(s"Error: In the expression '${Show.applyV(notlin)}', '${Show.applyV(v)}' is outside the domain of arcsin (-1<=x<=1).")
                else math.asin(apply(state,v))
              }
              case ("arccos",v::Nil) => {
                if ((math.acos(apply(state,v))).isNaN) return throw new RuntimeException(s"Error: In the expression '${Show.applyV(notlin)}', '${Show.applyV(v)}' is outside the domain of arccos (-1<=x<=1).")
                else math.acos(apply(state,v))
              }
              case ("arctan",v::Nil) => math.atan(apply(state,v))
              case ("sinh",v::Nil) => math.sinh(apply(state,v))
              case ("cosh",v::Nil) => math.cosh(apply(state,v))
              case ("tanh",v::Nil) => math.tanh(apply(state,v))
              case ("sqrt",v::Nil) => {
                if ((math.sqrt(apply(state,v))).isNaN) return throw new RuntimeException(s"Error: In the expression '${Show.applyV(notlin)}', '${Show.applyV(v)}' is outside the domain of sqrt (x>=0).")
                else math.sqrt(apply(state,v))
              }
              case ("log",v::Nil) =>  {
                if (apply(state,v)<=0) return throw new RuntimeException(s"Error: In the expression '${Show.applyV(notlin)}', '${Show.applyV(v)}' is outside the domain of log (x>0).")
                else math.log(apply(state,v))
              }
              case ("log10",v::Nil) =>  {
                if (apply(state,v)<=0) return throw new RuntimeException(s"Error: In the expression '${Show.applyV(notlin)}', '${Show.applyV(v)}' is outside the domain of log10 (x>0).")
                else math.log10(apply(state,v))
              }
               case (_,_) => throw new RuntimeException(s"Unknown function '${s}(${(list.map(Show.applyV).toList).mkString(",")})', or the number of arguments are incorrect")

            }
    }
  //println(s"Eval: notlin->${notlin} to ${res}")
  res
}

  def updateNotlin(state:ValuationNotLin, notlin: NotLin,vars:List[String]): NotLin = {
    val res = notlin match {
            case Var(v) => {if (vars.contains(v)) {Var(v)} else {state(v)}}
            case Value(v) => Value(v)
            case Add(l1, l2) => Add(updateNotlin(state,l1,vars), updateNotlin(state,l2,vars))
            case Mult(l1,l2)  => Mult(updateNotlin(state,l1,vars), updateNotlin(state,l2,vars))
            case Div(l1,l2)  => Div(updateNotlin(state,l1,vars), updateNotlin(state,l2,vars))
            case Res(l1,l2)  => Res(updateNotlin(state,l1,vars), updateNotlin(state,l2,vars))
           // case Pow(l1,l2)  => Pow(updateNotlin(state,l1,vars), updateNotlin(state,l2,vars))
            case Func(s,list) => Func(s,list.map(l=>updateNotlin(state,l,vars)).toList)

            }
    res
}

 // The purpose of this function is to replace the constant variables of a diff.eq. by their respective constant values 
  def updateDiffEq(diffeq:DiffEq,v:ValuationNotLin,vars:List[String]):DiffEq = {
     
     var newNotLin= updateNotlin(v,diffeq.e,vars)
     var newdiffeq= DiffEq(diffeq.v,newNotLin)
     return newdiffeq
  }


  // New
  def apply(state:Point, cond: Cond): Boolean =
    cond match {
      case BVal(b) =>  b
      case And(c1, c2) => apply(state,c1) && apply(state,c2)
      case Or(c1, c2) => apply(state,c1) || apply(state,c2)
      case Not(c) => !apply(state,c)
      case EQ(v, l) => apply(state,v) == apply(state,l)
      case GT(v, l) => apply(state,v) >  apply(state,l)
      case LT(v, l) => apply(state,v) <  apply(state,l)
      case GE(v, l) => apply(state,v) >= apply(state,l)
      case LE(v, l) => apply(state,v) <= apply(state,l)
      
    }






 def apply(e:SyExprAll, t: Double, x: Valuation): Double = {
    val res = e match {
    case SVal(v) => v
    case _:SArg => t
    //case SVar("e")  if !x.contains("e")  => math.E
    //case SVar("pi") if !x.contains("pi") => math.Pi
    case s:SVar if !x.contains(s.v) =>
      throw new RuntimeException(s"Evaluating $e but ${s.v} not found in [${Show(x)}].")
    case s:SVar => apply(x(s.v),t,x)
    case SDiv(e1, e2) =>(apply(e1,t,x) / apply(e2,t,x))                                     
    case SRes(e1, e2) => apply(e1,t,x) % apply(e2,t,x)
    case SMult(e1, e2) =>apply(e1,t,x) * apply(e2,t,x)
    case SPow(e1, e2) => math.pow(apply(e1,t,x),apply(e2,t,x))
    case SAdd(e1, e2) => apply(e1,t,x) + apply(e2,t,x)
    case SSub(e1, e2) => apply(e1,t,x) - apply(e2,t,x)
    case s:SFun[SymbolicExpr.All] => (s.f,s.args) match {
      case (v,List(SVal(0.0))) if x contains v => apply(x(v),t,x) // could create infinite loop
      case ("PI",Nil) => math.Pi
      case ("E",Nil) => math.E
      case ("max",v1::v2::Nil) => math.max(apply(v1,t,x), apply(v2,t,x))
      case ("min",v1::v2::Nil) => math.min(apply(v1,t,x), apply(v2,t,x))
      case ("pow",v1::v2::Nil) => math.pow(apply(v1,t,x), apply(v2,t,x))
      case ("exp",v::Nil) => math.exp(apply(v,t,x))
      case ("sin",v::Nil) => {if (multOfPi(apply(v,t,x))) {return 0}

                                      else {return math.sin(apply(v,t,x))}

              }
      case ("cos",v::Nil) => {if (multOfPiOn2(apply(v,t,x))) {return 0}

                                      else {return math.cos(apply(v,t,x))}

              }
      case ("tan",v::Nil) => {if (multOfPi(apply(v,t,x))) {return 0}

                                      else {return math.tan(apply(v,t,x))}

              }
      case ("arcsin",v::Nil) => math.asin(apply(v,t,x))
      case ("arccos",v::Nil) => math.acos(apply(v,t,x))
      case ("arctan",v::Nil) => math.atan(apply(v,t,x))
      case ("sinh",v::Nil) => math.sinh(apply(v,t,x))
      case ("cosh",v::Nil) => math.cosh(apply(v,t,x))
      case ("tanh",v::Nil) => math.tanh(apply(v,t,x))
      case ("sqrt",v::Nil) => math.sqrt(apply(v,t,x))
      case ("log",v::Nil) => math.log(apply(v,t,x))
      case ("log10",v::Nil) => math.log10(apply(v,t,x))
<<<<<<< HEAD
      //case (_,_) => throw new RuntimeException(s"")
      case (_,_) => throw new RuntimeException(s"Unknown function '${s.f}(${s.args.mkString(",")})',or the number of arguments are incorrect")
=======
      case (_,_) => throw new RuntimeException(
        s"Unknown function '${s.f}(${s.args.mkString(",")})',or the number of arguments are incorrect")
>>>>>>> 44bafb4c6dddf0bd489867f824528c771bea5703
    }
  }
 /*
  {
                         { if (apply(e2,t,x)==0) {return throw new RuntimeException(s"Error: the denominator of the division '${Show.apply(e)}' is zero.")}
                          else {return (apply(e1,t,x) / apply(e2,t,x))}
                         }
                         */
  //println(s"Eval(SY): SyExprAll->${e} to ${res}")
  res
}





///////////////////////////////////////////////////////////////////////////////////////////
/**
  def apply(e:SyExprAll, t: Double, x: Valuation): Double = {
    val res = e match {
    case SVal(v) => v
    case _:SArg => t
    case SVar("e")  if !x.contains("e")  => math.E
    case SVar("pi") if !x.contains("pi") => math.Pi
    case s:SVar if !x.contains(s.v) =>
      throw new RuntimeException(s"Evaluating $e but ${s.v} not found in [${Show(x)}].")
    case s:SVar => apply(x(s.v),t,x)
    case SDiv(e1, e2) =>(apply(e1,t,x) / apply(e2,t,x))                                     
    case SRes(e1, e2) => apply(e1,t,x) % apply(e2,t,x)
    case SMult(e1, e2) =>apply(e1,t,x) * apply(e2,t,x)
    case SPow(e1, e2) => math.pow(apply(e1,t,x),apply(e2,t,x))
    case SAdd(e1, e2) => apply(e1,t,x) + apply(e2,t,x)
    case SSub(e1, e2) => apply(e1,t,x) - apply(e2,t,x)
    case s:SFun[SymbolicExpr.All] => (s.f,s.args) match {
      case (v,List(SVal(0.0))) if x contains v => apply(x(v),t,x) // could create infinite loop
      case ("PI",Nil) => math.Pi
      case ("E",Nil) => math.E
      case ("max",v1::v2::Nil) => math.max(apply(v1,t,x), apply(v2,t,x))
      case ("min",v1::v2::Nil) => math.min(apply(v1,t,x), apply(v2,t,x))
      case ("pow",v1::v2::Nil) => math.pow(apply(v1,t,x), apply(v2,t,x))
      case ("exp",v::Nil) => math.exp(apply(v,t,x))
      case ("sin",v::Nil) => {if (multOfPi(apply(v,t,x))) {return 0}

                                      else {return math.sin(apply(v,t,x))}

              }
      case ("cos",v::Nil) => {if (multOfPiOn2(apply(v,t,x))) {return 0}

                                      else {return math.cos(apply(v,t,x))}

              }
      case ("tan",v::Nil) => {if (multOfPi(apply(v,t,x))) {return 0}

                                      else {return math.tan(apply(v,t,x))}

              }
      case ("arcsin",v::Nil) => math.asin(apply(v,t,x))
      case ("arccos",v::Nil) => math.acos(apply(v,t,x))
      case ("arctan",v::Nil) => math.atan(apply(v,t,x))
      case ("sinh",v::Nil) => math.sinh(apply(v,t,x))
      case ("cosh",v::Nil) => math.cosh(apply(v,t,x))
      case ("tanh",v::Nil) => math.tanh(apply(v,t,x))
      case ("sqrt",v::Nil) => math.sqrt(apply(v,t,x))
      case ("log",v::Nil) => math.log(apply(v,t,x))
      case ("log10",v::Nil) => math.log10(apply(v,t,x))
      //case (_,_) => throw new RuntimeException(s"")
      case (_,_) => throw new RuntimeException(s"Unknown function '${s.f}(${s.args.mkString(",")})',or the number of arguments are incorrect")
    }
  }
 /*
  {
                         { if (apply(e2,t,x)==0) {return throw new RuntimeException(s"Error: the denominator of the division '${Show.apply(e)}' is zero.")}
                          else {return (apply(e1,t,x) / apply(e2,t,x))}
                         }
                         */
  //println(s"Eval(SY): SyExprAll->${e} to ${res}")
  res
}
*/


  // Ignore variables or time arguments 
  def apply(e:SyExprTime, t:Double): Double = apply(e,t,Map())
  def apply(e:SyExprVar, x:Valuation): Double = apply(e,0,x)
  def apply(e:SyExpr): Double = apply(e,0,Map())

  def apply(v: Valuation): Point = v.view.mapValues(apply).toMap 

  def update(e:SyExprAll, t:SyExpr, v:Valuation): SyExpr =
    updInput(Eval.updTime(t,e),v)

  def update(phi:SySolution, t:SyExpr, v:Valuation): Valuation =
    updInput(v,Eval.updTime(t,phi))


  // variation for numerically computed solutions
  def updateNum(phi: Solution, t: SyExpr, v: Valuation): Valuation =
    phi.view.mapValues(updater => SVal(updater(apply(t))(apply(v)))).toMap
   


    
  /** Update an expression by replacing initial values v(0)
    * @param e expression to be updated
    * @param sol solution with the new values
    * @return updated expression
    */
  def updInputFun(e:SyExprAll, sol:Valuation): SyExprTime = e match {
    case s:SVar => sol(s.v)
    case s:SFun[SymbolicExpr.All] =>
      SFun[SymbolicExpr.Time](s.f,s.args.map(e2 =>  updInputFun(e2,sol)))
    case SDiv(e1, e2)  => SDiv( updInputFun(e1,sol),updInputFun(e2,sol))
    case SRes(e1, e2)  => SRes( updInputFun(e1,sol),updInputFun(e2,sol))
    case SMult(e1, e2) => SMult(updInputFun(e1,sol),updInputFun(e2,sol))
    case SPow(e1, e2)  => SPow( updInputFun(e1,sol),updInputFun(e2,sol))
    case SAdd(e1, e2)  => SAdd( updInputFun(e1,sol),updInputFun(e2,sol))
    case SSub(e1, e2)  => SSub( updInputFun(e1,sol),updInputFun(e2,sol))
    case SVal(v) => SVal(v)
    case t:SArg  => t
  }

  def updInput(e:SyExprVar, sol:Valuation): SyExpr = updInputFun(e,sol) match {
    case t:SyExpr @unchecked => t // guaranteed to succeed (but type eliminated by erasure)
    case v => throw new RuntimeException(s"updating variable in ${Show(e)} does not yield an SExpr (${Show(v)}).")
  }

  def updTime(t:SyExpr, phi:SySolution): SySolutionVar =
    phi.view.mapValues(updTime(t,_)).toMap
  def updInput(input:Valuation,phi:SySolutionVar): Valuation =
    phi.view.mapValues(updInput(_,input)).toMap
  def updInputFun(input:Valuation,phi:SySolution): SySolutionTime =
    phi.view.mapValues(updInputFun(_,input)).toMap
  def solveValues(s:Solver,phi:Valuation): Valuation =
    phi.view.mapValues(s.solveSymbExpr).toMap

  def updTime(newt: SyExprVar, expr: SyExprAll): SyExprVar = updTimeFun(newt, expr) match {
    case e: SyExprVar @unchecked => e  // guaranteed to succeed (but type eliminated by erasure)
    case v => throw new RuntimeException(s"updating time in ${Show(expr)} does not yield an SExprV (${Show(v)}).")
  }

  def updTimeFun(newt: SyExprAll, expr:SyExprAll): SyExprAll = expr match {
    case _:SArg => newt
    case sf:SFun[SymbolicExpr.All]  =>
      SFun(sf.f,sf.args.map((e2:SyExprAll) => updTimeFun(newt,e2)))
    case SDiv(e1, e2)  => SDiv( updTimeFun(newt,e1), updTimeFun(newt,e2))
    case SRes(e1, e2)  => SRes( updTimeFun(newt,e1), updTimeFun(newt,e2))
    case SMult(e1, e2) => SMult(updTimeFun(newt,e1), updTimeFun(newt,e2))
    case SPow(e1, e2)  => SPow( updTimeFun(newt,e1), updTimeFun(newt,e2))
    case SAdd(e1, e2)  => SAdd( updTimeFun(newt,e1), updTimeFun(newt,e2))
    case SSub(e1, e2)  => SSub( updTimeFun(newt,e1), updTimeFun(newt,e2))
    case e:SVal => e
    case e:SVar => e
  }

  
/*
  def lin2sage(l:Lin): SyExprVar = l match {
    case Var(v) => SVar(v) //SFun(v,List(SVal(0))) //SVar(v)
    case Value(v) => SVal(v)
    case Add(l1, l2) => SAdd(lin2sage(l1),lin2sage(l2))
    case Mult(l1, l2) => SMult(lin2sage(l1),lin2sage(l2)) //new


  }
  */

<<<<<<< HEAD
def syExpr2notlin(l:SyExpr):NotLin= l match {
  case SVal(v) => Value(v) 
  //case SVar(v) => Var(v) 
  case SFun(s,list)=> Func(s,list.map((l:SyExpr) => syExpr2notlin(l)))
  case SDiv(e1, e2) => Div(syExpr2notlin(e1),syExpr2notlin(e2))
  case SRes(e1, e2) => Res(syExpr2notlin(e1),syExpr2notlin(e2))
  case SMult(e1, e2)=> Mult(syExpr2notlin(e1),syExpr2notlin(e2))
  case SPow(e1, e2) => Func("pow",List(syExpr2notlin(e1),syExpr2notlin(e2)))
  case SAdd(e1, e2) => Add(syExpr2notlin(e1),syExpr2notlin(e2))
  case SSub(e1, e2) => Add(syExpr2notlin(e1),Mult(Value(-1),syExpr2notlin(e2)))
}
/**
=======

>>>>>>> 44bafb4c6dddf0bd489867f824528c771bea5703
// Convert SyExpr to NotLin
def syExpr2notlin(l:SyExpr):NotLin= l match {
  case SVal(v) => Value(v) 
  //case SVar(v) => Var(v) 
  case SFun(s,list)=> Func(s,list.map((l:SyExpr) => syExpr2notlin(l)))
  case SDiv(e1, e2) => Div(syExpr2notlin(e1),syExpr2notlin(e2))
  case SRes(e1, e2) => Res(syExpr2notlin(e1),syExpr2notlin(e2))
  case SMult(e1, e2)=> Mult(syExpr2notlin(e1),syExpr2notlin(e2))
  case SPow(e1, e2) => Func("pow",List(syExpr2notlin(e1),syExpr2notlin(e2)))
  case SAdd(e1, e2) => Add(syExpr2notlin(e1),syExpr2notlin(e2))
  case SSub(e1, e2) => Add(syExpr2notlin(e1),Mult(Value(-1),syExpr2notlin(e2)))
}
<<<<<<< HEAD
*/
=======

>>>>>>> 44bafb4c6dddf0bd489867f824528c771bea5703

 // New
  def notlin2sage(l:NotLin): SyExprVar = l match {
    case Var(v) => SVar(v) //SFun(v,List(SVal(0))) //SVar(v)
    case Value(v) => SVal(v)
    case Add(l1, l2) => SAdd(notlin2sage(l1),notlin2sage(l2))
    case Mult(l1, l2) => SMult(notlin2sage(l1),notlin2sage(l2))
    case Div(l1, l2) => SDiv(notlin2sage(l1),notlin2sage(l2))
    //case Pow(l1,l2) => SPow(notlin2sage(l1),notlin2sage(l2))
    case Func(s,list)=>SFun(s,list.map((l:NotLin) => notlin2sage(l)))
    case Res(l1,l2) => SRes(notlin2sage(l1),notlin2sage(l2)) 

  }


//NEWWWW
  def sFunToSVar(e: SyExprVar): SyExprVar = e match {
    case _:SVal => e
    case _:SArg => e
    case SVar(_) => e
    case SFun("_e", List(SVal(0))) => SVar("e")
    case SFun("_pi", List(SVal(0))) => SVar("pi")
    case SFun(f, List(SVal(0))) => SVar(f)
    case SFun(f,a) =>
      error(s"Trying to calculate the value of an expression with a function $f(${a.map(Show(_)).mkString(",")}")
    case SDiv(e1, e2) => SDiv(sFunToSVar(e1),sFunToSVar(e2))
    case SRes(e1, e2) => SRes(sFunToSVar(e1),sFunToSVar(e2))
    case SMult(e1, e2) => SMult(sFunToSVar(e1),sFunToSVar(e2))
    case SPow(e1, e2) => SPow(sFunToSVar(e1),sFunToSVar(e2))
    case SAdd(e1, e2) => SAdd(sFunToSVar(e1),sFunToSVar(e2))
    case SSub(e1, e2) => SSub(sFunToSVar(e1),sFunToSVar(e2))
  }

  def baseSage(vs:Iterable[String]): SySolution =
    vs.map(v => v -> SFun(v,List(SVal(0.0)))).toMap


  def simplifyMan(e:SyExprAll): SyExprAll = e match {
    case _:SVal => e
    case _:SArg => e
    case _:SVar => e
    case SFun(f, args) => SFun(f,args.map(simplifyMan))
    case SDiv(e1, e2) => (simplifyMan(e1),simplifyMan(e2)) match {
      case (x,SVal(1)) => x
      case (SVal(0),_) => SVal(0)
      case (SVal(x),SVal(y)) => SVal(x/y)
      case (x,y) => SDiv(x,y)
    }
    case SRes(e1, e2) => (simplifyMan(e1),simplifyMan(e2)) match {
      case (x,SVal(1)) => SVal(0)
      case (SVal(1),_) => SVal(1)
      case (SVal(0),_) => SVal(0)
      case (SVal(x),SVal(y)) => SVal(x%y)
      case (x,y) => SRes(x,y)
    }
    case SMult(e1, e2) => (simplifyMan(e1),simplifyMan(e2)) match {
      case (x,SVal(1)) => x
      case (SVal(1),x) => x
      case (SVal(0),_) => SVal(0)
      case (_,SVal(0)) => SVal(0)
      case (SVal(x),SVal(y)) => SVal(x*y)
      case (x,y) => SMult(x,y)
    }
    case SPow(e1, e2) => (simplifyMan(e1),simplifyMan(e2)) match {
      case (x,SVal(1)) => x
      case (_,SVal(0)) => SVal(1)
      //case (SVal(0),_) => SVal(0)
      case (SVal(1),_) => SVal(1)
      case (SVal(x),SVal(y)) => SVal(math.pow(x,y))
      case (x,y) => SPow(x,y)
    }
    case SAdd(e1, e2) => (simplifyMan(e1),simplifyMan(e2)) match {
      case (x,SVal(0)) => x
      case (SVal(0),x) => x
      case (SVal(x),SVal(y)) => SVal(x+y)
      case (SAdd(e11,e12),e21) => simplifyMan(SAdd(e11,SAdd(e12,e21)))
      case (x,y) => SAdd(x,y)
    }
    case SSub(e1, e2) => (simplifyMan(e1),simplifyMan(e2)) match {
      case (x,SVal(0)) => x
      case (SVal(x),SVal(y)) => SVal(x-y)
      case (SSub(e11,e12),e21) => simplifyMan(SSub(e11,SAdd(e12,e21)))
      case (x,y) => SSub(x,y)
    }
  }

}
