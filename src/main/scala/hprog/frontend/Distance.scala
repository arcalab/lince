package hprog.frontend

//import breeze.numerics.{pow, sqrt}
import hprog.ast
import hprog.ast._
import Syntax._
import hprog.frontend.CommonTypes.Point
import optimus.algebra.{Constraint, Expression}
import optimus.optimization._
import optimus.optimization.enums.SolverLib
import optimus.optimization.model.{MPFloatVar, MPVar}

import scala.math._

class Distance(eps:Double) extends Deviator {
  override def closest(p: Point, cond: Cond): Option[Point] =
    Distance.closest(p,cond,eps)
}

object Distance {

  /**
    * Find the closest point that satisfies a condition, up to a distance of `max`
    * @param p reference point
    * @param c condition with the domain where to search
    * @param max maximum distance to search
    * @return some closest point within c and not further than `max`
    */



  def closest(p:Point,c:Cond,max:Double): Option[Point] = {
    val res = if (max<=0) {
      if (normaliseCond(c) contains p) Some(p) else None
    }
    else closest(p, normaliseCond(c), max).flatMap(p2 =>
        if (dist(p,p2)<=max) Some(p2) else None
    )
   
    res
  }



  type DNF = Or
  case class Or(ands:Set[And]) {
    def contains(p:Point): Boolean = ands.exists(_ contains p)
  }
  case class And(ineqs:Set[Ineq]) {
    def contains(p:Point): Boolean = ineqs.forall(_ contains p)
  }
  sealed abstract class Ineq {
    def contains(p:Point): Boolean = this match {
      case GT(l1, l2) => Eval(p,l1) >  Eval(p,l2)
      case LT(l1, l2) => Eval(p,l1) <  Eval(p,l2)
      case GE(l1, l2) => Eval(p,l1) >= Eval(p,l2)
      case LE(l1, l2) => Eval(p,l1) <= Eval(p,l2)
    }
  }



  // New
  case class GT(l1:NotLin,l2:NotLin)      extends Ineq
  case class LT(l1:NotLin,l2:NotLin)      extends Ineq
  case class GE(l1:NotLin,l2:NotLin)      extends Ineq
  case class LE(l1:NotLin,l2:NotLin)      extends Ineq

//////////////////////////////////////////////////////////////////

// New
  def normaliseCond(c:Cond): DNF = c match {
    case BVal(true) => Or(Set(And(Set())))
    case BVal(false) => Or(Set()) 
    case ast.Syntax.And(c1, c2) => (normaliseCond(c1),normaliseCond(c2)) match {
      case (Or(ands1),Or(ands2)) => Or( for(a1<-ands1;a2<-ands2) yield And(a1.ineqs++a2.ineqs))
    }
    case ast.Syntax.Or(c1, c2) =>  (normaliseCond(c1),normaliseCond(c2)) match {
      case (Or(ands1), Or(ands2)) => Or(ands1++ands2)
    }
    case Not(c2) => normaliseCond(swapOps(c2))
    case ast.Syntax.EQ(l1, l2) => Or(Set(And(Set(GE(l1, l2),LE(l1, l2)))))
    case ast.Syntax.GT(l1, l2) => Or(Set(And(Set(GT(l1, l2)))))
    case ast.Syntax.LT(l1, l2) => Or(Set(And(Set(LT(l1, l2)))))
    case ast.Syntax.GE(l1, l2) => Or(Set(And(Set(GE(l1, l2)))))
    case ast.Syntax.LE(l1, l2) => Or(Set(And(Set(LE(l1, l2)))))
  }
  

  // Condition swap
  def swapOps(c: Cond): Cond = c match {
    case BVal(b)        => BVal(!b)
    case ast.Syntax.And(c1, c2)=> ast.Syntax.Or(swapOps(c1),swapOps(c2))
    case ast.Syntax.Or(c1, c2) => ast.Syntax.And(swapOps(c1),swapOps(c2))
    case Not(c2)        => c2
    case ast.Syntax.EQ(l1, l2)   => ast.Syntax.Or(ast.Syntax.LT(l1, l2),ast.Syntax.GT(l1, l2))
    case ast.Syntax.GT(l1, l2)   => ast.Syntax.LE(l1, l2)
    case ast.Syntax.LT(l1, l2)   => ast.Syntax.GE(l1, l2)
    case ast.Syntax.GE(l1, l2)   => ast.Syntax.LT(l1, l2)
    case ast.Syntax.LE(l1, l2)   => ast.Syntax.GT(l1, l2)
  }

///////////////////////////////////////////////////////////////////////////


  private def closest(p:Point, dnf:DNF, max:Double): Option[Point] = dnf.ands.headOption match {
    case Some(and) =>
      closest(p, and, max) match {
        case Some(p1) =>
          closest(p, Or(dnf.ands.tail),max) match {
            case Some(p2) =>
              if (dist(p,p1) <= dist(p,p2)) Some(p1) else Some(p2)
            case None =>
              Some(p1)
          }
        case None =>
          closest(p, Or(dnf.ands.tail),max)
      }
    case None => None
  }

  def dist(p1:Point,p2:Point): Double = {
    val sqrs = for ((x,v) <- p1) yield math.pow(p2.getOrElse(x,v)-v,2)
    math.sqrt(sqrs.sum)
  }


  def closest(p:Point,and:And,max:Double): Option[Point] =
    closest(p,and.ineqs.map(closeIneq),max)



  def closest(p:Point,ineqs:Set[Ineq],max:Double): Option[Point] = {
    for (ineq <- ineqs) {
      val p2: Point = closest(p,ineq)
      if (And(ineqs - ineq) contains p2) return Some(p2)
    }
    quadraticProgrm(p,ineqs,max)
  }

  // replace < by <= and > by >=
  def closeIneq(i:Ineq): Ineq = i match {
    case GT(l1, l2) => GE(l1,l2)
    case LT(l1, l2) => LE(l1,l2)
    case _ => i
  }

  def closest(p:Point,ineq:Ineq): Point = {
    if (ineq contains p) p
    else closest(p,left(ineq),right(ineq))
  }

  def closest(p:Point, l1:NotLin, l2:NotLin): Point = {
    val delta = neg(p)
    val p1 = notlin2point(shiftNotLin(l1,delta))
    val p2 = notlin2point(shiftNotLin(l2,delta))
    val plane = add(p1,neg(p2))
    val d = 0.0 - plane.getOrElse("",0.0) // extra value without variable from p1-p2
    val plane2 = plane - "" // take out empty variable
    val p3 = closestToOrigin(plane2,d) // got closest to origin
    //println(s"closest to origin solving ${plane2} = $d\n returned $p3")
    add(p3 , p) // add point p in the end
  }

//New
  def notlin2point(notlin: NotLin): Point = notlin match {
    case Var(v) => Map(v->1.0)
    case Value(v) => Map("" -> v)
    case Add(l1, l2) => add(notlin2point(l1),notlin2point(l2))
    case Mult(l1, l2) => mul(notlin2point(l1),notlin2point(l2))
    case Div(l1, l2) => div(notlin2point(l1),notlin2point(l2))
    case Res(l1, l2) => res(notlin2point(l1),notlin2point(l2))
    //case Pow(l1, l2) => powdef(notlin2point(l1),notlin2point(l2))
    case Func(s,list) => funcdef(s,list.map((l:NotLin) => notlin2point(l)))
   

    }

  // Why 'x' exist before '->'??  
  def add(p1:Point,p2:Point): Point =
    p1 ++ (for ((x,v) <- p2) yield x -> (p1.getOrElse(x,0.0)+v)) // getOrElse remove the value of p1, if it not exist, return (x,0)


//////////////////////////////////////////////////////////////////

//New
  def mul(p1:Point,p2:Point): Point=
    p1 ++ (for ((x,v) <- p2) yield x -> (p1.getOrElse(x,1.0)*v))

  def div(p1:Point,p2:Point): Point=
    p1 ++ (for ((x,v) <- p2) yield x -> (if (p1.contains(x))  p1(x)/v else v)) 


  def res(p1:Point,p2:Point): Point=
    p1 ++ (for ((x,v) <- p2) yield x -> (if (p1.contains(x))  p1(x)%v else v))

 
  def seno(p1:Map[String,Double]): Map[String,Double]=
    p1.map(v => v._1 -> sin(v._2))
  


  def cosseno(p1:Map[String,Double]): Map[String,Double]=
    p1.map(v => v._1 -> cos(v._2))


  def tangente(p1:Map[String,Double]): Map[String,Double]=
    p1.map(v => v._1 -> tan(v._2))

/**
  def powdef(p1:Map[String,Double],p2:Map[String,Double]): Map[String,Double]=
   p1 ++ (for ((x,v) <- p2) yield x -> (if (p1.contains(x))  pow(p1(x),v) else v))
*/
  
  def funcdef(s:String,list:List[Point]):Point ={
    if(list.length == 0 || list.length>2){
      s match {
        case ("PI") => Map("" -> math.Pi)
        case ("E") => Map("" -> math.E)
        case (_) => throw new RuntimeException(s"Unknown function '${s}',or the number of arguments are incorrect")
      }
      
    }
    else {
      if (list.length == 1){
        s match {
          case ("exp") => list(0).map(v => v._1 -> math.exp(v._2))
          case ("sin") => list(0).map(v => v._1 -> math.sin(v._2))
          case ("cos") => list(0).map(v => v._1 -> math.cos(v._2))
          case ("tan") => list(0).map(v => v._1 -> math.tan(v._2))
          case ("arcsin") => list(0).map(v => v._1 -> math.asin(v._2))
          case ("arccos") => list(0).map(v => v._1 -> math.acos(v._2))
          case ("arctan") => list(0).map(v => v._1 -> math.atan(v._2))
          case ("sinh") => list(0).map(v => v._1 -> math.sinh(v._2))
          case ("cosh") => list(0).map(v => v._1 -> math.cosh(v._2))
          case ("tanh") => list(0).map(v => v._1 -> math.tanh(v._2))
          case ("sqrt") => list(0).map(v => v._1 -> math.sqrt(v._2))
          case ("log") => list(0).map(v => v._1 -> math.log(v._2))
          case ("log10") => list(0).map(v => v._1 -> math.log10(v._2))
          case (_)=>throw new RuntimeException(s"Unknown function '${s}',or the number of arguments are incorrect")
        }
      }
      else {
        s match {
          case ("max") => list(0) ++ (for ((x,v) <- list(1)) yield x -> (if (list(0).contains(x))  math.max((list(0))(x),v) else v))
          case ("min") => list(0) ++ (for ((x,v) <- list(1)) yield x -> (if (list(0).contains(x))  math.min((list(0))(x),v) else v))
          case ("pow") => list(0) ++ (for ((x,v) <- list(1)) yield x -> (if (list(0).contains(x))  pow(list(0)(x),v) else v))
          case (_)=>throw new RuntimeException(s"Unknown function '${s}',or the number of arguments are incorrect")
        }  
      }

    }
  }

  
/////////////////////////////////////////////////////////////////


  def neg(p1:Point): Point =
    p1.view.mapValues(_*(-1)).toMap

  // v*d / |v|^2
  def closestToOrigin(plane:Point,d:Double): Point = {
    val norm = plane.values.map(math.pow(_,2)).sum
    plane.view.mapValues(x => x*d/norm).toMap
  }

  def left(ineq: Ineq): NotLin = ineq match {
    case GT(l1,_) => l1
    case LT(l1,_) => l1
    case GE(l1,_) => l1
    case LE(l1,_) => l1
  }
  def right(ineq: Ineq): NotLin = ineq match {
    case GT(_,l2) => l2
    case LT(_,l2) => l2
    case GE(_,l2) => l2
    case LE(_,l2) => l2
  }


  // need to shift based on `max` because variables will only get positive values
  def quadraticProgrm(p: Point, ineqs: Set[Ineq], max:Double): Option[Point] = {
    val delta = p.view.mapValues(x => if (x<max) max-x else 0).toMap
    val shiftedP = add(p,delta)
    val shiftedIneqs = ineqs.map(i => shiftIneq(i,delta))
    quadraticProgrm(shiftedP,shiftedIneqs)
      .map(p2 => add(p2,neg(delta)))
  }

  def shiftIneq(ineq: Ineq, delta:Point): Ineq = ineq match {
    case GT(l1, l2) => GT(shiftNotLin(l1,delta),shiftNotLin(l2,delta))
    case LT(l1, l2) => LT(shiftNotLin(l1,delta),shiftNotLin(l2,delta))
    case GE(l1, l2) => GE(shiftNotLin(l1,delta),shiftNotLin(l2,delta))
    case LE(l1, l2) => LE(shiftNotLin(l1,delta),shiftNotLin(l2,delta))
  }




  //New
  def shiftNotLin(notlin: NotLin, delta:Point): NotLin = notlin match {
    case Var(v) => if (delta(v)!=0) Add(notlin,Value(-delta(v))) else notlin // Porquê o menos ??
    case Value(_) => notlin
    case Add(l1, l2) => Add(shiftNotLin(l1,delta),shiftNotLin(l2,delta))
    case Mult(l1, l2) => Mult(shiftNotLin(l1,delta),shiftNotLin(l2,delta))
    case Div(l1,l2) => Div(shiftNotLin(l1,delta),shiftNotLin(l2,delta))
    case Res(l1,l2) => Res(shiftNotLin(l1,delta),shiftNotLin(l2,delta))
    //case Pow(l1,l2) => PowNotLin(shiftNotLin(l1,delta),shiftNotLin(l2,delta))
    case Func(s,list)=> Func(s,list.map((l:NotLin) => shiftNotLin(l,delta)))

     
  }


    // Use quadratic programming to find the intersection point
  def quadraticProgrm(p: Point, ineqs: Set[Ineq]): Option[Point] = {

    val model = MPModel(SolverLib.oJSolver)
    model.start()
    val vars = p.map(kv => kv._1 -> MPFloatVar.positive(kv._1)(model))

    // minimise sum(i) . Xi^2 - 2Pi.Xi
    val squares =
      for ((name,x) <- vars) yield x*x - 2*p(name)*x

    val sumSquares: Expression = squares.fold(0:Expression)(_+_)

 
    minimize(sumSquares)(model)

    // subject to constraints
    for (ineq<-ineqs) {
     
      subjectTo(ineqToConstr(ineq, vars))(model)
    }

  
    val ok = start()(model)
   

    release()(model)


    if (ok) {
      val res = vars.map(kv =>
        kv._1 -> kv._2.value.get // assuming there is a value
      )
      Some(res)
    }
    else
      None

 
  }

  def ineqToConstr(ineq: Ineq, vars: Map[String, MPVar]): Constraint = {
    val small:Point = notlin2point(smallPart(ineq))
    val big:Point   = notlin2point(bigPart(ineq))

    point2Expr(small,vars) <:= point2Expr(big,vars)
  }

  def smallPart(ineq: Ineq): NotLin = ineq match {
    case GT(_, l2) => l2
    case LT(l1, _) => l1
    case GE(_, l2) => l2
    case LE(l1, _) => l1
  }
  def bigPart(ineq: Ineq): NotLin = ineq match {
    case GT(l1, _) => l1
    case LT(_, l2) => l2
    case GE(l1, _) => l1
    case LE(_, l2) => l2
  }
  def point2Expr(p: Point, vars:Map[String,MPVar]): Expression = {
    val x = p.map(kv => vars.get(kv._1) match {
      case Some(mpvar:MPVar) => mpvar * kv._2
      case None => kv._2:Expression
    } )
    x.fold(0:Expression)(_+_)
  }


  ////
  def test(s:String,p:Point,max:Double):Unit = {
    val c = hprog.lang.Parser.parseAll(hprog.lang.Parser.condP,s)
    c match {
      case hprog.lang.Parser.Success(cond, _) =>
        val res = closest(p,cond,max)
        if (res.isEmpty) println("no closest point")
        else
          println(s"got result: ${res.get.map(kv=>kv._1+":"+kv._2).mkString(", ")}")
      case f: hprog.lang.Parser.NoSuccess =>
        println(s"failed to parse - $f")
    }
  }
}
