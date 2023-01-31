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




  // PEDIR AO PROF AJUDA PARA PERCEBER MELHOR
  def closest(p:Point,c:Cond,max:Double): Option[Point] = {
    val res = if (max<=0) {
      if (normaliseCond(c) contains p) Some(p) else None
    }
    else closest(p, normaliseCond(c), max).flatMap(p2 =>
        if (dist(p,p2)<=max) Some(p2) else None
    )
   
    res
  }




// PEDIR AO PROF AJUDA PARA PERCEBER MELHOR
  type DNF = Or
  case class Or(ands:Set[And]) {
    def contains(p:Point): Boolean = ands.exists(_ contains p)
  }
  case class And(ineqs:Set[Ineq]) {
    def contains(p:Point): Boolean = ineqs.forall(_ contains p)
  }
  sealed abstract class Ineq {
    def contains(p:Point): Boolean = this match {
      //case EQ(l1, l2) => Eval(p,l1) == Eval(p,l2)
      case GT(l1, l2) => Eval(p,l1) >  Eval(p,l2) // COMO FOI POSSÍVEL ACEDER AO EVAL??
      case LT(l1, l2) => Eval(p,l1) <  Eval(p,l2)
      case GE(l1, l2) => Eval(p,l1) >= Eval(p,l2)
      case LE(l1, l2) => Eval(p,l1) <= Eval(p,l2)
    }
  }



  // ALTEREI!!!!!!!!!!!!
  case class GT(l1:NotLin,l2:NotLin)      extends Ineq
  case class LT(l1:NotLin,l2:NotLin)      extends Ineq
  case class GE(l1:NotLin,l2:NotLin)      extends Ineq
  case class LE(l1:NotLin,l2:NotLin)      extends Ineq

//////////////////////////////////////////////////////////////////

// ALTEREI!

// TENHO DUVIDAS NOS BVALS, AND E OR
  def normaliseCond(c:Cond): DNF = c match {
    case BVal(true) => Or(Set(And(Set())))
    case BVal(false) => Or(Set()) // PORQUE ESTE É ASSIM E O DO BVAL(TRUE) NÃO?
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
  

  // no fundo faz o swap da condição
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
//      val p1 = closest(p1,and)
//      val p2 = closest(p1,Or(dnf.ands.tail))
//      if (dist(p,p1) <= dist(p,p2)) p1 else p2
//    case None => throw new RuntimeException(s"No point in the domain of $dnf.")
  }

  def dist(p1:Point,p2:Point): Double = {
    val sqrs = for ((x,v) <- p1) yield math.pow(p2.getOrElse(x,v)-v,2)
    math.sqrt(sqrs.sum)
  }


  def closest(p:Point,and:And,max:Double): Option[Point] =
    closest(p,and.ineqs.map(closeIneq),max)
//      .flatMap(p2 =>
//      if (and contains p2) Some(p2)
//      else None
//    )


  def closest(p:Point,ineqs:Set[Ineq],max:Double): Option[Point] = {
    for (ineq <- ineqs) {
      //println(s"manual closest of ${p} to ${ineq}")
      val p2: Point = closest(p,ineq)
      //println(s"got ${p2}")
      if (And(ineqs - ineq) contains p2) return Some(p2)
    }
    // no trivial solution; solve quadratic programming optimisation problem
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
    //    val plane = add(add(p1,neg(p2)),neg(p)) // p1-p2 - p
    val plane = add(p1,neg(p2))
    val d = 0.0 - plane.getOrElse("",0.0) // extra value without variable from p1-p2
    val plane2 = plane - "" // take out empty variable
    val p3 = closestToOrigin(plane2,d) // got closest to origin
    //println(s"closest to origin solving ${plane2} = $d\n returned $p3")
    add(p3 , p) // add point p in the end
  }
// ALTEREI!!!!
  def notlin2point(notlin: NotLin): Point = notlin match {
    case VarNotLin(v) => Map(v->1.0)
    case ValueNotLin(v) => Map("" -> v)
    case AddNotLin(l1, l2) => add(notlin2point(l1),notlin2point(l2))
    case MultNotLin(l1, l2) => mul(notlin2point(l1),notlin2point(l2))
    case DivNotLin(l1, l2) => div(notlin2point(l1),notlin2point(l2))
    case ResNotLin(l1, l2) => res(notlin2point(l1),notlin2point(l2))
    //case SinNotLin(l1) => seno(notlin2point(l1)) 
    //case CosNotLin(l1) => cosseno(notlin2point(l1)) 
    //case TanNotLin(l1) => tangente(notlin2point(l1))
    case PowNotLin(l1, l2) => powdef(notlin2point(l1),notlin2point(l2))
    case FuncNotLin(s,list) => funcdef(s,list.map((l:NotLin) => notlin2point(l)))
    //case SqrtNotLin(l1, l2) => ???

    }

  // Porque só tem 'x' depois do yield?   
  def add(p1:Point,p2:Point): Point =
    p1 ++ (for ((x,v) <- p2) yield x -> (p1.getOrElse(x,0.0)+v)) // getOrElse retira o valor de p1 e se não existir retorna (x,0)


//////////////////////////////////////////////////////////////////

// ADICIONEI ESTAs (PROVAVELMENTE ESTÁ MAL)
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


  def powdef(p1:Map[String,Double],p2:Map[String,Double]): Map[String,Double]=
   p1 ++ (for ((x,v) <- p2) yield x -> (if (p1.contains(x))  pow(p1(x),v) else v))

  
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
          case (_)=>throw new RuntimeException(s"Unknown function '${s}',or the number of arguments are incorrect")
        }  
      }

    }
  }
/*
  def cosseno(p1:Point): Point=
    p1.mapValues((x,v)=>(x,cos(v)))

  def tangente(p1:Point): Point=
    p1.mapValues((x,v)=>(x,tan(v)))


  def powdef(p1:Point,p2:Point): Point=
  

  def sqrtdef(p1:Point,p2:Point): Point=
*/
  
/////////////////////////////////////////////////////////////////


  def neg(p1:Point): Point =
    p1.view.mapValues(_*(-1)).toMap

  // v*d / |v|^2
  def closestToOrigin(plane:Point,d:Double): Point = {
    val norm = plane.values.map(math.pow(_,2)).sum
    plane.view.mapValues(x => x*d/norm).toMap
  }

  def left(ineq: Ineq): NotLin = ineq match {
    //case EQ(l1,_) => l1
    case GT(l1,_) => l1
    case LT(l1,_) => l1
    case GE(l1,_) => l1
    case LE(l1,_) => l1
  }
  def right(ineq: Ineq): NotLin = ineq match {
    //case EQ(_,l2) => l2
    case GT(_,l2) => l2
    case LT(_,l2) => l2
    case GE(_,l2) => l2
    case LE(_,l2) => l2
  }


  // need to shift based on `max` because variables will only get positive values
  def quadraticProgrm(p: Point, ineqs: Set[Ineq], max:Double): Option[Point] = {
    val delta = p.view.mapValues(x => if (x<max) max-x else 0).toMap
    //println(s"before: ${ineqs.mkString(", ")}")
    //println(s"delta: ${delta.map(kv=>kv._1+"->"+kv._2).mkString(", ")}")
    val shiftedP = add(p,delta)
    val shiftedIneqs = ineqs.map(i => shiftIneq(i,delta))
    //println(s"after:  ${shiftedIneqs.mkString(", ")}")
    quadraticProgrm(shiftedP,shiftedIneqs)
      .map(p2 => add(p2,neg(delta)))
  }

  def shiftIneq(ineq: Ineq, delta:Point): Ineq = ineq match {
    case GT(l1, l2) => GT(shiftNotLin(l1,delta),shiftNotLin(l2,delta))
    case LT(l1, l2) => LT(shiftNotLin(l1,delta),shiftNotLin(l2,delta))
    case GE(l1, l2) => GE(shiftNotLin(l1,delta),shiftNotLin(l2,delta))
    case LE(l1, l2) => LE(shiftNotLin(l1,delta),shiftNotLin(l2,delta))
  }




  //ALTEREI!!!
  def shiftNotLin(notlin: NotLin, delta:Point): NotLin = notlin match {
    case VarNotLin(v) => if (delta(v)!=0) AddNotLin(notlin,ValueNotLin(-delta(v))) else notlin // Porquê o menos ??
    case ValueNotLin(_) => notlin
    case AddNotLin(l1, l2) => AddNotLin(shiftNotLin(l1,delta),shiftNotLin(l2,delta))
    case MultNotLin(l1, l2) => MultNotLin(shiftNotLin(l1,delta),shiftNotLin(l2,delta))
    case DivNotLin(l1,l2) => DivNotLin(shiftNotLin(l1,delta),shiftNotLin(l2,delta))
    case ResNotLin(l1,l2) => ResNotLin(shiftNotLin(l1,delta),shiftNotLin(l2,delta))
    //case SinNotLin(l1) => SinNotLin(shiftNotLin(l1,delta))
    //case CosNotLin(l1) => CosNotLin(shiftNotLin(l1,delta))
    //case TanNotLin(l1) => TanNotLin(shiftNotLin(l1,delta))
    case PowNotLin(l1,l2) => PowNotLin(shiftNotLin(l1,delta),shiftNotLin(l2,delta))
    case FuncNotLin(s,list)=> FuncNotLin(s,list.map((l:NotLin) => shiftNotLin(l,delta)))
    //case SqrtNotLin(l1,l2) => SqrtNotLin(shiftNotLin(l1,delta),shiftNotLin(l2,delta))
     
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

    //println(s"minimizing: $sumSquares")
    minimize(sumSquares)(model)

    // subject to constraints
    for (ineq<-ineqs) {
      ///println(s"add constraint: ${ineqToConstr(ineq,vars)}")
      subjectTo(ineqToConstr(ineq, vars))(model)
    }

    //println(s"constraints: ${ineqs.mkString(",")}")
    val ok = start()(model)
    //println(s" - $ok")

    release()(model)


    if (ok) {
      val res = vars.map(kv =>
        kv._1 -> kv._2.value.get // assuming there is a value
      )
      Some(res)
    }
    else
      None

    /*
import optimus.optimization._
import optimus.optimization.enums.SolverLib
import optimus.optimization.model.MPFloatVar

implicit val model = MPModel(SolverLib.oJSolver)

  Ok! Let's create a couple of variables:

// Both variables are positive, that is, bounds are in [0, +inf]
val x = MPFloatVar.positive("x")
val y = MPFloatVar.positive("y")

  Then we can define our optimization problem subject to a couple of constraints using our known maths:

minimize(-8*x - 16*y + x*x + 4*y^2)
subjectTo(
          x + y <:= 5,
          x <:= 3
         )
  At last.Syntax, we can solve the problem by starting the solver and displaying the results:

start()
println(s"objective: $objectiveValue")
println(s"x = ${x.value} y = ${y.value}")

  Finally, don't forget to release the memory used by the internal solver:

release()
     */
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
        //println("parsed")
        val res = closest(p,cond,max)
        if (res.isEmpty) println("no closest point")
        else
          println(s"got result: ${res.get.map(kv=>kv._1+":"+kv._2).mkString(", ")}")
      case f: hprog.lang.Parser.NoSuccess =>
        println(s"failed to parse - $f")
    }
  }
}
