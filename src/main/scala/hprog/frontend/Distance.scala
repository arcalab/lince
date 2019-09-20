package hprog.frontend

//import breeze.numerics.{pow, sqrt}
import hprog.ast
import hprog.ast._
import hprog.frontend.CommonTypes.Point
import optimus.algebra.{Constraint, Expression}
import optimus.optimization._
import optimus.optimization.enums.SolverLib
import optimus.optimization.model.{MPFloatVar, MPVar}

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
    //println(s"got closest to ${p} when ${Show(c)} - ${res}")
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
      //case EQ(l1, l2) => Eval(p,l1) == Eval(p,l2)
      case GT(l1, l2) => Eval(p,l1) >  Eval(p,l2)
      case LT(l1, l2) => Eval(p,l1) <  Eval(p,l2)
      case GE(l1, l2) => Eval(p,l1) >= Eval(p,l2)
      case LE(l1, l2) => Eval(p,l1) <= Eval(p,l2)
    }
  }
  //  case class EQ(l1:Lin,l2:Lin)      extends Ineq
  case class GT(l1:Lin,l2:Lin)      extends Ineq
  case class LT(l1:Lin,l2:Lin)      extends Ineq
  case class GE(l1:Lin,l2:Lin)      extends Ineq
  case class LE(l1:Lin,l2:Lin)      extends Ineq



  def normaliseCond(c:Cond): DNF = c match {
    case BVal(true) => Or(Set(And(Set())))
    case BVal(false) => Or(Set())
    case ast.And(c1, c2) => (normaliseCond(c1),normaliseCond(c2)) match {
      case (Or(ands1),Or(ands2)) => Or( for(a1<-ands1;a2<-ands2) yield And(a1.ineqs++a2.ineqs))
    }
    case ast.Or(c1, c2) =>  (normaliseCond(c1),normaliseCond(c2)) match {
      case (Or(ands1), Or(ands2)) => Or(ands1++ands2)
    }
    case Not(c2) => normaliseCond(swapOps(c2))
    case ast.EQ(v, l) => Or(Set(And(Set(GE(v,l),LE(v,l)))))
    case ast.GT(v, l) => Or(Set(And(Set(GT(v,l)))))
    case ast.LT(v, l) => Or(Set(And(Set(LT(v,l)))))
    case ast.GE(v, l) => Or(Set(And(Set(GE(v,l)))))
    case ast.LE(v, l) => Or(Set(And(Set(LE(v,l)))))
  }

  def swapOps(c: Cond): Cond = c match {
    case BVal(b)        => BVal(!b)
    case ast.And(c1, c2)=> ast.Or(swapOps(c1),swapOps(c2))
    case ast.Or(c1, c2) => ast.And(swapOps(c1),swapOps(c2))
    case Not(c2)        => c2
    case ast.EQ(v, l)   => ast.Or(ast.LT(v,l),ast.GT(v,l))
    case ast.GT(v, l)   => ast.LE(v,l)
    case ast.LT(v, l)   => ast.GE(v,l)
    case ast.GE(v, l)   => ast.LT(v,l)
    case ast.LE(v, l)   => ast.GT(v,l)
  }

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

  def closest(p:Point, l1:Lin, l2:Lin): Point = {
    val delta = neg(p)
    val p1 = lin2point(shiftLin(l1,delta))
    val p2 = lin2point(shiftLin(l2,delta))
    //    val plane = add(add(p1,neg(p2)),neg(p)) // p1-p2 - p
    val plane = add(p1,neg(p2))
    val d = 0.0 - plane.getOrElse("",0.0) // extra value without variable from p1-p2
    val plane2 = plane - "" // take out empty variable
    val p3 = closestToOrigin(plane2,d) // got closest to origin
    //println(s"closest to origin solving ${plane2} = $d\n returned $p3")
    add(p3 , p) // add point p in the end
  }

  def lin2point(lin: Lin): Point = lin match {
    case Var(v) => Map(v->1.0)
    case Value(v) => Map("" -> v)
    case Add(l1, l2) => add(lin2point(l1),lin2point(l2))
    case Mult(v, l) => lin2point(l).mapValues(_ * v.v)
  }
  def add(p1:Point,p2:Point): Point =
    p1 ++ (for ((x,v) <- p2) yield x -> (p1.getOrElse(x,0.0)+v))

  def neg(p1:Point): Point =
    p1.mapValues(_*(-1))

  // v*d / |v|^2
  def closestToOrigin(plane:Point,d:Double): Point = {
    val norm = plane.values.map(math.pow(_,2)).sum
    plane.mapValues(x => x*d/norm)
  }

  def left(ineq: Ineq): Lin = ineq match {
    //case EQ(l1,_) => l1
    case GT(l1,_) => l1
    case LT(l1,_) => l1
    case GE(l1,_) => l1
    case LE(l1,_) => l1
  }
  def right(ineq: Ineq): Lin = ineq match {
    //case EQ(_,l2) => l2
    case GT(_,l2) => l2
    case LT(_,l2) => l2
    case GE(_,l2) => l2
    case LE(_,l2) => l2
  }


  // need to shift based on `max` because variables will only get positive values
  def quadraticProgrm(p: Point, ineqs: Set[Ineq], max:Double): Option[Point] = {
    val delta = p.mapValues(x => if (x<max) max-x else 0)
    //println(s"before: ${ineqs.mkString(", ")}")
    //println(s"delta: ${delta.map(kv=>kv._1+"->"+kv._2).mkString(", ")}")
    val shiftedP = add(p,delta)
    val shiftedIneqs = ineqs.map(i => shiftIneq(i,delta))
    //println(s"after:  ${shiftedIneqs.mkString(", ")}")
    quadraticProgrm(shiftedP,shiftedIneqs)
      .map(p2 => add(p2,neg(delta)))
  }

  def shiftIneq(ineq: Ineq, delta:Point): Ineq = ineq match {
    case GT(l1, l2) => GT(shiftLin(l1,delta),shiftLin(l2,delta))
    case LT(l1, l2) => LT(shiftLin(l1,delta),shiftLin(l2,delta))
    case GE(l1, l2) => GE(shiftLin(l1,delta),shiftLin(l2,delta))
    case LE(l1, l2) => LE(shiftLin(l1,delta),shiftLin(l2,delta))
  }
  def shiftLin(lin: Lin, delta:Point): Lin = lin match {
    case Var(v) => if (delta(v)!=0) Add(lin,Value(-delta(v))) else lin
    case Value(_) => lin
    case Add(l1, l2) => Add(shiftLin(l1,delta),shiftLin(l2,delta))
    case Mult(v, l) => Mult(v,shiftLin(l,delta))
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
  At last, we can solve the problem by starting the solver and displaying the results:

start()
println(s"objective: $objectiveValue")
println(s"x = ${x.value} y = ${y.value}")

  Finally, don't forget to release the memory used by the internal solver:

release()
     */
  }

  def ineqToConstr(ineq: Ineq, vars: Map[String, MPVar]): Constraint = {
    val small:Point = lin2point(smallPart(ineq))
    val big:Point   = lin2point(bigPart(ineq))

    point2Expr(small,vars) <:= point2Expr(big,vars)
  }

  def smallPart(ineq: Ineq): Lin = ineq match {
    case GT(_, l2) => l2
    case LT(l1, _) => l1
    case GE(_, l2) => l2
    case LE(l1, _) => l1
  }
  def bigPart(ineq: Ineq): Lin = ineq match {
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
