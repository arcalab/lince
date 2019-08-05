package hprog.backend

import hprog.ast.SageExpr.{Pure, SExpr, SExprFun}
import hprog.ast._
import hprog.frontend.Eval
import hprog.frontend.Semantics.{SageSolution, Valuation}

object Show {

  def apply(p:Syntax): String = p match {
    case a: At    => apply(a)
//    case Seq(p::Nil) => apply(p)
    case Seq(ps) => ps.map(apply).mkString("\n")
    case Skip    => "skip"
    case ITE(ifP, thenP, elseP) => s"if ${apply(ifP)} then ${apply(thenP)} else ${apply(elseP)} "
    case While(Guard(c), doP)   => s"while (${apply(c)}) { ${apply(doP)} }"
    case While(Counter(i), doP) => s"while ($i) { ${apply(doP)} }"
  }

  def apply(a:At): String = a match {
    case Assign(v, e) => s"${v.v} := ${apply(e)}"
    case DiffEqs(eqs, dur) => apply(eqs)+apply(dur)
  }

  def apply(eqs:List[DiffEq]): String =
    eqs.map(apply).mkString(", ")

  def apply(de: DiffEq): String = de match {
    case DiffEq(v, e) => s"${v.v}' = ${apply(e)}"
  }

  def apply(dur: Dur): String = dur match {
    case For(t)  => s" & ${apply(t)}"
    case Until(c) => s" & ${apply(c)}"
    case Forever => ""
  }

  def apply(lin: Lin): String = apply(lin,Map():Valuation)

  def apply(lin: Lin, vl: Valuation): String = lin match {
    case v:Var => showVar(v,vl,apply[Pure])
    case Value(v) => floatToFraction(v)//if (v-v.toInt == 0) v.toInt.toString else v.toString
    case Add(l1, l2)    => s"${apply(l1,vl)} + ${apply(l2,vl)}"
    case Mult(v, l:Add) => s"${apply(v,vl)}*(${apply(l,vl)})"
    case Mult(v, l)     => s"${apply(v,vl)}*${apply(l,vl)}"
  }

  // show a condition parseable by Sage
  def apply(cond: Cond, vl:Valuation = Map()): String = cond match {
    case BVal(b)     => b.toString
    case And(And(e1,e2),e3) => apply(And(e1,And(e2,e3)),vl)
    case And(e1,e2:And)     => s"${showP(e1,vl)} & ${showP(e2,vl)}"
    case And(e1, e2) => s"${showP(e1,vl)} & ${showP(e2,vl)}"
    case Or(e1, e2)  => s"${showP(e1,vl)} | ${showP(e2,vl)}"
    case Not(e1)     => s"!${showP(e1,vl)}"
    case EQ(v, l)    => s"${showVar(v,vl,applyP)} == ${apply(l,vl)}"
    case GT(v, l)    => s"${showVar(v,vl,applyP)} > ${apply(l,vl)}"
    case LT(v, l)    => s"${showVar(v,vl,applyP)} < ${apply(l,vl)}"
    case GE(v, l)    => s"${showVar(v,vl,applyP)} >= ${apply(l,vl)}"
    case LE(v, l)    => s"${showVar(v,vl,applyP)} <= ${apply(l,vl)}"
  }

  private def showP(exp:Cond, vl:Valuation):String = exp match {
//    case _:Or | _:And => s"(${apply(exp,vl)})"
//    case _ => apply(exp,vl)
    case BVal(b) => b.toString
    case _ => s"(${apply(exp,vl)})"
  }

  private def showVar(v: Var, valuation: Valuation,cont:SExpr => String): String = {
    valuation.get(v.v) match {
      case Some(exp) => cont(Eval.updInput(exp,valuation))
      case None => v.v
    }
  }

  def apply[E<:SageExpr.All](expr: SageExpr[E]): String = expr match {
    case SVal(v) => floatToFraction(v) //f"$v%1.8f"
    case _:SArg  => "_t_"
    case s:SVar  => s.v
    case s:SFun[E]  => s"${s.f}(${s.args.map(apply[E]).mkString(",")})"
    case s:SDiv[E]  => s"${applyP[E](s.e1)}/${applyP[E](s.e2)}"
    case s:SMult[E] => s"${applyP[E](s.e1)}*${applyP[E](s.e2)}"
    case s:SPow[E]  => s"${applyP[E](s.e1)}^${applyP[E](s.e2)}"
    case s:SAdd[E]  => s"${applyP[E](s.e1)}+${applyP[E](s.e2)}"
    case s:SSub[E]  => s"${applyP[E](s.e1)}-${applyP[E](s.e2)}"
  }
  private def applyP[E<:SageExpr.All](expr: SageExpr[E]): String = expr match {
    case SVal(v) =>
      val res = floatToFraction(v)
      if (res.contains('/')) s"($res)" else res
    case _:SArg | _:SVar | _:SFun[E] => apply(expr)
    case _ => "("+apply[E](expr)+")"
  }


  def pp[E<:SageExpr.All](expr: SageExpr[E]): String = expr match {
    case SVal(v) => if (v.round == v) v.toInt.toString else v.toString // f"$v%1.8f"
    case _:SArg  => "t"
    case s:SVar  => s.v
    case SMult(SVal(-1),e)=> s"-${ppP[E](e)}"
    case SMult(e,SVal(-1))=> s"-${ppP[E](e)}"
    case SMult(SVar(x),d@SVal(_))=> s"${pp[E](d)}$x"
    case SMult(SArg(),d@SVal(_))=> s"${pp[E](d)}t"
    case SMult(d@SVal(_),SVar(x))=> s"${pp[E](d)}$x"
    case SMult(d@SVal(_),SArg())=> s"${pp[E](d)}t"
//    case SMult(e1,e2@SAdd(_,_))=> s"${ppP[E](e1)}*${ppP[E](e2)}"
    case SAdd(e1,e2@SAdd(_,_)) => s"${ppP[E](e1)}+${pp[E](e2)}"
//    case SSub(e1,e2@SAdd(_,_)) => s"${ppP[E](e1)}-${ppP[E](e2)}"
//    case SMult(e1@SAdd(_,_),e2)=> s"${ppP[E](e1)}*${ppP[E](e2)}"
    case SAdd(e1@SAdd(_,_),e2)=> s"${pp[E](e1)}+${ppP[E](e2)}"
//    case SSub(e1@SAdd(_,_),e2)=> s"${ppP[E](e1)}-${ppP[E](e2)}"
    case s:SFun[E]  => s"${s.f}(${s.args.map(pp[E]).mkString(",")})"
    case s:SDiv[E]  => s"${ppP[E](s.e1)}/${ppP[E](s.e2)}"
    case s:SMult[E] => s"${ppP[E](s.e1)}*${ppP[E](s.e2)}"
    case s:SPow[E]  => s"${ppP[E](s.e1)}^${ppP[E](s.e2)}"
    case s:SAdd[E]  => s"${ppP[E](s.e1)}+${ppP[E](s.e2)}"
    case s:SSub[E]  => s"${ppP[E](s.e1)}-${ppP[E](s.e2)}"
  }
  private def ppP[E<:SageExpr.All](expr: SageExpr[E]): String = expr match {
    case _:SVal | _:SArg | _:SVar | _:SFun[E] |
         SMult(SArg(),SVal(_))  |  SMult(SVal(_),SArg())  |
         SMult(SVar(_),SVal(_))  |  SMult(SVal(_),SVar(_)) |
         SMult(SVal(-1),_) | SMult(_,SVal(-1))  => pp(expr)
    case _ => "("+pp[E](expr)+")"
  }


  def apply(sol:SageSolution): String =
    sol.map(kv => s"${kv._1}:${apply(kv._2)}").mkString(", ")


  def floatToFraction(v: Double): String = {
    var den: BigInt = 1
    var num: BigDecimal = v
    while (num.toBigIntExact().isEmpty) { // && num < BigDecimal.apply("100000000000000000000000")) {
      num *= 10
      den *= 10
      //println(s"   - $num/$den")
    }
    //println(s"-- $v -> ${num.toBigInt.toString}/${den.toString}")
    if (den == 1) num.toBigInt.toString
    else s"${num.toBigInt.toString}/${den.toString}"
  }



  //  def apply(p:Progr): String = p match {
//    case Seq(ps) => ps.map(apply).mkString(" ; ")
//    case Statement(as,Some(dur)) => addPar(as.map(apply)) +" & "+ apply(dur)
//    case Statement(as,None) => as.map(apply).mkString(", ")
//  }
//
//  def apply(a:Assgn): String = a match {
//    case Assgn(v,e) => s"${apply(v)}:=${apply(e)}"
//  }
//
//  private def addPar(l:List[String]): String = l match {
//    case _::_::_ =>l.mkString("(",", ",")")
//    case _ => l.mkString(", ")
//  }
//
//
//  def apply(exp: Expr): String = exp match {
//    case Val(n) => n.toString
//    case Var(x,n) => x+(for (_<-1 to n) yield '\'').mkString("")
//    case Add(e1,e2) => s"${showP(e1)} + ${showP(e2)}"
//    case Sub(e1,e2) => s"${showP(e1)} - ${showP(e2)}"
//    case Mul(e1,e2) => s"${showP(e1)} * ${showP(e2)}"
//    case Div(e1,e2) => s"${showP(e1)} / ${showP(e2)}"
//     // bools
//    case BVal(b) => b.toString
//    case EQ(e1, e2)  => s"${showP(e1)} == ${showP(e2)}"
//    case GT(e1, e2)  => s"${showP(e1)} > ${showP(e2)}"
//    case LT(e1, e2)  => s"${showP(e1)} < ${showP(e2)}"
//    case GE(e1, e2)  => s"${showP(e1)} >= ${showP(e2)}"
//    case LE(e1, e2)  => s"${showP(e1)} <= ${showP(e2)}"
//    case And(Nil)    => ""
//    case And(e::Nil) => apply(e)
//    case And(es)     => es.map(showP).mkString(" && ")
//    case Or(e1, e2)  => s"${showP(e1)} || ${showP(e2)}"
//    case Not(e1)     => s"!${showP(e1)}"
//
//  }
//
//  private def showP(exp:Expr):String = exp match {
//    case BVal(_) | Val(_) | Var(_,_) | Not(_) => apply(exp)
//    case _ => s"(${apply(exp)})"
//
//  }
//
////   ////////////////
////   //
//  def source(progr: Progr): String = progr match {
//    case Seq(ps) => "Seq("+ps.map(source).map(indent).mkString("","\n",")")
//    case Statement(st, dur) => s"Statement(\n  st:  ${st.map(source).mkString(", ")}"+
//      (dur match {
//        case Some(e) => s"\n  dur: ${apply(e)} )"
//        case None => ")"
//      })
//}
//  def source(assgn: Assgn): String = {
//    s"Var(${assgn.v.name}, ${assgn.v.der}) := Expr(${assgn.e})"
//  }
//  def indent(s:String):String = {
//    s.split("\n").map("  "+_).mkString("\n")
//  }


//
//     case Seq(c1, c2)    => s"${showSP(c1)} & ${showSP(c2)}"
//     case Par(c1, c2)    => s"${showSP(c1)} * ${showSP(c2)}"
//     case Id(Port(IVal(1))) => "id"
//     case Id(Port(IVal(0))) => "(id^0)"
//     case Id(x)          => s"(id^${showP(x)})"
//     case Symmetry(i, j) => s"sym(${apply(i)},${apply(j)})"
//     case Trace(i, c)    => s"Tr(${apply(i)},${source(c)})"
//     case Prim(name,_,_,_) => name
//     case Exp(a, c)  => s"${showSP(c)}^${showP(a)}"
//     case ExpX(x, a, c)  => s"(${showSP(c)}^(${apply(x)}<--${showP(a)}))"
//     case Choice(b, c1, c2) => s"${showP(b)} ? ${showSP(c1)} + ${showSP(c2)}"
     //s"if ${showP(b)} then ${showP(c1)} else ${showP(c2)}"

//     case Abs(x,et,c)   => s"lam(${apply(x)},${apply(et)},${source(c)})"
// //    case BAbs(x, c)     => s"lam(${apply(x)}${source(c)})"
//     case App(c, a)     => s"${showSP(c)}(${apply(a)})"
// //    case BApp(c, b)     => s"${showSP(c)}(${apply(b)})"
//     case Restr(c,b)     => s"${showSP(c)} | ${showP(b)}"
//
//     case SubConnector(name, c, _) => if (name=="") showSP(c) else name + s"{${showSP(c)}}"
//   }

//   private def showSP(con:Connector): String = con match {
//     case Seq(_,_) | Par(_,_) | Choice(_,_,_) | Abs(_,_,_) |
//          Exp(_,_) | ExpX(_,_,_) | Restr(_,_) => s"(${source(con)})"
//     case _ => source(con)
//   }

}