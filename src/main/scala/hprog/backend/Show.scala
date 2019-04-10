package hprog.backend

import hprog.ast._

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
    case DiffEqs(eqs, dur) => eqs.map(apply).mkString(", ")+apply(dur)
  }

  def apply(de: DiffEq): String = de match {
    case DiffEq(v, e) => s"${v.v}' = ${apply(e)}"
  }

  def apply(dur: Dur): String = dur match {
    case For(t)  => s" & ${apply(t)}"
    case Until(c) => s" & ${apply(c)}"
    case Forever => ""
  }

  def apply(lin: Lin): String = lin match {
    case Var(v) => v
    case Value(v) => if (v-v.toInt == 0) v.toInt.toString else v.toString
    case Add(l1, l2) => s"${apply(l1)} + ${apply(l2)}"
    case Mult(v, l:Add) =>  s"${apply(v)}*(${apply(l)})"
    case Mult(v, l) =>  s"${apply(v)}*${apply(l)}"
  }

  def apply(cond: Cond): String = cond match {
    case BVal(b)     => b.toString
    case And(And(e1,e2),e3) => apply(And(e1,And(e2,e3)))
    case And(e1,e2:And)     => s"${showP(e1)} /\\ ${apply(e2)}"
    case And(e1, e2) => s"${showP(e1)} /\\ ${showP(e2)}"
    case Or(e1, e2)  => s"${showP(e1)} \\/ ${showP(e2)}"
    case Not(e1)     => s"!${showP(e1)}"
    case EQ(v, l)    => s"${v.v} = ${apply(l)}"
    case GT(v, l)    => s"${v.v} > ${apply(l)}"
    case LT(v, l)    => s"${v.v} < ${apply(l)}"
    case GE(v, l)    => s"${v.v} >= ${apply(l)}"
    case LE(v, l)    => s"${v.v} <= ${apply(l)}"
  }

  private def showP(exp:Cond):String = exp match {
    case _:Or | _:And => s"(${apply(exp)})"
    case _ => apply(exp)
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