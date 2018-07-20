package hprog.frontend

import hprog.ast._

object Show {
  def apply(p:Progr): String = p match {
    case Seq(ps) => ps.map(apply).mkString(" ; ")
    case Statement(as,Some(dur)) => addPar(as.map(apply)) +" & "+ apply(dur)
    case Statement(as,None) => as.map(apply).mkString(", ")
  }

  def apply(a:Assgn): String = a match {
    case Assgn(v,e) => s"${apply(v)}:=${apply(e)}"
  }

  private def addPar(l:List[String]): String = l match {
    case _::_::_ =>l.mkString("(",", ",")")
    case _ => l.mkString(", ")
  }

  //   def show(con: Connector, isShort:Boolean): String = con match {
//     case Seq(c1, c2)    => s"${showP(c1,isShort)} ; ${showP(c2,isShort)}"
//     case Par(c1, c2)    => s"${showP(c1,isShort)} ⊗ ${showP(c2,isShort)}"
//     case Id(Port(IVal(1))) => "id"
//     case Id(Port(IVal(0))) => "nil"
//     case Id(x)          => s"Id(${apply(x)})"
//     case Symmetry(i, j) => s"sym(${apply(i)},${apply(j)})"
//     case Trace(i, c)    => s"Tr(${apply(i)})(${show(c,isShort)})"
//     case Prim(name,_,_,_) => name
//     case Exp(a, c)  => s"${showP(c,isShort)}^${showP(a)}"
//     case ExpX(x, a, c)  => s"${showP(c,isShort)}^{${apply(x:IExpr)}<--${apply(a)}}"
//     case Choice(b, c1, c2) => s"${showP(b)} ? ${showP(c1,isShort)} ⊕ ${showP(c2,isShort)}"
//                              //s"if ${showP(b)} then ${showP(c1)} else ${showP(c2)}"

//     case Abs(x,et, c)  => s"\\${apply(x)}:${apply(et)}${showAP(c,isShort)}"
//     case App(c, a)     => s"${showP(c,isShort)}(${apply(a)})"
//     case Restr(c,b)     => s"${showP(c,isShort)} | ${showP(b)}"
//     case SubConnector(name, c, _) if  isShort => if (name=="") showP(c,isShort) else name
//     case SubConnector(name, c, _) if !isShort => (if (name=="") "" else name) + s"{${show(c,isShort)}}"
//   }

//   private def showP(con:Connector,isShort:Boolean): String = con match {
//     case Seq(_,_) | Par(_,_) | Choice(_,_,_) | Abs(_,_,_) |
//          Exp(_,_) | ExpX(_,_,_) | Restr(_,_) => s"(${show(con,isShort)})"

//     case _ => show(con,isShort)
//   }
//   private def showAP(con:Connector,isShort:Boolean): String = con match {
//     case Abs(x,et,c) => s" ${apply(x)}:${apply(et)}${showAP(c,isShort)}"
//     case _ => s".${showP(con,isShort)}"
//   }

//   def apply(itf: Interface): String = itf match {
//     case Tensor(i, j)  => s"${showP(i)} ⊗ ${showP(j)}"
//     case Port(a)       => apply(a)
//     case Repl(i, a)    => s"${showP(i)}^${showP(a)}"
//     case Cond(b, i, j) => s"${showP(b)} ? ${showP(i)} ⊕ ${showP(j)}"
//   }
//   private def showP(itf:Interface):String = itf match {
//     case Port(a) => showP(a)
//     case _ => s"(${apply(itf)})"
//   }

// //  def apply(v:Var): String = v.x
//   def apply(exprType: ExprType) = exprType match {
//     case IntType => "I"
//     case BoolType => "B"
//   }

//   def apply(exp: Expr): String = exp match {
//     case Var(x) =>  x
//     case e: IExpr => applyie(e)
//     case e: BExpr => applybe(e)
//   }

  def apply(exp: Expr): String = exp match {
    case Val(n) => n.toString
    case Var(x,n) => x+(for (_<-1 to n) yield '!').mkString("")
    case Add(e1,e2) => s"${showP(e1)} + ${showP(e2)}"
    case Sub(e1,e2) => s"${showP(e1)} - ${showP(e2)}"
    case Mul(e1,e2) => s"${showP(e1)} * ${showP(e2)}"
    case Div(e1,e2) => s"${showP(e1)} / ${showP(e2)}"
     // bools
    case BVal(b) => b.toString
    case EQ(e1, e2)  => s"${showP(e1)} == ${showP(e2)}"
    case GT(e1, e2)  => s"${showP(e1)} > ${showP(e2)}"
    case LT(e1, e2)  => s"${showP(e1)} < ${showP(e2)}"
    case GE(e1, e2)  => s"${showP(e1)} >= ${showP(e2)}"
    case LE(e1, e2)  => s"${showP(e1)} <= ${showP(e2)}"
    case And(Nil)    => ""
    case And(e::Nil) => apply(e)
    case And(es)     => es.map(showP).mkString(" && ")
    case Or(e1, e2)  => s"${showP(e1)} || ${showP(e2)}"
    case Not(e1)     => s"!${showP(e1)}"

   }

   private def showP(exp:Expr):String = exp match {
    case BVal(_) | Val(_) | Var(_,_) | Not(_) => apply(exp)
    case _ => s"(${apply(exp)})"

   }


//   def applybe(exp: BExpr): String = exp match {
//     case BVal(b)     => b.toString
//     case Var(x)     => x
//     case AndN(x,f,t,e) => s"∧{${apply(f)} ≤ ${x.x} < ${apply(t)}}${showP(e)}"
//   }
//   private def showP(exp:BExpr):String = exp match {
//     case BVal(_) | Var(_) | Not(_) => apply(exp)
//     case _ => s"(${apply(exp)})"
//   }

// //  def showVar(v:Var) = v match {
// //    case Var(x) => x
// //  }

// //  def short(con:Connector): String = con match {
// //    case Seq(c1, c2) =>
// //    case Par(c1, c2) =>
// //    case Id(i) =>
// //    case Symmetry(i, j) =>
// //    case Trace(i, c) =>
// //    case Prim(name, i, j, extra) =>
// //    case SubConnector(name, c1) =>
// //    case Exp(a, c) =>
// //    case ExpX(x, a, c) =>
// //    case Choice(b, c1, c2) =>
// //    case Abs(x, et, c) =>
// //    case App(c, a) =>
// //    case Restr(c, phi) =>
// //  }

//   def apply(typ:Type): String =
//     (if (typ.isGeneral) "" else "© ") +
//       (if (typ.args.vars.isEmpty) "" else "∀"+typ.args.toString+" . ") +
//       apply(typ.i) + " -> "+ apply(typ.j) +
//       (if (typ.const == BVal(b=true) || typ.const == And(List())) ""
//       else " | " + apply(typ.const) )



//   ////////////////
//   //
//   def source(con: Connector): String = con match {
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
//     //s"if ${showP(b)} then ${showP(c1)} else ${showP(c2)}"

//     case Abs(x,et,c)   => s"lam(${apply(x)},${apply(et)},${source(c)})"
// //    case BAbs(x, c)     => s"lam(${apply(x)}${source(c)})"
//     case App(c, a)     => s"${showSP(c)}(${apply(a)})"
// //    case BApp(c, b)     => s"${showSP(c)}(${apply(b)})"
//     case Restr(c,b)     => s"${showSP(c)} | ${showP(b)}"

//     case SubConnector(name, c, _) => if (name=="") showSP(c) else name + s"{${showSP(c)}}"
//   }

//   private def showSP(con:Connector): String = con match {
//     case Seq(_,_) | Par(_,_) | Choice(_,_,_) | Abs(_,_,_) |
//          Exp(_,_) | ExpX(_,_,_) | Restr(_,_) => s"(${source(con)})"
//     case _ => source(con)
//   }

}