package hprog.backend

import hprog.ast.SymbolicExpr.{Pure, SyExpr}
import hprog.ast._
import Syntax._
import hprog.frontend.CommonTypes.{SySolution, Valuation}
import hprog.frontend.{Eval, Traj}
import hprog.frontend.Traj.RunTarget


/*
Método map():
collection = (e1, e2, e3, ...)

func is some function collection.map(func)

returns collection(func(e1), func(e2), func(e3), ...)

*/



/*
Método mkString():
Method Definition: def mkString(sep: String): String

Return Type: It returns all the elements of the list in a string along with a separator.
*/


object Show {


  def apply(p:Syntax): String = p match {
    case Atomic(as, de) => // Se for um atómico
      val assg = as.map(a => s"${a.v.v}:=${apply(a.e)}") // Guardo aqui os assigments, em forma de string, tipo: " v:= 37", no fim retorna uma lista disso
      val eqs = de.eqs.map(apply) // Guardo as equações diferenciais também em forma de string, tipo: "v'= v+2", no fim retorna uma lista disso
      aux_gramatic(assg.mkString(";"),eqs.mkString(","),apply(de.dur))++";"// Concateno as listas de assg e eqs, aplico o mkString para separar os assigments por virgulas e as eqs dif e no fim colo a duração, retornando uma string disso tudo

    case Seq(p, q) => List(apply(p),apply(q)).filter(_.nonEmpty).mkString("\n")
    case ITE(ifP, thenP, elseP) =>  s"if ${apply(ifP)} then {${apply(thenP)}} else {${apply(elseP)}} "
    case While(pre, Guard(c), doP) => apply(pre) + "\n" + s"while ${apply(c)} do { ${apply(doP)} }"
    case While(pre, Counter(i), doP) => apply(pre) + "\n" +  s"while $i do { ${apply(doP)} }"
  }


  def apply(eqs:List[DiffEq]): String =
    eqs.map(apply).mkString(",")
  
// coloca os ; nos sitios corretos, antes não estavam e dava erro no parsing
  def aux_gramatic(ass:String,diff:String,dur:String):String={
    if (ass.length==0){
      return (diff + dur)
    } else {
      if ((diff.length!=0) || (dur.length!=0)){
        return (ass+";"+diff+dur)
      } else {
        return ass
      }
    }
  }  
/*
    if (eqs.length != 0){
      var s = ";" + eqs.map(apply).mkString(",")
      return s
    } else {
      return ";"
    }
*/ 

  def apply(de: DiffEq): String = de match {
    case DiffEq(v, e) => s"${v.v}'=${apply(e)}"
  }

  def apply(dur: Dur): String = dur match {
    case For(t)  => s" for ${apply(t)}" // retorna a string " for" ++ string da espressão notlin
    case Until(c,e,j) => s" until_$e${
      if(j.isDefined)","+j.get else ""} ${apply(c)}"  // retorna a string "until_" ++ string do esp ++ espaço ++ string das condições
    case Forever => " forever"
  }


  def apply(lin: Lin): String = apply(lin,Map():Valuation)

  def apply(lin: Lin, vl: Valuation): String = lin match {
    case v:Var => showVar(v,vl,apply[Pure])
    case Value(v) => floatToFraction(v)//if (v-v.toInt == 0) v.toInt.toString else v.toString
    case Add(l1, l2)    => s"${apply(l1,vl)} + ${apply(l2,vl)}"
    case Mult(v, l:Add) => s"${apply(v,vl)}*(${apply(l,vl)})"
    case Mult(v, l)     => s"${apply(v,vl)}*${apply(l,vl)}"
  }

/*
  def apply(lin: Lin): String = apply(lin,Map():Valuation)

  def apply(lin: Lin, vl: Valuation): String = lin match {
    case v:Var => showVar(v,vl,apply[Pure])
    // Se Value for um inteiro vai imprimir sem o zero, senão imprime com os decimais	
    
    case Value(v) => floatToFraction(v)//if (v-v.toInt == 0) v.toInt.toString else v.toString
    
    case Add(l1, l2)    => s"${apply(l1,vl)} + ${apply(l2,vl)}"
    case Mult(l1:Add, l2:Add) => s"(${apply(l1,vl)})*(${apply(l2,vl)})" 
    case Mult(l1, l2:Add) => s"${apply(l1,vl)}*(${apply(l2,vl)})"
    case Mult(l1:Add, l2) => s"(${apply(l1,vl)})*${apply(l2,vl)}"
    case Mult(l1, l2) => s"${apply(l1,vl)}*${apply(l2,vl)}" 
    /*
    case Div(l1:Add, l2:Add) => s"(${apply(l1,vl)})/(${apply(l2,vl)})" 
    case Div(l1, l2:Add) => s"${apply(l1,vl)}/(${apply(l2,vl)})"
    case Div(l1:Add, l2) => s"(${apply(l1,vl)})/${apply(l2,vl)}"
    case Div(l1, l2) => s"${apply(l1,vl)}/${apply(l2,vl)}"
    

    case Res(l1:Add, l2:Add) => s"(${apply(l1,vl)})%(${apply(l2,vl)})" 
    case Res(l1, l2:Add) => s"${apply(l1,vl)}%(${apply(l2,vl)})"
    case Res(l1:Add, l2) => s"(${apply(l1,vl)})%${apply(l2,vl)}"
    case Res(l1, l2) => s"${apply(l1,vl)}%${apply(l2,vl)}"



    case Sin(l1) => s"sin(${apply(l1,vl)})" 
    case Cos(l1) => s"cos(${apply(l1,vl)})" 
    case Tan(l1) => s"tan(${apply(l1,vl)})"
    case Pow(l1,l2)=> s"pow(${apply(l1,vl)},${apply(l2,vl)})"
    case Sqrt(l1,l2)=> s"sqrt(${apply(l1,vl)},${apply(l2,vl)})"
    */
  }

*/

  
  def apply(notlin:NotLin): String= apply(notlin,Map():Valuation)

  def apply(notlin: NotLin, vl: Valuation): String = notlin match {
    case v:VarNotLin => showVarNotLin(v,vl,apply[Pure])
    // Se Value for um inteiro vai imprimir sem o zero, senão imprime com os decimais	
    case ValueNotLin(v) => floatToFraction(v)//if (v-v.toInt == 0) v.toInt.toString else v.toString
    
    // Aqui estes não estão bem, pois agora são expressões lineares no argumento, não outra coisa!
    // Alterei isto tudo !!
    case AddNotLin(l1, l2)    => s"${apply(l1,vl)} + ${apply(l2,vl)}"
    case MultNotLin(l1:AddNotLin, l2:AddNotLin) => s"(${apply(l1,vl)})*(${apply(l2,vl)})" 
    case MultNotLin(l1, l2:AddNotLin) => s"${apply(l1,vl)}*(${apply(l2,vl)})"
    case MultNotLin(l1:AddNotLin, l2) => s"(${apply(l1,vl)})*${apply(l2,vl)}"
    case MultNotLin(l1, l2) => s"${apply(l1,vl)}*${apply(l2,vl)}"

    case DivNotLin(l1:AddNotLin, l2:AddNotLin) => s"(${apply(l1,vl)})/(${apply(l2,vl)})" 
    case DivNotLin(l1, l2:AddNotLin) => s"${apply(l1,vl)}/(${apply(l2,vl)})"
    case DivNotLin(l1:AddNotLin, l2) => s"(${apply(l1,vl)})/${apply(l2,vl)}"
    case DivNotLin(l1, l2) => s"${apply(l1,vl)}/${apply(l2,vl)}"
    

    case ResNotLin(l1:AddNotLin, l2:AddNotLin) => s"(${apply(l1,vl)})%(${apply(l2,vl)})" 
    case ResNotLin(l1, l2:AddNotLin) => s"${apply(l1,vl)}%(${apply(l2,vl)})"
    case ResNotLin(l1:AddNotLin, l2) => s"(${apply(l1,vl)})%${apply(l2,vl)}"
    case ResNotLin(l1, l2) => s"${apply(l1,vl)}%${apply(l2,vl)}"



    //case SinNotLin(l1) => s"sin(${apply(l1,vl)})" 
    //case CosNotLin(l1) => s"cos(${apply(l1,vl)})" 
    //case TanNotLin(l1) => s"tan(${apply(l1,vl)})"
    case PowNotLin(l1,l2)=> s"pow(${apply(l1,vl)},${apply(l2,vl)})"
    case FuncNotLin(s,list)=>s"${s}(${stringList(list)})"
    //case PowNotLin(l1,DivNotLin(ValueNotLin(1),l2))=> s"sqrt(${apply(l1,vl)},${apply(l2,vl)})"
  }

def stringList(list:List[NotLin]): String = list match{
    case List() => s""
    case n::List() => s"${apply(n)}"
    case n::ns => s"${apply(n)},${stringList(ns)}"
    //case n => s"${apply(n)}"
  }

  // show a condition parseable by Sage
  def apply(cond: Cond, vl:Valuation = Map()): String = cond match {
    case BVal(b)     => b.toString
    case And(And(e1,e2),e3) => apply(And(e1,And(e2,e3)),vl) // Para que serve este caso ??
    case And(e1,e2:And)     => s"${showP(e1,vl)} /\\ ${showP(e2,vl)}" // E este??
    case And(e1, e2) => s"${showP(e1,vl)} & ${showP(e2,vl)}"
    case Or(e1, e2)  => s"${showP(e1,vl)} | ${showP(e2,vl)}"
    case Not(EQ(l1,l2)) => s"${apply(l1,vl)}!=${apply(l2,vl)}"
    case Not(e1)     => s"!(${showP(e1,vl)})"
    case EQ(l1, l2)    => s"${apply(l1,vl)}==${apply(l2,vl)}"
    case GT(l1, l2)    => s"${apply(l1,vl)}>${apply(l2,vl)}"
    case LT(l1, l2)    => s"${apply(l1,vl)}<${apply(l2,vl)}"
    case GE(l1, l2)    => s"${apply(l1,vl)}>=${apply(l2,vl)}"
    case LE(l1, l2)    => s"${apply(l1,vl)}<=${apply(l2,vl)}"
    //case IsoletedCond(l1) => s"${apply(l1,vl)}"
  }
  
  // já vi e faz sentido
  private def showP(exp:Cond, vl:Valuation):String = exp match {
    case BVal(b) => b.toString
    case _ => s"(${apply(exp,vl)})"
  }


  // O que é o SyExpr?
  private def showVarNotLin(v: VarNotLin, valuation: Valuation,cont:SyExpr => String): String = {
    valuation.get(v.v) match {
      case Some(exp) => cont(Eval.updInput(exp,valuation))  // NÃO PERCEBI!!!!!!!!!!
      case None => v.v 
    }
  }


// O QUE FAZ ESTA FUNÇÃO E AS SEGUINTES??? 
  // O que é o SyExpr?
  private def showVar(v: Var, valuation: Valuation,cont:SyExpr => String): String = {
    valuation.get(v.v) match {
      case Some(exp) => cont(Eval.updInput(exp,valuation))  // NÃO PERCEBI!!!!!!!!!!
      case None => v.v 
    }
  }



  // NÃO PERCEBI PARA O QUE SERVE
  def apply[E<:SymbolicExpr.All](expr: SymbolicExpr[E]): String = expr match {
    case SVal(v) => floatToFraction(v) //f"$v%1.8f"
    case _:SArg  => "_t_"
    case s:SVar  => s.v
    case SFun("PI",Nil) => "pi"
    case s:SFun[E]  => s"${s.f}(${s.args.map(apply[E]).mkString(",")})"
    case s:SDiv[E]  => s"${applyP[E](s.e1)}/${applyP[E](s.e2)}"
    case s:SRes[E]  => s"${applyP[E](s.e1)}%${applyP[E](s.e2)}"
    case s:SMult[E] => s"${applyP[E](s.e1)}*${applyP[E](s.e2)}"
    case s:SPow[E]  => s"${applyP[E](s.e1)}^${applyP[E](s.e2)}"
    case s:SAdd[E]  => s"${applyP[E](s.e1)}+${applyP[E](s.e2)}"
    case SSub(SVal(0),v:SymbolicExpr[E]) => s"-${applyP[E](v)}"
    case s:SSub[E]  => s"${applyP[E](s.e1)}-${applyP[E](s.e2)}"
  }
  private def applyP[E<:SymbolicExpr.All](expr: SymbolicExpr[E]): String = expr match {
    case SVal(v) =>
      val res = floatToFraction(v)
      if (res.contains('/')) s"($res)" else res
    case _:SArg | _:SVar | _:SFun[E] => apply(expr)
    case _ => "("+apply[E](expr)+")"
  }


  // NÃO PERCEBI PARA O QUE SERVE
  def pp[E<:SymbolicExpr.All](expr: SymbolicExpr[E]): String = expr match {
    case SVal(v) => if (v.round == v) v.toInt.toString else v.toString // f"$v%1.8f"
    case _:SArg  => "t"
    case s:SVar  => s.v
    //// Optimizations (prettifiers)
    case SMult(SVal(-1),e)=> s"-${ppP[E](e)}"
    case SMult(e,SVal(-1))=> s"-${ppP[E](e)}"
    case SMult(SVal(1),e)=> s"${pp[E](e)}"
    case SMult(e,SVal(1))=> s"${pp[E](e)}"
    case SMult(SVar(x),d@SVal(_))=> s"${pp[E](d)}$x"
    case SMult(SArg(),d@SVal(_))=> s"${pp[E](d)}t"
    case SMult(d@SVal(_),SVar(x))=> s"${pp[E](d)}$x"
    case SMult(d@SVal(_),SArg())=> s"${pp[E](d)}t"
//    case SMult(e1,e2@SAdd(_,_))=> s"${ppP[E](e1)}*${ppP[E](e2)}"
    case SAdd(SVal(0),e)=> s"${pp[E](e)}"
    case SAdd(e,SVal(0))=> s"${pp[E](e)}"
    case SAdd(e1,e2@SAdd(_,_)) => s"${ppP[E](e1)}+${pp[E](e2)}"
//    case SSub(e1,e2@SAdd(_,_)) => s"${ppP[E](e1)}-${ppP[E](e2)}"
//    case SMult(e1@SAdd(_,_),e2)=> s"${ppP[E](e1)}*${ppP[E](e2)}"
    case SAdd(e1@SAdd(_,_),e2)=> s"${pp[E](e1)}+${ppP[E](e2)}"
//    case SSub(e1@SAdd(_,_),e2)=> s"${ppP[E](e1)}-${ppP[E](e2)}"
    case SSub(SVal(0),e)=> s"-${ppP[E](e)}"
    case SSub(e,SVal(0))=> s"${pp[E](e)}"
    //// General cases
    case s:SFun[E]  => s"${s.f}(${s.args.map(pp[E]).mkString(",")})"
    case s:SDiv[E]  => s"${ppP[E](s.e1)}/${ppP[E](s.e2)}"
    case s:SRes[E]  => s"${ppP[E](s.e1)}%${ppP[E](s.e2)}"
    case s:SMult[E] => s"${ppP[E](s.e1)}*${ppP[E](s.e2)}"
    case s:SPow[E]  => s"${ppP[E](s.e1)}^${ppP[E](s.e2)}"
    case s:SAdd[E]  => s"${ppP[E](s.e1)}+${ppP[E](s.e2)}"
    case s:SSub[E]  => s"${ppP[E](s.e1)}-${ppP[E](s.e2)}"
  }
  private def ppP[E<:SymbolicExpr.All](expr: SymbolicExpr[E]): String = expr match {
    case _:SVal | _:SArg | _:SVar | _:SFun[E] |
         SMult(SArg(),SVal(_))  |  SMult(SVal(_),SArg())  |
         SMult(SVar(_),SVal(_))  |  SMult(SVal(_),SVar(_)) |
         SMult(SVal(-1),_) | SMult(_,SVal(-1))  => pp(expr)
    case _ => "("+pp[E](expr)+")"
  }


  def apply(sol:SySolution): String =
    sol.map(kv => s"${kv._1}(t) = ${apply(kv._2)}").mkString("</br>")

  def pp(sol:SySolution): String =
    sol.map(kv => s"${kv._1}(t) = ${pp(kv._2)}").mkString("</br>")

  def pp(sol:SySolution, v:Valuation): String = {
    pp(sol.view.mapValues(Eval.updInputFun(_,v)).toMap)
  }


  // NÃO PERCEBI O QUE ESTA FAZ
  def floatToFraction(v: Double): String = try {
    var den: BigInt = 1
    var num: BigDecimal = v
    while (num.toBigIntExact.isEmpty) { // && num < BigDecimal.apply("100000000000000000000000")) {
      num *= 10
      den *= 10
      //println(s"   - $num/$den")
    }
    //println(s"-- $v -> ${num.toBigInt.toString}/${den.toString}")
    if (den == 1) num.toBigInt.toString
    else s"${num.toBigInt.toString}/${den.toString}"
    } catch {
    case e: java.lang.NumberFormatException => "NaN"
  }

  // NÃO PERCEBI PARA O QUE SERVE
  def apply(r:RunTarget): String = r match {
    case Traj.Time(t) => apply(t)
    case Traj.Times(from,to,step) => s"${from.toString}:${step.toString}:${to.toString}" //ts.map(apply[SymbolicExpr.All]).mkString(", ")
    case Traj.Bound(n,timer) => s"forever ($n loop iterations, max $timer time)"
  }
}