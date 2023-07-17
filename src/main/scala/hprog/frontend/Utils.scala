package hprog.frontend

import hprog.ast.SymbolicExpr.{SyExpr, SyExprAll, SyExprVar}
import hprog.ast._
import Syntax._
import hprog.backend.Show
import hprog.frontend.CommonTypes.Valuation
import scala.math._
import scala.util.control._



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



// Function responsible for extracting the declarations
def extractAssigments(prog: Syntax): List[Assign] = prog match {
  /** If the parser output is a case class Atomic it is necessary to check whether it refers
   * to an assignment or to differential equations */
  case Atomic(as, deqs) => {
    /** If the output from the parser is an atomic relative to an assigment, then
     * just return the a list with this assignment
     */
    if (deqs.eqs == List()) {
      as
    }
    else {
      /** If the output from the parser is an atomic relative to differential equations,
       * then is returned a list with the case class Assign relative to a variable named ``Stop case".
       * Since variable names can never be separated by a space, putting this assignment
       * in the list of assigments to be returned will act as a marker for later retrieving only
       * the assignments referring to declarations!!! */
      var aux = List(Assign(Var("Stop case"), Value(-1)))
      aux
    }
  }

  /** If the parser output is a case class While, only the list of assignments preceding that loop need
   * to be extracted. */
  case While(pre, c, p) => {
    extractAssigments(pre)
  }

  /** If the output of the parser is a case class Seq (representing the sequence of programs)
   * it is necessary to extract the list of assignments of both sequenced programs */
  case Seq(p, q) => {
    (extractAssigments(p) ++ extractAssigments(q))
  }

  /** If the parser output is a case class different from the previous ones (such as an ITE) an
   * empty list is returned because there are no assignments preceding the respective instruction */
  case _ => List()
}








// New
// Function with de responsability to extract a list of lists of  differential equations
def extractDifEqs(prog:Syntax):List[List[DiffEq]] = prog match {
  case Atomic(as,de) => return List(de.eqs)
  case Seq(Atomic(as,de),q) => List(de.eqs)++extractDifEqs(q)
  case Seq(p,q) => extractDifEqs(p) ++ extractDifEqs(q)
  case While(pre,c,p) =>  extractDifEqs(pre) ++ extractDifEqs(p)
  case ITE(ifP,thenP,elseP) => extractDifEqs(thenP) ++ extractDifEqs(elseP)
}

// New
// Function with the responsibility to extract de variables of the differential equations
def extractVarsDifEqs(prog:Syntax):List[List[String]] = {
  var eqsdiff=extractDifEqs(prog)
  var listVars:List[List[String]]=List()

  for (lsteqDiff <- eqsdiff){
    var aux:List[String]=List()
    for (eqDiff <- lsteqDiff){

      if (extractTotalVarsLinearExp(eqDiff.e)==0) {
        //println("calc_doubles:",calc_doubles(eqDiff.e))
        if (calc_doubles(eqDiff.e)!=0) aux=aux ++ List((eqDiff.v).v)
        else aux=aux
      } else aux=aux ++ List((eqDiff.v).v)

    }
    listVars=listVars ++ List(aux)

  }

  return listVars
}

def extractVarsDifEqs(prog:Atomic):List[String] = {
  
  var listVars:List[String]=List()

  for (eqDiff <- prog.de.eqs){
     if (extractTotalVarsLinearExp(eqDiff.e)==0) {
        if (calc_doubles(eqDiff.e)!=0) listVars=listVars ++ List((eqDiff.v).v)
      } else listVars=listVars ++ List((eqDiff.v).v)
  }

  return listVars
}
//def extractAssigmentsyntax(prog:Assign):Map[String,NotLin]={}

//This function converts the Syntax coming from the Parser, into a Syntax where the parsing of the constant variables becomes the parsing of the linear expression that defines it
def updateSyntax(prog:Syntax,varsDcl:Map[String,NotLin],iteration:Int,varsDifEqs:List[List[String]],control:Int):(Syntax,Map[String,NotLin],Int)={
 prog match {
  case Atomic(as,de) => {
    var listAcum:List[Assign]=List()
   // var aux:(Assign,Map[String,NotLin])=updateSyntax(as(0),varsDcl,iteration,varsDifEqs,0)
    var aux1:Map[String,NotLin]=varsDcl
    //listAcum=listAcum ++ aux(0)

    for (a<-as){
       var aux= updateSyntax(a,aux1,iteration,varsDifEqs,0)
       listAcum=listAcum ::: aux._1 :: Nil
       aux1=aux._2
    }
    var auxDe=updateSyntax(de,aux1,iteration + 1,varsDifEqs,1)
    return (Atomic(listAcum,auxDe._1),auxDe._2,iteration+1)
  }
  case Seq(p,q) =>{
    var auxP=updateSyntax(p,varsDcl,iteration,varsDifEqs,0)
    var auxQ=updateSyntax(q,auxP._2,auxP._3,varsDifEqs,0)
   return (Seq(auxP._1,auxQ._1),auxQ._2,auxQ._3)
  }
  case While(pre,c,p) => {
    var auxPre=updateSyntax(pre,varsDcl,iteration,varsDifEqs,0)
    var auxP=updateSyntax(p,auxPre._2,auxPre._3,varsDifEqs,0)
    return (While(auxPre._1,c,auxP._1),auxP._2,auxP._3)
  
  }
  case ITE(ifP,thenP,elseP) =>  {
    var choice:Map[String,NotLin]=Map()
    var auxifp=updateSyntax(ifP,varsDcl,iteration,varsDifEqs,0)
    var bol:Boolean=Eval.apply(Map(),auxifp._1)
    var auxThenP=updateSyntax(thenP,varsDcl,iteration,varsDifEqs,0)
    var auxElseP=updateSyntax(elseP,varsDcl,auxThenP._3,varsDifEqs,0)

    if (bol) {
      choice=auxThenP._2
    } else {
      choice=auxElseP._2
    }
    
    return (ITE(ifP,auxThenP._1,auxElseP._1),choice,auxElseP._3)
  }

 }

}

def updateSyntax(prog:NotLin,varsDcl:Map[String,NotLin],iteration:Int,varsDifEqs:List[List[String]],control:Int):(NotLin,Map[String,NotLin],Int)={
 prog match {
  case Var(v)=> {
    if (control==1) {
        println("iteration:",iteration)
        println("varsDifEqs:",varsDifEqs)
        println("varsDifEqs(iteration-1).contains(v):",varsDifEqs(iteration-1).contains(v))
        if (varsDifEqs(iteration-1).contains(v)) {
            return (Var(v),varsDcl,iteration)
        } else {
          return (varsDcl(v),varsDcl,iteration)
        }
  } else {
    return (varsDcl(v),varsDcl,iteration)
  }
}
  case Value(v) => {
    return (Value(v),varsDcl,iteration)
  }
  case Add(x,y)=> {
    var auxX=updateSyntax(x,varsDcl,iteration,varsDifEqs,control)
    var auxY=updateSyntax(y,varsDcl,iteration,varsDifEqs,control)
    return (Add(auxX._1,auxY._1),varsDcl,iteration)
  }
  case Mult(x,y)=> {
    var auxX=updateSyntax(x,varsDcl,iteration,varsDifEqs,control)
    var auxY=updateSyntax(y,varsDcl,iteration,varsDifEqs,control)
    return (Mult(auxX._1,auxY._1),varsDcl,iteration)
  }
  case Div(x,y)=> {
    var auxX=updateSyntax(x,varsDcl,iteration,varsDifEqs,control)
    var auxY=updateSyntax(y,varsDcl,iteration,varsDifEqs,control)
    return (Div(auxX._1,auxY._1),varsDcl,iteration)
    }
  case Res(x,y)=> {
    var auxX=updateSyntax(x,varsDcl,iteration,varsDifEqs,control)
    var auxY=updateSyntax(y,varsDcl,iteration,varsDifEqs,control)
    return (Res(auxX._1,auxY._1),varsDcl,iteration)
  }
  /**
  case Pow(x,y)=> {
    var auxX=updateSyntax(x,varsDcl,iteration,varsDifEqs,control)
    var auxY=updateSyntax(y,varsDcl,iteration,varsDifEqs,control)
    return (Pow(auxX._1,auxY._1),varsDcl,iteration)
  }*/
  case Func(s,xs)=> {
    var listNotLin:List[NotLin]=List()
    for(x<-xs){
      var aux=updateSyntax(x,varsDcl,iteration,varsDifEqs,control)
      listNotLin=listNotLin ::: aux._1 :: Nil
    }
    return (Func(s,listNotLin),varsDcl,iteration)
  }
  }
}


def updateSyntax(prog:Assign,varsDcl:Map[String,NotLin],iteration:Int,varsDifEqs:List[List[String]],control:Int):(Assign,Map[String,NotLin],Int)={
 prog match {
  case Assign(v,e) => {
    //println("old:",varsDcl)
    var aux=updateSyntax(e,varsDcl,iteration,varsDifEqs,0)
    var newvarsDcl=aux._2+(v.v->aux._1)
    println("new:",newvarsDcl)
    return (Assign(v,aux._1),newvarsDcl,iteration)
  }
}
}

def updateSyntax(prog:DiffEq,varsDcl:Map[String,NotLin],iteration:Int,varsDifEqs:List[List[String]],control:Int):(DiffEq,Map[String,NotLin],Int)={
  prog match {
  case DiffEq(v,e) => {
    var aux=updateSyntax(e,varsDcl,iteration,varsDifEqs,1)
    return (DiffEq(v,aux._1),aux._2,iteration)
  }
}
}

def updateSyntax(prog:DiffEqs,varsDcl:Map[String,NotLin],iteration:Int,varsDifEqs:List[List[String]],control:Int):(DiffEqs,Map[String,NotLin],Int)={
 prog match {
  case DiffEqs(eqs,dur) => {
    var listDiffEq:List[DiffEq]=List()
    //var aux=updateSyntax(eqs(0),varsDcl,iteration,varsDifEqs,1)
    var aux1:Map[String,NotLin]=varsDcl
    //listDiffEq=listDiffEq ++ aux(0)

    for (e<-eqs){
       var aux= updateSyntax(e,aux1,iteration,varsDifEqs,1)
       listDiffEq=listDiffEq ::: aux._1 :: Nil
       aux1=aux._2
    }

    return (DiffEqs(listDiffEq,dur),aux1,iteration)
  }
}
}


def updateSyntax(prog:Cond,varsDcl:Map[String,NotLin],iteration:Int,varsDifEqs:List[List[String]],control:Int):(Cond,Map[String,NotLin],Int)={
 prog match {
  case BVal(b)=> {
   return (BVal(b),varsDcl,iteration)
  }
  case And(c1,c2)=> {
    var auxC1=updateSyntax(c1,varsDcl,iteration,varsDifEqs,0)
    var auxC2=updateSyntax(c2,auxC1._2,iteration,varsDifEqs,0)
    return (And(auxC1._1,auxC2._1),auxC2._2,iteration)
  }
  case Or(c1,c2)=> {
    var auxC1=updateSyntax(c1,varsDcl,iteration,varsDifEqs,0)
    var auxC2=updateSyntax(c2,auxC1._2,iteration,varsDifEqs,0)
    return (Or(auxC1._1,auxC2._1),auxC2._2,iteration)
  }
  case Not(c1)=> {
    var auxC1=updateSyntax(c1,varsDcl,iteration,varsDifEqs,0)
    return (Not(auxC1._1),auxC1._2,iteration)
  }
  case EQ(x1,x2)=> {
    var auxX1=updateSyntax(x1,varsDcl,iteration,varsDifEqs,0)
    var auxX2=updateSyntax(x2,auxX1._2,iteration,varsDifEqs,0)
    return (EQ(auxX1._1,auxX2._1),auxX2._2,iteration)
  }
  case GT(x1,x2)=> {
    var auxX1=updateSyntax(x1,varsDcl,iteration,varsDifEqs,0)
    var auxX2=updateSyntax(x2,auxX1._2,iteration,varsDifEqs,0)
    return (GT(auxX1._1,auxX2._1),auxX2._2,iteration)
  }
  case GE(x1,x2)=> {
    var auxX1=updateSyntax(x1,varsDcl,iteration,varsDifEqs,0)
    var auxX2=updateSyntax(x2,auxX1._2,iteration,varsDifEqs,0)
    return (GE(auxX1._1,auxX2._1),auxX2._2,iteration)
  }
  case LT(x1,x2)=> {
    var auxX1=updateSyntax(x1,varsDcl,iteration,varsDifEqs,0)
    var auxX2=updateSyntax(x2,auxX1._2,iteration,varsDifEqs,0)
    return (LT(auxX1._1,auxX2._1),auxX2._2,iteration)
  }
  case LE(x1,x2)=> {
    var auxX1=updateSyntax(x1,varsDcl,iteration,varsDifEqs,0)
    var auxX2=updateSyntax(x2,auxX1._2,iteration,varsDifEqs,0)
    return (LE(auxX1._1,auxX2._1),auxX2._2,iteration)
  }
}
}








 

 /*
 // LINEAR EXPRESSIONS ONLY
 // chamar sets em vez de listas
 // New
 // This function extract the number of variables in a linear expression of an Eq.Diff
 def extractVarsLinearExp(lin:Lin,listOfVars:List[String]):Int = lin match {

  case Value(value) =>  0
  
  case Var(v) => {
   if (listOfVars.contains(v))  1 
   else  0
  }
  case Add(l1,l2) => math.max(extractVarsLinearExp(l1,listOfVars),extractVarsLinearExp(l2,listOfVars))
  
  case Mult(l1,l2) => (extractVarsLinearExp(l1,listOfVars) + extractVarsLinearExp(l2,listOfVars))
  

 }

*/


def extract_VarsEqs(notlin:NotLin,listOfVars:List[String]):(List[NotLin],Set[String]) = notlin match {
  case Value(v) => (List(),Set())
  case Var(v) => if (listOfVars.contains(v))  (List(Var(v)),Set(v)) 
                else  (List(),Set())
  case Add(l1,l2) => (extract_VarsEqs(l1,listOfVars)._1++extract_VarsEqs(l2,listOfVars)._1,extract_VarsEqs(l1,listOfVars)._2++extract_VarsEqs(l2,listOfVars)._2)
  case Mult(l1,l2)=>{
   var aux1=getVars(l1)++getVars(l2)
   var aux:Set[String]=Set()
   for (i<-aux1) {
       if(listOfVars contains i) aux=aux+i
   }
    if (aux.nonEmpty) return(List(Mult(l1,l2)),aux)
    else return(List(),Set())
  }
  case Res(l1,l2) => {
   var aux1=getVars(l1)++getVars(l2)
   var aux:Set[String]=Set()
   for (i<-aux1) {
       if(listOfVars contains i) aux=aux+i
   }
    if (aux.nonEmpty) return(List(Res(l1,l2)),aux)
    else return(List(),Set())
  }
  case Div(l1,l2)=>{
   var aux1=getVars(l1)++getVars(l2)
   var aux:Set[String]=Set()
   for (i<-aux1) {
       if(listOfVars contains i) aux=aux+i
   }
    if (aux.nonEmpty) return(List(Div(l1,l2)),aux)
    else return(List(),Set())
  }
  case Func(s,list)=>{
    
    var aux1:Set[String]=Set()
    for (i<-list){
      aux1=aux1++getVars(i)
    }
    var aux:Set[String]=Set()
    for (i<-aux1) {
       if(listOfVars contains i) aux=aux+i
    }
    
    if (aux.nonEmpty) return(List(Func(s,list)),aux)
    else return(List(),Set())
  }

}


def varsEqs_calculation(list:List[NotLin],set:Set[String]):Double = {
  var point:Map[String,Double]=Map()
  var count=1
  var count2:Double=0
  for(i<-set) {
    point=point+(i->count)
    count=count+1
  }
  for (i<-list) {
    count2=count2+Eval.apply(point,i)
  }
  return count2
}


/**
// NON LINEAR EXPRESSIONS ONLY
 // chamar sets em vez de listas
 // New
 // This function extract the number of variables in a linear expression of an Eq.Diff
 def extractVarsLinearExp(notlin:NotLin,listOfVars:List[String]):Int =  {
  var aux1=extract_VarsEqs(notlin,listOfVars)
  println("aux1:",aux1)
  var aux2=varsEqs_calculation(aux1._1,aux1._2)
  println("aux2:",aux2)

  if(aux2!=0) return 10
  else return 0

 }

def extractVarsLinearExp(notlin:NotLin,listOfVars:List[String]):Int = notlin match {

  case Value(value) =>  0
  
  case Var(v) => {
   if (listOfVars.contains(v))  1 
   else  0
  }
  case Add(l1,l2) => math.max(extractVarsLinearExp(l1,listOfVars),extractVarsLinearExp(l2,listOfVars))
  
  case Mult(l1,l2) => {

   if((extractVarsLinearExp(l1,listOfVars) + extractVarsLinearExp(l2,listOfVars))>0) {
    var aux1=extract_VarsEqs(notlin,listOfVars)
    println("aux1:",aux1)
    var aux2=varsEqs_calculation(aux1._1,aux1._2)
    println("aux2:",aux2)

    if(aux2!=0) return 10
    else return 0

 }
 else 0
 
  }
  
  case Div(l1,l2) => {
     if((extractVarsLinearExp(l1,listOfVars) + 1000*extractVarsLinearExp(l2,listOfVars))>0) {
    var aux1=extract_VarsEqs(notlin,listOfVars)
    println("aux1:",aux1)
    var aux2=varsEqs_calculation(aux1._1,aux1._2)
    println("aux2:",aux2)

    if(aux2!=0) return 10
    else return 0

 }
 else 0 

}

  case Res(l1,l2) => {
     if((1000*extractVarsLinearExp(l1,listOfVars) + 1000*extractVarsLinearExp(l2,listOfVars))>0) {
    var aux1=extract_VarsEqs(notlin,listOfVars)
    println("aux1:",aux1)
    var aux2=varsEqs_calculation(aux1._1,aux1._2)
    println("aux2:",aux2)

    if(aux2!=0) return 10
    else return 0

 }
 else 0 

}

  case Func(s,list) => funcextract(s,list,listOfVars)

 }


//new
 def funcextract(s:String,list:List[NotLin],listOfVars:List[String]):Int = (s,list) match {
  case ("PI",List()) => 0
  case ("E",List()) => 0
  case ("max",List(n1,n2)) => math.max(extractVarsLinearExp(n1,listOfVars),extractVarsLinearExp(n2,listOfVars))
  case ("min",List(n1,n2)) => math.max(extractVarsLinearExp(n1,listOfVars),extractVarsLinearExp(n2,listOfVars))
  case ("pow",List(n1,n2)) =>  {
     if((1000*extractVarsLinearExp(n1,listOfVars) + 1000*extractVarsLinearExp(n2,listOfVars))>0) {
    var aux1=extract_VarsEqs(notlin,listOfVars)
    println("aux1:",aux1)
    var aux2=varsEqs_calculation(aux1._1,aux1._2)
    println("aux2:",aux2)

    if(aux2!=0) return 10
    else return 0

 }
 else 0 

}
  // Any variables found in the following functions make the expression non-linear 
  case ("exp",List(n)) =>  {
     if((1000*extractVarsLinearExp(n,listOfVars))>0) {
    var aux1=extract_VarsEqs(notlin,listOfVars)
    println("aux1:",aux1)
    var aux2=varsEqs_calculation(aux1._1,aux1._2)
    println("aux2:",aux2)

    if(aux2!=0) return 10
    else return 0

 }
 else 0 

}
  case ("sin",List(n)) => {
     if((1000*extractVarsLinearExp(n,listOfVars))>0) {
    var aux1=extract_VarsEqs(notlin,listOfVars)
    println("aux1:",aux1)
    var aux2=varsEqs_calculation(aux1._1,aux1._2)
    println("aux2:",aux2)

    if(aux2!=0) return 10
    else return 0

 }
 else 0 

}
  case ("cos",List(n)) => {
     if((1000*extractVarsLinearExp(n,listOfVars))>0) {
    var aux1=extract_VarsEqs(notlin,listOfVars)
    println("aux1:",aux1)
    var aux2=varsEqs_calculation(aux1._1,aux1._2)
    println("aux2:",aux2)

    if(aux2!=0) return 10
    else return 0

 }
 else 0 

}
  case ("tan",List(n)) => {
     if((1000*extractVarsLinearExp(n,listOfVars))>0) {
    var aux1=extract_VarsEqs(notlin,listOfVars)
    println("aux1:",aux1)
    var aux2=varsEqs_calculation(aux1._1,aux1._2)
    println("aux2:",aux2)

    if(aux2!=0) return 10
    else return 0

 }
 else 0 

}
  case ("arcsin",List(n)) => {
     if((1000*extractVarsLinearExp(n,listOfVars))>0) {
    var aux1=extract_VarsEqs(notlin,listOfVars)
    println("aux1:",aux1)
    var aux2=varsEqs_calculation(aux1._1,aux1._2)
    println("aux2:",aux2)

    if(aux2!=0) return 10
    else return 0

 }
 else 0 

}
  case ("arccos",List(n)) => {
     if((1000*extractVarsLinearExp(n,listOfVars))>0) {
    var aux1=extract_VarsEqs(notlin,listOfVars)
    println("aux1:",aux1)
    var aux2=varsEqs_calculation(aux1._1,aux1._2)
    println("aux2:",aux2)

    if(aux2!=0) return 10
    else return 0

 }
 else 0 

}
  case ("arctan",List(n)) => {
     if((1000*extractVarsLinearExp(n,listOfVars))>0) {
    var aux1=extract_VarsEqs(notlin,listOfVars)
    println("aux1:",aux1)
    var aux2=varsEqs_calculation(aux1._1,aux1._2)
    println("aux2:",aux2)

    if(aux2!=0) return 10
    else return 0

 }
 else 0 

}
  case ("sinh",List(n)) => {
     if((1000*extractVarsLinearExp(n,listOfVars))>0) {
    var aux1=extract_VarsEqs(notlin,listOfVars)
    println("aux1:",aux1)
    var aux2=varsEqs_calculation(aux1._1,aux1._2)
    println("aux2:",aux2)

    if(aux2!=0) return 10
    else return 0

 }
 else 0 

}
  case ("cosh",List(n)) => {
     if((1000*extractVarsLinearExp(n,listOfVars))>0) {
    var aux1=extract_VarsEqs(notlin,listOfVars)
    println("aux1:",aux1)
    var aux2=varsEqs_calculation(aux1._1,aux1._2)
    println("aux2:",aux2)

    if(aux2!=0) return 10
    else return 0

 }
 else 0 

}
  case ("tanh",List(n)) => {
     if((1000*extractVarsLinearExp(n,listOfVars))>0) {
    var aux1=extract_VarsEqs(notlin,listOfVars)
    println("aux1:",aux1)
    var aux2=varsEqs_calculation(aux1._1,aux1._2)
    println("aux2:",aux2)

    if(aux2!=0) return 10
    else return 0

 }
 else 0 

}
  case ("sqrt",List(n)) => {
     if((1000*extractVarsLinearExp(n,listOfVars))>0) {
    var aux1=extract_VarsEqs(notlin,listOfVars)
    println("aux1:",aux1)
    var aux2=varsEqs_calculation(aux1._1,aux1._2)
    println("aux2:",aux2)

    if(aux2!=0) return 10
    else return 0

 }
 else 0 

}
  case ("log",List(n)) => {
     if((1000*extractVarsLinearExp(n,listOfVars))>0) {
    var aux1=extract_VarsEqs(notlin,listOfVars)
    println("aux1:",aux1)
    var aux2=varsEqs_calculation(aux1._1,aux1._2)
    println("aux2:",aux2)

    if(aux2!=0) return 10
    else return 0

 }
 else 0 

}
  case ("log10",List(n)) => {
     if((1000*extractVarsLinearExp(n,listOfVars))>0) {
    var aux1=extract_VarsEqs(notlin,listOfVars)
    println("aux1:",aux1)
    var aux2=varsEqs_calculation(aux1._1,aux1._2)
    println("aux2:",aux2)

    if(aux2!=0) return 10
    else return 0

 }
 else 0 

}
  case (_,_) => throw new RuntimeException(s"Unknown function '${s}(${(list.map(Show.applyV).toList).mkString(",")})', or the number of arguments are incorrect")

  }
  */


def extractTotalVarsLinearExp(notlin:NotLin):Int = notlin match {

  case Value(value) =>  0
  
  case Var(v) => 1
  case Add(l1,l2) => math.max(extractTotalVarsLinearExp(l1),extractTotalVarsLinearExp(l2))
  
<<<<<<< HEAD
  case Mult(l1,l2) =>  if (extractTotalVarsLinearExp(l1)==0){
                        if (calc_doubles(l1)==0) 0
                        else extractTotalVarsLinearExp(l2)
                     } else if (extractTotalVarsLinearExp(l2)==0){
                         if (calc_doubles(l2)==0) 0
                         else extractTotalVarsLinearExp(l1)
                     } else (extractTotalVarsLinearExp(l1) + extractTotalVarsLinearExp(l2))
=======
  case Mult(l1,l2) =>  (extractTotalVarsLinearExp(l1) + extractTotalVarsLinearExp(l2))
>>>>>>> 44bafb4c6dddf0bd489867f824528c771bea5703
  case Div(l1,l2) => (extractTotalVarsLinearExp(l1) + 2*extractTotalVarsLinearExp(l2)) //Only linear in the dividend, in the divisor it is always non-linear 

  case Res(l1,l2) => (2*extractTotalVarsLinearExp(l1) + 2*extractTotalVarsLinearExp(l2)) // remainder never can be linear

  case Func(s,list) => funcTotalextract(s,list)

 }

 def funcTotalextract(s:String,list:List[NotLin]):Int = (s,list) match {
  case ("PI",List()) => 0
  case ("E",List()) => 0
  case ("max",List(n1,n2)) => math.max(extractTotalVarsLinearExp(n1),extractTotalVarsLinearExp(n2))
  case ("min",List(n1,n2)) => math.max(extractTotalVarsLinearExp(n1),extractTotalVarsLinearExp(n2))
  case ("pow",List(n1,n2)) => if (extractTotalVarsLinearExp(n2)==0) {
                                  if (calc_doubles(n2)==0) 0
<<<<<<< HEAD
                                  else if (calc_doubles(n2)==1) extractTotalVarsLinearExp(n1) 
                                  else 2*extractTotalVarsLinearExp(n1)
                              } else if (extractTotalVarsLinearExp(n1)==0){
                                  if (calc_doubles(n1)==1 ) 0
                                  else 2
                              } else return 2
=======
                                  else {
                                      if (calc_doubles(n2)==1) extractTotalVarsLinearExp(n1) 
                                      else 2*extractTotalVarsLinearExp(n1)
                                  }
                              } else 2
>>>>>>> 44bafb4c6dddf0bd489867f824528c771bea5703
  // Any variables found in the following functions make the expression non-linear 
  case ("exp",List(n)) => 2*extractTotalVarsLinearExp(n)
  case ("sin",List(n)) => 2*extractTotalVarsLinearExp(n)
  case ("cos",List(n)) => 2*extractTotalVarsLinearExp(n)
  case ("tan",List(n)) => 2*extractTotalVarsLinearExp(n)
  case ("arcsin",List(n)) => 2*extractTotalVarsLinearExp(n)
  case ("arccos",List(n)) => 2*extractTotalVarsLinearExp(n)
  case ("arctan",List(n)) => 2*extractTotalVarsLinearExp(n)
  case ("sinh",List(n)) => 2*extractTotalVarsLinearExp(n)
  case ("cosh",List(n)) => 2*extractTotalVarsLinearExp(n)
  case ("tanh",List(n)) => 2*extractTotalVarsLinearExp(n)
  case ("sqrt",List(n)) => 2*extractTotalVarsLinearExp(n)
  case ("log",List(n)) => 2*extractTotalVarsLinearExp(n)
  case ("log10",List(n)) => 2*extractTotalVarsLinearExp(n)
  case (_,_) => throw new RuntimeException(s"Unknown function '${s}(${(list.map(Show.applyV).toList).mkString(",")})', or the number of arguments are incorrect")

  }


// NON LINEAR EXPRESSIONS ONLY
 // chamar sets em vez de listas
 // New
 // This function extract the number of variables in a non linear expression of an Eq.Diff
 def extractVarsLinearExp(notlin:NotLin,listOfVars:List[String]):Int = notlin match {

  case Value(value) =>  0
  
  case Var(v) => {
   if (listOfVars.contains(v))  1 
   else  0
  }
  case Add(l1,l2) => math.max(extractVarsLinearExp(l1,listOfVars),extractVarsLinearExp(l2,listOfVars))
  
  case Mult(l1,l2) => if (extractTotalVarsLinearExp(l1)==0){
                        if (calc_doubles(l1)==0) 0
                        else extractVarsLinearExp(l2,listOfVars)
                     } else if (extractTotalVarsLinearExp(l2)==0){
                         if (calc_doubles(l2)==0) 0
                         else extractVarsLinearExp(l1,listOfVars)
                     } else (extractVarsLinearExp(l1,listOfVars) + extractVarsLinearExp(l2,listOfVars))
  case Div(l1,l2) => (extractVarsLinearExp(l1,listOfVars) + 2*extractVarsLinearExp(l2,listOfVars)) //Only linear in the dividend, in the divisor it is always non-linear 

  case Res(l1,l2) => (2*extractVarsLinearExp(l1,listOfVars) + 2*extractVarsLinearExp(l2,listOfVars)) // remainder never can be linear

  case Func(s,list) => funcextract(s,list,listOfVars)

 }


//new
 def funcextract(s:String,list:List[NotLin],listOfVars:List[String]):Int = (s,list) match {
  case ("PI",List()) => 0
  case ("E",List()) => 0
  case ("max",List(n1,n2)) => math.max(extractVarsLinearExp(n1,listOfVars),extractVarsLinearExp(n2,listOfVars))
  case ("min",List(n1,n2)) => math.max(extractVarsLinearExp(n1,listOfVars),extractVarsLinearExp(n2,listOfVars))
  case ("pow",List(n1,n2)) => if (extractTotalVarsLinearExp(n2)==0) {
                                  if (calc_doubles(n2)==0) 0
<<<<<<< HEAD
                                  else if (calc_doubles(n2)==1) extractVarsLinearExp(n1,listOfVars) 
                                  else 2*extractVarsLinearExp(n1,listOfVars)                                  
                              } else if (extractTotalVarsLinearExp(n1)==0){
                                  if (calc_doubles(n1)==1) 0
                                  else 2
                              } else return 2
                              
=======
                                  else {
                                      if (calc_doubles(n2)==1) extractVarsLinearExp(n1,listOfVars) 
                                      else 2*extractVarsLinearExp(n1,listOfVars)
                                  }
                              } else 2
>>>>>>> 44bafb4c6dddf0bd489867f824528c771bea5703
  // Any variables found in the following functions make the expression non-linear 
  case ("exp",List(n)) => 2*extractVarsLinearExp(n,listOfVars)
  case ("sin",List(n)) => 2*extractVarsLinearExp(n,listOfVars)
  case ("cos",List(n)) => 2*extractVarsLinearExp(n,listOfVars)
  case ("tan",List(n)) => 2*extractVarsLinearExp(n,listOfVars)
  case ("arcsin",List(n)) => 2*extractVarsLinearExp(n,listOfVars)
  case ("arccos",List(n)) => 2*extractVarsLinearExp(n,listOfVars)
  case ("arctan",List(n)) => 2*extractVarsLinearExp(n,listOfVars)
  case ("sinh",List(n)) => 2*extractVarsLinearExp(n,listOfVars)
  case ("cosh",List(n)) => 2*extractVarsLinearExp(n,listOfVars)
  case ("tanh",List(n)) => 2*extractVarsLinearExp(n,listOfVars)
  case ("sqrt",List(n)) => 2*extractVarsLinearExp(n,listOfVars)
  case ("log",List(n)) => 2*extractVarsLinearExp(n,listOfVars)
  case ("log10",List(n)) => 2*extractVarsLinearExp(n,listOfVars)
  case (_,_) => throw new RuntimeException(s"Unknown function '${s}(${(list.map(Show.applyV).toList).mkString(",")})', or the number of arguments are incorrect")

  }

def multOfPi(number: Double): Boolean = {
  val eps = 1e-8 // Define a small value for tolerance
  val res = abs(number % math.Pi) // Calculate the remainder
  // Check if the remainder is within the tolerance range
  return res < eps || abs(res - math.Pi) < eps
}

def multOfPiOn2(number: Double): Boolean = {
  val eps = 1e-8 // Define a small value for tolerance
  val res = abs((number+math.Pi/2) % math.Pi) // Calculate the remainder
<<<<<<< HEAD
=======
  println(res)
>>>>>>> 44bafb4c6dddf0bd489867f824528c771bea5703
  // Check if the remainder is within the tolerance range
  return res < eps || abs(res - math.Pi) < eps
}
  def calc_doubles(notlin:NotLin):Double = notlin match {
    case Var(v) => 1
    case Value(v) => v
    case Add(l1, l2) => calc_doubles(l1) + calc_doubles(l2)
    case Mult(l1,l2)  => calc_doubles(l1)  * calc_doubles(l2)
    case Div(l1,l2)  =>calc_doubles(l1) / calc_doubles(l2)             
    case Res(l1,l2)  => calc_doubles(l1) % calc_doubles(l2)
    case Func(s,list) => (s,list) match {
      case ("PI",Nil) => math.Pi
      case ("E",Nil) => math.E
      case ("max",v1::v2::Nil) => math.max(calc_doubles(v1), calc_doubles(v2))
      case ("min",v1::v2::Nil) => math.min(calc_doubles(v1), calc_doubles(v2))
      case ("pow",v1::v2::Nil) => pow(calc_doubles(v1),calc_doubles(v2))
      case ("exp",v::Nil) => math.exp(calc_doubles(v))
      case ("sin",v::Nil) => {
        if (multOfPi(calc_doubles(v))) 0
        else math.sin(calc_doubles(v))
      }
      case ("cos",v::Nil) =>{
        if (multOfPiOn2(calc_doubles(v))) 0
        else math.cos(calc_doubles(v))
      }
      case ("tan",v::Nil) => {
        if (multOfPi(calc_doubles(v))) 0
        else math.tan(calc_doubles(v))
      }
      case ("arcsin",v::Nil) => math.asin(calc_doubles(v))
      case ("arccos",v::Nil) => math.acos(calc_doubles(v))
      case ("arctan",v::Nil) => math.atan(calc_doubles(v))
      case ("sinh",v::Nil) => math.sinh(calc_doubles(v))
      case ("cosh",v::Nil) => math.cosh(calc_doubles(v))
      case ("tanh",v::Nil) => math.tanh(calc_doubles(v))
      case ("sqrt",v::Nil) => math.sqrt(calc_doubles(v))
      case ("log",v::Nil) => math.log(calc_doubles(v))
      case ("log10",v::Nil) => math.log10(calc_doubles(v))
      case (_,_) => throw new RuntimeException(s"Unknown function '${s}(${(list.map(Show.applyV).toList).mkString(",")})', or the number of arguments are incorrect")
     }
    }

//New
// This function verify if the linear expressions of the Eqs.Diffs are linears
<<<<<<< HEAD
 def verifyLinearityEqsDiff(prog:Syntax):Option[DiffEq] =  {
=======
 def verifyLinearityEqsDiff(prog:Syntax):Option[List[DiffEq]] =  {
>>>>>>> 44bafb4c6dddf0bd489867f824528c771bea5703
   var diffeqs=extractDifEqs(prog) //List of List of Diff.eqs
   var varsDifEqs=extractVarsDifEqs(prog) // extract de variables of the differential equations
   //println("eqs.vars:",varsDifEqs)
   //var varsDifEqs=getFstDeclVarsTHEN(prog) // set of declared variables NEWWW
   var iteration=0
   var aux=0

   for (lsteqDiff <- diffeqs){
    var aux=0
    for (eqDiff <- lsteqDiff){
     aux=extractVarsLinearExp(eqDiff.e,varsDifEqs(iteration)) // extract the number of variables in a linear expressions 
     //println("aux:"+aux)
<<<<<<< HEAD
     if (aux > 1 ) return Some(eqDiff)
=======
     if (aux > 1 ) return Some(lsteqDiff)
>>>>>>> 44bafb4c6dddf0bd489867f824528c771bea5703
     }
     iteration=iteration + 1
  }
  return None   
 }
<<<<<<< HEAD



def vars_in_min_max(nl:NotLin):Double= nl match {
  case Var(v) => 0
  case Value(v) => 0
  case Add(l1, l2) => vars_in_min_max(l1) + vars_in_min_max(l2)
  case Mult(l1,l2)  =>if (extractTotalVarsLinearExp(l1)==0){
                        if (calc_doubles(l1)==0) 0
                        else vars_in_min_max(l2)
                     } else if (extractTotalVarsLinearExp(l2)==0){
                         if (calc_doubles(l2)==0) 0
                         else vars_in_min_max(l1)
                     } else vars_in_min_max(l1)  + vars_in_min_max(l2)
  case Div(l1,l2)  =>vars_in_min_max(l1)  + vars_in_min_max(l2)            
  case Res(l1,l2)  => vars_in_min_max(l1)  + vars_in_min_max(l2)
  case Func(s,list) => (s,list) match {
    case ("PI",Nil) => 0 
    case ("E",Nil) => 0
    case ("max",v1::v2::Nil) => extractTotalVarsLinearExp(v1)+extractTotalVarsLinearExp(v2)
    case ("min",v1::v2::Nil) => extractTotalVarsLinearExp(v1)+extractTotalVarsLinearExp(v2)
    case ("pow",v1::v2::Nil) => if (extractTotalVarsLinearExp(v2)==0) {
                                  if (calc_doubles(v2)==0) 0
                                  else vars_in_min_max(v1)                                 
                              } else if (extractTotalVarsLinearExp(v1)==0){
                                  if (calc_doubles(v1)==1) 0
                                  else vars_in_min_max(v2)
                              } else return vars_in_min_max(v1)  + vars_in_min_max(v2)
    case ("exp",v::Nil) => vars_in_min_max(v) 
    case ("sin",v::Nil) => vars_in_min_max(v) 
    case ("cos",v::Nil) => vars_in_min_max(v)
    case ("tan",v::Nil) => vars_in_min_max(v)
    case ("arcsin",v::Nil) => vars_in_min_max(v)
    case ("arccos",v::Nil) => vars_in_min_max(v)
    case ("arctan",v::Nil) => vars_in_min_max(v)
    case ("sinh",v::Nil) => vars_in_min_max(v)
    case ("cosh",v::Nil) => vars_in_min_max(v)
    case ("tanh",v::Nil) => vars_in_min_max(v)
    case ("sqrt",v::Nil) => vars_in_min_max(v)
    case ("log",v::Nil) => vars_in_min_max(v)
    case ("log10",v::Nil) => vars_in_min_max(v)
    case (_,_) => throw new RuntimeException(s"Unknown function '${s}(${(list.map(Show.applyV).toList).mkString(",")})', or the number of arguments are incorrect")
        
}
}


def verify_min_max(at:Atomic):Option[DiffEq]= {
  var diffeqs=at.de.eqs
  var aux:Double=1
  for (diffeq <- diffeqs){
   aux=vars_in_min_max(diffeq.e)
   if (aux>0) return Some(diffeq)
  }
  return None
}

    
=======
>>>>>>> 44bafb4c6dddf0bd489867f824528c771bea5703

////// New /////// 
//verify if the free varibles had already been declarated before being used.
  def assigmentsVerify(prog:Syntax): Set[String] = prog match {
    
    case Seq(p,q) => {
      var res=extractAssigments(p) ++ extractAssigments(q)
      var indice=res.indexOf(Assign(Var("Stop case"),Value(-1)))
      var as=res
      if (indice>=0) {
        as=as.take(indice)
      }
      /*
      var stop:Boolean=false
      //var i:Assign=Assign(Var("i"),Value(-1))
      var res2:List[Assign]=List()

      for (i <- 0 until res.length) {
        if (stop==false){
        if ((res(i).v.v) != "Stop case") res2=res2++List(res(i))
        else stop=true
      }
      }
      var as=res2
      */

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
    case While(pre,c,p) => assigmentsVerify(pre)
    case _ => Set()  
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

 
 def verifyUnsupFuncSyntax(prog:Syntax):List[String]=prog match {
    case Atomic(as,difeqs) => {
      verifyUnsupFuncLAssign(as)++verifyUnsupFuncDiffEqs(difeqs)
    }
    case While(pre,cond,p) => {
      verifyUnsupFuncSyntax(pre)++verifyUnsupFuncLoopGuard(cond)++verifyUnsupFuncSyntax(p)
    }
    case ITE(cond,t,e) => {
      verifyUnsupFuncCond(cond)++verifyUnsupFuncSyntax(t)++verifyUnsupFuncSyntax(e)
    
    }
    case Seq(p,q) => {
      verifyUnsupFuncSyntax(p)++verifyUnsupFuncSyntax(q)
    
    }
  }
  

 def verifyUnsupFuncLAssign(prog:List[Assign]):List[String]={
  var aux:List[String]=List()
  for (i<-prog){
   aux=aux ++ verifyUnsupFuncNotLin(i.e) 
  }
  return aux
}
   
 def verifyUnsupFuncDiffEqs(prog:DiffEqs):List[String]={
  var aux:List[String]=List()
  for (i<-prog.eqs){
   aux=aux ++ verifyUnsupFuncNotLin(i.e) 
  }
  aux=aux ++ verifyUnsupFuncDur(prog.dur)
  return aux
}

 def verifyUnsupFuncLoopGuard(prog:LoopGuard):List[String]= prog match {
  case Counter(i)=>List()
  case Guard(c)=>verifyUnsupFuncCond(c)
 }

 def verifyUnsupFuncCond(prog:Cond):List[String]= prog match {
  case BVal(b) => List()     
  case And(c1,c2) => verifyUnsupFuncCond(c1) ++ verifyUnsupFuncCond(c2)
  case Or(c1,c2) => verifyUnsupFuncCond(c1) ++ verifyUnsupFuncCond(c2)
  case Not(c)   => verifyUnsupFuncCond(c)       
  case EQ(l1,l2) => verifyUnsupFuncNotLin(l1) ++ verifyUnsupFuncNotLin(l2)
  case GT(l1,l2) => verifyUnsupFuncNotLin(l1) ++ verifyUnsupFuncNotLin(l2)   
  case LT(l1,l2) => verifyUnsupFuncNotLin(l1) ++ verifyUnsupFuncNotLin(l2)   
  case GE(l1,l2) => verifyUnsupFuncNotLin(l1) ++ verifyUnsupFuncNotLin(l2)   
  case LE(l1,l2) => verifyUnsupFuncNotLin(l1) ++ verifyUnsupFuncNotLin(l2)
 }

def verifyUnsupFuncNotLin(prog:NotLin):List[String]= prog match {
  case Var(v) => List()      
  case Value(v) => List() 
  case Add(l1,l2)  => verifyUnsupFuncNotLin(l1)++verifyUnsupFuncNotLin(l2) 
  case Mult(l1,l2) => verifyUnsupFuncNotLin(l1)++verifyUnsupFuncNotLin(l2)
  case Div(l1,l2) => verifyUnsupFuncNotLin(l1)++verifyUnsupFuncNotLin(l2)
  case Res(l1,l2) => verifyUnsupFuncNotLin(l1)++verifyUnsupFuncNotLin(l2)
  case Func(s, arg) => (s,arg) match {
      case ("PI",Nil) => List()
      case ("E",Nil) => List()
      case ("max",v1::v2::Nil) => verifyUnsupFuncNotLin(v1)++verifyUnsupFuncNotLin(v2)
      case ("min",v1::v2::Nil) => verifyUnsupFuncNotLin(v1)++verifyUnsupFuncNotLin(v2)
      case ("pow",v1::v2::Nil) => verifyUnsupFuncNotLin(v1)++verifyUnsupFuncNotLin(v2)
      case ("exp",v::Nil) => verifyUnsupFuncNotLin(v)
      case ("sin",v::Nil) => verifyUnsupFuncNotLin(v)
      case ("cos",v::Nil) => verifyUnsupFuncNotLin(v)
      case ("tan",v::Nil) => verifyUnsupFuncNotLin(v)
      case ("arcsin",v::Nil) => verifyUnsupFuncNotLin(v)
      case ("arccos",v::Nil) => verifyUnsupFuncNotLin(v)
      case ("arctan",v::Nil) => verifyUnsupFuncNotLin(v)
      case ("sinh",v::Nil) => verifyUnsupFuncNotLin(v)
      case ("cosh",v::Nil) => verifyUnsupFuncNotLin(v)
      case ("tanh",v::Nil) => verifyUnsupFuncNotLin(v)
      case ("sqrt",v::Nil) => verifyUnsupFuncNotLin(v)
      case ("log",v::Nil) => verifyUnsupFuncNotLin(v)
      case ("log10",v::Nil) => verifyUnsupFuncNotLin(v)
      case (_,_) => List(s"${s}(${(arg.map(Show.apply)).mkString(",")})")
      }   
    
    
  }


def verifyUnsupFuncDur(prog:Dur):List[String]= prog match {
  case For(nl) => verifyUnsupFuncNotLin(nl)
  case Until(c,eps,jump)=> verifyUnsupFuncCond(c)
  case Forever => List()
  }



// Verify if exists free variables  already been declarated before being used, and also if they are used variables that are not declareted
// Verify if the linear expressions of the Eq.Diffs are linears
  def isClosed(prog:Syntax): Either[String,Unit] = {
    //println("Program:",prog)

    val declVar=getFstDeclVars(prog)
    //println("declVar:",declVar)
    val usedVars = getUsedVars(prog)
    //println("usedVars:", usedVars)

   /**
    val declVarTHEN = getFstDeclVars(prog) //make a set with the firsts declareted variables (THEN)
    println("declVarThen:",declVarTHEN) 
    val declVarELSE = getFstDeclVars(prog) //make a set with the firsts declareted variables (ELSE)
    println("declVarElse:",declVarELSE)
    val usedVarsTHEN = getUsedVarsTHEN(prog)   //make a set with the firsts used variables
    println("usedVarsThen:",usedVarsTHEN)
    val usedVarsELSE = getUsedVarsELSE(prog)  //make a set with the firsts used variables
    println("usedVarsElse:",usedVarsELSE)
    */
    val asVerify=assigmentsVerify(prog) //make a set of free variables that not had been declareted before of ther invocation.
    //println("asVerify:",asVerify)





    //val unsupportedFunc:List[String]= verifyUnsupFuncSyntax(prog)
    //println("unsupportedFunc:",unsupportedFunc)
    
    //val varsEqDiffVerify=verifyLinearityEqsDiff(prog)

    
    //println("linear?:"+varsEqDiffVerify)


    if (asVerify.nonEmpty) //Verify if exist free variables that not had been declareted before, if exist i print it.
      Left(s"Initial declaration has free variables that were not declared: ${asVerify.mkString(", ")}")
    else if (!usedVars.forall(declVar))
      Left(s"Variable(s) not declared: ${((usedVars-- declVar)).mkString(", ")}")
   // else if (!usedVarsELSE.forall(declVarELSE))
     //  Left(s"Variable(s) not declared: ${((usedVarsTHEN -- declVarTHEN)++(usedVarsELSE-- declVarELSE)).mkString(", ")}")
   // else if (unsupportedFunc.nonEmpty){
     // return Left(s"Unknown function/s '${unsupportedFunc.mkString(",")}' or the number of arguments are incorrect")    
    //}
    //else if (varsEqDiffVerify.nonEmpty)
    //  Left(s"There are differential equations that are not linear or the semantic analyser suspects that they are non-linear (try simplifying the differential equations): ${varsEqDiffVerify.get.map(Show.apply).mkString(", ")}")
    else 
      Right(())
  }















////////////////////////////////////////////////////////////////////////////////////////////////////////

  // take a list of differential equations and take the variables with the tilde on top! (ex: p'=1+x returns set(p))
  def getDefVars(eqs: List[DiffEq]): Set[String] =
    eqs.map(_.v.v).toSet

////////////////////////////////////////////////////////////////////////////////////////////////////////






  //New
  // The function getUsedVars is defined for List[DiffEq],Syntax,DiffEqs and Dur, returning the variables used there
  def getUsedVars(eqs: List[DiffEq]): Set[String] =
    eqs.flatMap(eq => getVars(eq.e)+eq.v.v).toSet  // New

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
    case For(nl) => getVars(nl) //New
    case _ => Set()
  }



///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


/**
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


*/





////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


// New
  // Take a program and get the first declared variables (from the first atomics)
  //@scala.annotation.tailrec
  def getFstDeclVars(prog:Syntax): Set[String] = prog match {
    
    case Seq(p,q) => {
      var res=extractAssigments(Seq(p,q))
      //println("res:",res)
      var indice=res.indexOf(Assign(Var("Stop case"),Value(-1)))
      var as=res
      if (indice>=0) {
        as=as.take(indice)
      }
      var asSet=as.map(_.v.v).toSet
      
      return asSet
      }
    case Atomic(a, _)     => a.map(_.v.v).toSet // creates an set of the first variables declared
    case While(pre, _, _) => getFstDeclVars(pre)
    case _ => Set() }

/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


/**
//New
  def getFstDeclVarsELSE(prog:Syntax): Set[String] = prog match {
    case Atomic(a, _)     => a.map(_.v.v).toSet // creates an set of the first variables declared
   
    case Seq(p,q) => {
      var res=extractAssigments(p) ++ extractAssigments(q)
      var indice=res.indexOf(Assign(Var("Stop case"),Value(-1)))
      var as=res
      if (indice>=0) {
        as=as.take(indice)
      }
      /**
      var stop:Boolean=false
      //var i:Assign=Assign(Var("i"),Value(-1))
      var res2:List[Assign]=List()

      for (i <- 0 until res.length) {
        if (stop==false){
        if ((res(i).v.v) != "Stop case") res2=res2++List(res(i))
        else stop=true
      }
      }
      var as=res2
      */
      var asSet=as.map(_.v.v).toSet
      return asSet
      }
    //case ITE(_, _, elseP) =>getFstDeclVarsELSE(elseP) 
    case While(pre, _, _) => getFstDeclVarsELSE(pre)
    case _ => Set() 
  }

*/







/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// create sets with the variables used there
  def getVars(guard: LoopGuard): Set[String] = guard match {
    case Counter(_) => Set()
    case Guard(c) => getVars(c)
  }


/*
  //new
  def getVars(lin: Lin): Set[String] = lin match {
    case Var(v) => Set(v)
    case Value(_) => Set()
    case Add(l1, l2) => getVars(l1) ++ getVars(l2)
    case Mult(l1, l2) => getVars(l1) ++ getVars(l2)
  }

*/

    // new
  def getVars(notlin: NotLin): Set[String] = notlin match {
    case Var(v) => Set(v)
    case Value(_) => Set()
    case Add(l1, l2) => getVars(l1) ++ getVars(l2)
    case Mult(l1, l2) => getVars(l1) ++ getVars(l2)
    case Div(l1,l2) => getVars(l1) ++ getVars(l2)
    case Res(l1,l2) => getVars(l1) ++ getVars(l2)  
    //case Pow(l1,l2) => getVars(l1) ++ getVars(l2)
    case Func(s,list) => getVarsAux(list)
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

/*
  // new
  def getVarsList(lin: Lin): List[String] = lin match {
    case Var(v) => List(v)
    case Value(_) => List()
    case Add(l1, l2) => getVarsList(l1) ++ getVarsList(l2)
    case Mult(l1, l2) => getVarsList(l1) ++ getVarsList(l2)
  }
  */

    // new
  def getVarsList(notlin: NotLin): List[String] = notlin match {
    case Var(v) => List(v)
    case Value(_) => List()
    case Add(l1, l2) => getVarsList(l1) ++ getVarsList(l2)
    case Mult(l1, l2) => getVarsList(l1) ++ getVarsList(l2)
    case Div(l1,l2) => getVarsList(l1) ++ getVarsList(l2)
    case Res(l1,l2) => getVarsList(l1) ++ getVarsList(l2) 
    //case Pow(l1,l2) => getVarsList(l1) ++ getVarsList(l2)
    case Func(s,list) => getVarsListAux(list)
    
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
    case SFun("log10",args:List[SyExprVar]) =>SDiv(exprVarToExpr(SFun("log",args),prev),exprVarToExpr(SFun("log",List(SVal(10))),prev))
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

/** Fixes conventions produced by SageMath */
  def fixVars(e:SyExprAll): SyExprAll = {
    val res = e match {
      case SVal(_) => e
      //case SVar("e") => SFun("E", Nil)
      //case SVar("pi") => SFun("PI", Nil)
      case SVar(_) => e
      case SArg() => e
      case SFun("_e", List(SVal(0))) => SVar("_e")
      case SFun("_pi", List(SVal(0))) => SVar("_pi")
      case SFun(f, List(SVal(0)))
        if (f != "PI" && f != "E" && f != "sin" && f != "cos" && f != "tan" && f != "exp" && f != "arcsin" && f != "arccos" && f != "arctan" && f != "sinh" && f != "cosh" && f != "tanh" && f != "sqrt" && f != "log" && f != "log10")=> SVar(f)
      //case SFun("e", args) => SFun("E", args.map(fixVars))
      //case SFun("pi",args)=> SFun("PI", args.map(fixVars))
      case SFun("log10",args)=>SDiv(fixVars(SFun("log",args)),fixVars(SFun("log",List(SVal(10)))))
      case SFun(f, args) => SFun(f, args.map(fixVars))
      case SDiv(e1, e2) => SDiv(fixVars(e1), fixVars(e2))
      case SRes(e1, e2) => SRes(fixVars(e1), fixVars(e2))
      case SMult(e1, e2) => SMult(fixVars(e1), fixVars(e2))
      case SPow(e1, e2) => SPow(fixVars(e1), fixVars(e2))
      case SAdd(e1, e2) => SAdd(fixVars(e1), fixVars(e2))
      case SSub(e1, e2) => SSub(fixVars(e1), fixVars(e2))
    }
    println(s"-------------Fixing ${(e)} into ${(res)}")
    res
  }

/**
  /** Fixes conventions produced by SageMath */
  def fixVars(e:SyExprAll): SyExprAll = {
    val res = e match {
      case SVal(_) => e
      case SVar("e") => SFun("E", Nil)
      case SVar("pi") => SFun("PI", Nil)
      case SVar(_) => e
      case SArg() => e
      case SFun("_e", List(SVal(0))) => SVar("_e")
      case SFun("_pi", List(SVal(0))) => SVar("_pi")
      case SFun(f, List(SVal(0)))
        if (f != "pi" && f != "e" && f != "sin" && f != "cos" && f != "tan" && f != "exp" && f != "arcsin" && f != "arccos" && f != "arctan" && f != "sinh" && f != "cosh" && f != "tanh" && f != "sqrt" && f != "log" && f != "log10")=> SVar(f)
      case SFun("e", args) => SFun("E", args.map(fixVars))
      case SFun("pi",args)=> SFun("PI", args.map(fixVars))
      case SFun("log10",args)=>SDiv(fixVars(SFun("log",args)),fixVars(SFun("log",List(SVal(10)))))
      case SFun(f, args) => SFun(f, args.map(fixVars))
      case SDiv(e1, e2) => SDiv(fixVars(e1), fixVars(e2))
      case SRes(e1, e2) => SRes(fixVars(e1), fixVars(e2))
      case SMult(e1, e2) => SMult(fixVars(e1), fixVars(e2))
      case SPow(e1, e2) => SPow(fixVars(e1), fixVars(e2))
      case SAdd(e1, e2) => SAdd(fixVars(e1), fixVars(e2))
      case SSub(e1, e2) => SSub(fixVars(e1), fixVars(e2))
    }
    println(s"-------------Fixing ${(e)} into ${(res)}")
    res
  }
*/

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
    case GT(Var(v), Value(d)) => Some(Map(v->Hole(Inf,Open(d))))
    case GE(Var(v), Value(d)) => Some(Map(v->Hole(Inf,Close(d))))
    case LT(Var(v), Value(d)) => Some(Map(v->Hole(Open(d),Inf)))
    case LE(Var(v), Value(d)) => Some(Map(v->Hole(Close(d),Inf)))
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
