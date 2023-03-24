package hprog.frontend

import hprog.ast.SymbolicExpr.{SyExpr, SyExprAll, SyExprVar}
import hprog.ast._
import Syntax._
import hprog.backend.Show
import hprog.frontend.CommonTypes.Valuation
import scala.math._
import scala.util.control.Breaks._



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



// New
def extractAssigments(prog:Syntax):List[Assign] = prog match {
  
  case Atomic(as,_) => {
  	return as
  }
  case Seq(Atomic(as,_),q) => {
  	var ac=as++extractAssigments(q)
  	return ac
  }
  case Seq(While(pre,c,p),q) => {
    var ac=extractAssigments(pre)
    return ac
  }
  //case Seq(p,q) =>{
  
  // return extractAssigments(p) ++ extractAssigments(q)
  //}
  case While(pre,c,p) => {
  	return extractAssigments(pre)
  
  }
  case Seq(p,q) =>{
  
   return extractAssigments(p) ++ extractAssigments(q)
  }
  case _ =>  List()


}


// New
// Function with de responsability to extract a list of lists of  differential equations
def extractDifEqs(prog:Syntax):List[List[DiffEq]] = prog match {
  
  case Atomic(as,de) => {
    return List(de.eqs)
  }
  case Seq(Atomic(as,de),q) => {
    var ac=List(de.eqs)++extractDifEqs(q)
    return ac
  }
  case Seq(p,q) =>{
  
   return extractDifEqs(p) ++ extractDifEqs(q)
  }
  case While(pre,c,p) => {
    return extractDifEqs(pre) ++ extractDifEqs(p)
  
  }
  case ITE(ifP,thenP,elseP) =>  {
    return extractDifEqs(thenP) ++ extractDifEqs(elseP)
  }

}

// New
// Function with the responsibility to extract de variables of the differential equations
def extractVarsDifEqs(prog:Syntax):List[List[String]] = {
  var eqsdiff=extractDifEqs(prog)
  var listVars:List[List[String]]=List()

  for (lsteqDiff <- eqsdiff){
    var aux:List[String]=List()
    for (eqDiff <- lsteqDiff){
      aux=aux ++ List((eqDiff.v).v)
      //println("eqDiff:"+(eqDiff.v).v)
      //println("aux:"+aux)
    }
    listVars=listVars ++ List(aux)

  }

  return listVars
}

def extractVarsDifEqs(prog:Atomic):List[String] = {
  
  var listVars:List[String]=List()

  for (eqDiff <- prog.de.eqs){
    //var aux:List[String]=List()
      var aux:String=eqDiff.v.v
      listVars=listVars ++ List(aux)
      //println("eqDiff:"+(eqDiff.v).v)
      //println("aux:"+aux)   

  }

  return listVars
}
//def extractDeclarationSyntax(prog:Assign):Map[String,NotLin]={}

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
  case VarNotLin(v)=> {
    if (control==1) {
        println("iteration:",iteration)
        println("varsDifEqs:",varsDifEqs)
        println("varsDifEqs(iteration-1).contains(v):",varsDifEqs(iteration-1).contains(v))
        if (varsDifEqs(iteration-1).contains(v)) {
            return (VarNotLin(v),varsDcl,iteration)
        } else {
          return (varsDcl(v),varsDcl,iteration)
        }
  } else {
    return (varsDcl(v),varsDcl,iteration)
  }
}
  case ValueNotLin(v) => {
    return (ValueNotLin(v),varsDcl,iteration)
  }
  case AddNotLin(x,y)=> {
    var auxX=updateSyntax(x,varsDcl,iteration,varsDifEqs,control)
    var auxY=updateSyntax(y,varsDcl,iteration,varsDifEqs,control)
    return (AddNotLin(auxX._1,auxY._1),varsDcl,iteration)
  }
  case MultNotLin(x,y)=> {
    var auxX=updateSyntax(x,varsDcl,iteration,varsDifEqs,control)
    var auxY=updateSyntax(y,varsDcl,iteration,varsDifEqs,control)
    return (MultNotLin(auxX._1,auxY._1),varsDcl,iteration)
  }
  case DivNotLin(x,y)=> {
    var auxX=updateSyntax(x,varsDcl,iteration,varsDifEqs,control)
    var auxY=updateSyntax(y,varsDcl,iteration,varsDifEqs,control)
    return (DivNotLin(auxX._1,auxY._1),varsDcl,iteration)
    }
  case ResNotLin(x,y)=> {
    var auxX=updateSyntax(x,varsDcl,iteration,varsDifEqs,control)
    var auxY=updateSyntax(y,varsDcl,iteration,varsDifEqs,control)
    return (ResNotLin(auxX._1,auxY._1),varsDcl,iteration)
  }
  case PowNotLin(x,y)=> {
    var auxX=updateSyntax(x,varsDcl,iteration,varsDifEqs,control)
    var auxY=updateSyntax(y,varsDcl,iteration,varsDifEqs,control)
    return (PowNotLin(auxX._1,auxY._1),varsDcl,iteration)
  }
  case FuncNotLin(s,xs)=> {
    var listNotLin:List[NotLin]=List()
    for(x<-xs){
      var aux=updateSyntax(x,varsDcl,iteration,varsDifEqs,control)
      listNotLin=listNotLin ::: aux._1 :: Nil
    }
    return (FuncNotLin(s,listNotLin),varsDcl,iteration)
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







//New
// This function verify if the linear expressions of the Eqs.Diffs are linears
 def verifyLinearityEqsDiff(prog:Syntax):Option[List[DiffEq]] =  {
   var diffeqs=extractDifEqs(prog) //List of List of Diff.eqs
   var varsDifEqs=extractVarsDifEqs(prog) // extract de variables of the differential equations
   //var varsDifEqs=getFstDeclVarsTHEN(prog) // set of declared variables NEWWW
   var iteration=0
   var aux=0

   for (lsteqDiff <- diffeqs){
    var aux=0
    for (eqDiff <- lsteqDiff){
     aux=extractVarsLinearExp(eqDiff.e,varsDifEqs(iteration)) // extract the number of variables in a linear expressions 
     //println("aux:"+aux)
     if (aux > 1 ) return Some(lsteqDiff)
     }
     iteration=iteration + 1
  }
  return None   
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

// NON LINEAR EXPRESSIONS ONLY
 // chamar sets em vez de listas
 // New
 // This function extract the number of variables in a linear expression of an Eq.Diff
 def extractVarsLinearExp(notlin:NotLin,listOfVars:List[String]):Int = notlin match {

  case ValueNotLin(value) =>  0
  
  case VarNotLin(v) => {
   if (listOfVars.contains(v))  1 
   else  0
  }
  case AddNotLin(l1,l2) => math.max(extractVarsLinearExp(l1,listOfVars),extractVarsLinearExp(l2,listOfVars))
  
  case MultNotLin(l1,l2) => (extractVarsLinearExp(l1,listOfVars) + extractVarsLinearExp(l2,listOfVars))
  
  case DivNotLin(l1,l2) => (extractVarsLinearExp(l1,listOfVars) + 1000*extractVarsLinearExp(l2,listOfVars)) //Only linear in the dividend, in the divisor it is always non-linear 

  case ResNotLin(l1,l2) => (1000*extractVarsLinearExp(l1,listOfVars) + 1000*extractVarsLinearExp(l2,listOfVars)) // remainder never can be linear

  case PowNotLin(l1,l2) => (1000*extractVarsLinearExp(l1,listOfVars) + 1000*extractVarsLinearExp(l2,listOfVars)) // pow never can be linear

  case FuncNotLin(s,list) => funcextract(s,list,listOfVars)

 }


//new
 def funcextract(s:String,list:List[NotLin],listOfVars:List[String]):Int = (s,list) match {
  case ("PI",List()) => 0
  case ("E",List()) => 0
  case ("max",List(n1,n2)) => math.max(extractVarsLinearExp(n1,listOfVars),extractVarsLinearExp(n2,listOfVars))
  case ("min",List(n1,n2)) => math.min(extractVarsLinearExp(n1,listOfVars),extractVarsLinearExp(n2,listOfVars))
  // Any variables found in the following functions make the expression non-linear 
  case ("exp",List(n)) => 1000*extractVarsLinearExp(n,listOfVars)
  case ("sin",List(n)) => 1000*extractVarsLinearExp(n,listOfVars)
  case ("cos",List(n)) => 1000*extractVarsLinearExp(n,listOfVars)
  case ("tan",List(n)) => 1000*extractVarsLinearExp(n,listOfVars)
  case ("arcsin",List(n)) => 1000*extractVarsLinearExp(n,listOfVars)
  case ("arccos",List(n)) => 1000*extractVarsLinearExp(n,listOfVars)
  case ("arctan",List(n)) => 1000*extractVarsLinearExp(n,listOfVars)
  case ("sinh",List(n)) => 1000*extractVarsLinearExp(n,listOfVars)
  case ("cosh",List(n)) => 1000*extractVarsLinearExp(n,listOfVars)
  case ("tanh",List(n)) => 1000*extractVarsLinearExp(n,listOfVars)
  case ("sqrt",List(n)) => 1000*extractVarsLinearExp(n,listOfVars)
  case ("log",List(n)) => 1000*extractVarsLinearExp(n,listOfVars)
  case ("log10",List(n)) => 1000*extractVarsLinearExp(n,listOfVars)
  case (_,_) => throw new RuntimeException(s"Unknown function '${s}(${list.mkString(",")})', or the number of arguments are incorrect")

  }

////// New /////// 
//verify if the free varibles had already been declarated before being used.
  def assigmentsVerify(prog:Syntax): Set[String] = prog match {
    
    case Seq(p,q) => {
      var as=extractAssigments(p) ++ extractAssigments(q)
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
    case ITE(ifP,thenP,elseP) =>getVars(ifP)++assigmentsVerify(thenP)++assigmentsVerify(elseP) //probably this case does not have any effect because it is obligatory the declaration of variables above the instructions 
    case While(pre,c,p) => assigmentsVerify(pre)  
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





// Verify if exists free variables  already been declarated before being used, and also if they are used variables that are not declareted
// Verify if the linear expressions of the Eq.Diffs are linears
  def isClosed(prog:Syntax): Either[String,Unit] = {
    val declVarTHEN = getFstDeclVarsTHEN(prog) //make a set with the firsts declareted variables (THEN)
     
    val declVarELSE = getFstDeclVarsELSE(prog) //make a set with the firsts declareted variables (ELSE)
    
    val usedVarsTHEN = getUsedVarsTHEN(prog)   //make a set with the firsts used variables
    
    val usedVarsELSE = getUsedVarsELSE(prog)  //make a set with the firsts used variables
    
    val asVerify=assigmentsVerify(prog) //make a set of free variables that not had been declareted before of ther invocation.

    val varsEqDiffVerify=verifyLinearityEqsDiff(prog)
    //println("linear?:"+varsEqDiffVerify)


    if (asVerify.nonEmpty) //Verify if exist free variables that not had been declareted before, if exist i print it.
      Left(s"Initial declaration has free variables that were not declared: ${asVerify.mkString(", ")}")
    else if (!usedVarsTHEN.forall(declVarTHEN)) 
      Left(s"Variable(s) not declared: ${((usedVarsTHEN -- declVarTHEN)++(usedVarsELSE-- declVarELSE)).mkString(", ")}")
    else if (!usedVarsELSE.forall(declVarELSE))
       Left(s"Variable(s) not declared: ${((usedVarsTHEN -- declVarTHEN)++(usedVarsELSE-- declVarELSE)).mkString(", ")}")
    else if (varsEqDiffVerify.nonEmpty)
      Left(s"There are differential equations that are not linear: ${varsEqDiffVerify.get.map(Show.apply).mkString(", ")}")
    else 
      Right(())
  }















////////////////////////////////////////////////////////////////////////////////////////////////////////

  // take a list of differential equations and take the variables with the tilde on top! (ex: p'=1+x returns set(p))
  def getDefVars(eqs: List[DiffEq]): Set[String] =
    eqs.map(_.v.v).toSet

////////////////////////////////////////////////////////////////////////////////////////////////////////










///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

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








////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


// New
  // Take a program and remove the first declared variables (from the first atomic)
  //@scala.annotation.tailrec
  def getFstDeclVarsTHEN(prog:Syntax): Set[String] = prog match {
    
    case Seq(p,q) => {
      var as=extractAssigments(Seq(p,q))
      var asSet=as.map(_.v.v).toSet
      
      return asSet
      }
    case Atomic(a, _)     => a.map(_.v.v).toSet // creates an set of the first variables declared
   
    
      
    case ITE(_, thenP, _) => getFstDeclVarsTHEN(thenP) 
    
    case While(pre, _, _) => getFstDeclVarsTHEN(pre)
  }

/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////



//New
  def getFstDeclVarsELSE(prog:Syntax): Set[String] = prog match {
    case Atomic(a, _)     => a.map(_.v.v).toSet // creates an set of the first variables declared
   
    case Seq(p,q) => {
      var as=extractAssigments(p) ++ extractAssigments(q)
      var asSet=as.map(_.v.v).toSet
      return asSet
      }
    case ITE(_, _, elseP) =>getFstDeclVarsELSE(elseP) 
    case While(pre, _, _) => getFstDeclVarsELSE(pre)
  }









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
    case VarNotLin(v) => Set(v)
    case ValueNotLin(_) => Set()
    case AddNotLin(l1, l2) => getVars(l1) ++ getVars(l2)
    case MultNotLin(l1, l2) => getVars(l1) ++ getVars(l2)
    case DivNotLin(l1,l2) => getVars(l1) ++ getVars(l2)
    case ResNotLin(l1,l2) => getVars(l1) ++ getVars(l2)  
    case PowNotLin(l1,l2) => getVars(l1) ++ getVars(l2)
    case FuncNotLin(s,list) => getVarsAux(list)
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
    case VarNotLin(v) => List(v)
    case ValueNotLin(_) => List()
    case AddNotLin(l1, l2) => getVarsList(l1) ++ getVarsList(l2)
    case MultNotLin(l1, l2) => getVarsList(l1) ++ getVarsList(l2)
    case DivNotLin(l1,l2) => getVarsList(l1) ++ getVarsList(l2)
    case ResNotLin(l1,l2) => getVarsList(l1) ++ getVarsList(l2) 
    case PowNotLin(l1,l2) => getVarsList(l1) ++ getVarsList(l2)
    case FuncNotLin(s,list) => getVarsListAux(list)
    
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
      case SVar("e") => SFun("E", Nil)
      case SVar("pi") => SFun("PI", Nil)
      case SVar(_) => e
      case SArg() => e
<<<<<<< HEAD
      case SFun("_e", List(SVal(0))) => SVar("e")
      case SFun("_pi", List(SVal(0))) => SVar("pi")
      case SFun(f, List(SVal(0)))
        if f != "sin" && f != "cos" => SVar(f)
      case SFun("e", args) => SFun("E", args.map(fixVars))
=======
      case SFun("_e", List(SVal(0))) => SVar("_e")
      case SFun("_pi", List(SVal(0))) => SVar("_pi")
      case SFun(f, List(SVal(0)))
        if (f != "sin" && f != "cos" && f != "tan" && f != "exp" && f != "arcsin" && f != "arccos" && f != "arctan" && f != "sinh" && f != "cosh" && f != "tanh" && f != "sqrt" && f != "log" && f != "log10")=> SVar(f)
      case SFun("e", args) => SFun("E", args.map(fixVars))
      case SFun("pi",args)=> SFun("PI", args.map(fixVars))
      case SFun("log10",args)=>SDiv(fixVars(SFun("log",args)),fixVars(SFun("log",List(SVal(10)))))
>>>>>>> b032e87
      case SFun(f, args) => SFun(f, args.map(fixVars))
      case SDiv(e1, e2) => SDiv(fixVars(e1), fixVars(e2))
      case SRes(e1, e2) => SRes(fixVars(e1), fixVars(e2))
      case SMult(e1, e2) => SMult(fixVars(e1), fixVars(e2))
      case SPow(e1, e2) => SPow(fixVars(e1), fixVars(e2))
      case SAdd(e1, e2) => SAdd(fixVars(e1), fixVars(e2))
      case SSub(e1, e2) => SSub(fixVars(e1), fixVars(e2))
    }
<<<<<<< HEAD
    // println(s"Fixing ${(e)} into ${(res)}")
||||||| d7fdfc7
    println(s"Fixing ${(e)} into ${(res)}")
=======
<<<<<<< HEAD
    println(s"Fixing ${(e)} into ${(res)}")
=======
    //println(s"Fixing ${(e)} into ${(res)}")
>>>>>>> b032e87
>>>>>>> 15751d985bb05ed30554a998e11f301c4ba5fac6
    res
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
