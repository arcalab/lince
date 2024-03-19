package hprog.ast
import Syntax._
/*
A element `p` of the class `SyntaxConfig` is our internal representation of a Config program `p`.

Overview of the grammar:

 p := Atomic(vars, maxTime, maxIterations)
 vars := Var(String)
 maxTime := Value(Double)
 maxIterations := Value(Double)

*/
object SyntaxConfig {
  
  sealed abstract class SyntaxConfig     

  /*def ~(other:SyntaxConfig): SyntaxConfig = other match {
      case _ => new Seq(this,other)
    }*/

  /** Sequence of programs is a program*/
  case class Seq(p:SyntaxConfig,q:SyntaxConfig) extends SyntaxConfig

  /** An atomic program is a list of variables, a int representing the max time and a int representing max iterations*/
  case class Prog(axis:AxisList,maxTime:Value,maxIterations:Value) extends SyntaxConfig    

  /* AxisList is a lsit of strings that represents a axis' variables in a Lince Program */
  case class AxisList(v:List[Var]) extends SyntaxConfig 
  
  /* Var is a string that represents a variable in a Lince Program */
  case class Var(v:String) extends SyntaxConfig 

  /* Value is a double that represents the max time or max iterations that a lince program can run */
  case class Value(v:Double) extends SyntaxConfig 

}
