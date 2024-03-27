package hprog.ast
import Syntax._

sealed abstract class SyntaxConfig   

object SyntaxConfig {

  /** An atomic program is a list of variables, a int representing the max time and a int representing max iterations //COnfig */
  case class SyntaxConfig(axis:AxisList,maxTime:Value,maxIterations:Value) 

  /* AxisList is a lsit of strings that represents a axis' variables in a Lince Program */
  case class AxisList(v:List[Var]) 

  /* Var is a string that represents a variable in a Lince Program */
  case class Var(v:String) 

  /* Value is a double that represents the max time or max iterations that a lince program can run */
  case class Value(v:Double) 

}
