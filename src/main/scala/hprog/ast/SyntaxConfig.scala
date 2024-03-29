/*package hprog.ast
import Syntax._

sealed abstract class SyntaxConfig   

object SyntaxConfig {

  /** An atomic program is a list of variables, a int representing the max time and a int representing max iterations //COnfig */
  case class SyntaxConfig(axis:AxisList,maxTime:MaxTime,maxIterations:MaxIterations) 

  /* AxisList is a lsit of strings that represents a axis' variables in a Lince Program */
  case class AxisList(v:List[Var]) 

  /* Var is a string that represents a variable in a Lince Program */
  case class Var(v:String) 

  /* Value is a double that represents the max time or max iterations that a lince program can run */
  case class MaxTime(v:Double) 

   /* Value is a double that represents the max time or max iterations that a lince program can run */
  case class MaxIterations(v:Int) 

}*/

package hprog.ast
import Syntax._

sealed abstract class SyntaxConfig   

object SyntaxConfig {

  /** An atomic program is a list of variables, a int representing the max time and a int representing max iterations //COnfig */
  case class SyntaxConfig(axis:Option[AxisList] = None, maxTime:Option[MaxTime] = None, maxIterations:Option[MaxIterations] = None) {
    
    def getAxis: AxisList = axis.getOrElse(defaultAxis)
    def getMaxTime: MaxTime = maxTime.getOrElse(defaultMaxTime)
    def getMaxIterations: MaxIterations = maxIterations.getOrElse(defaultMaxIterations)
    
    private val defaultAxis = AxisList(List())
    private val defaultMaxTime = MaxTime(20.0)
    private val defaultMaxIterations = MaxIterations(100)
  }

  /* AxisList is a list of strings that represents an axis' variables in a Lince Program */
  case class AxisList(v:List[Var]) 

  /* Var is a string that represents a variable in a Lince Program */
  case class Var(v:String) 

  /* Value is a double that represents the max time or max iterations that a lince program can run */
  case class MaxTime(v:Double) 

  /* Value is a double that represents the max time or max iterations that a lince program can run */
  case class MaxIterations(v:Int) 

}
