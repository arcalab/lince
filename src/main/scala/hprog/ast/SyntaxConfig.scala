package hprog.ast
import Syntax._

sealed abstract class SyntaxConfig   

object SyntaxConfig {

  sealed trait ConfigVal
  case class StrValue(v: String) extends ConfigVal
  case class MaxIterationsValue(v: Double) extends ConfigVal  
  case class MaxTimeValue(v: Double) extends ConfigVal
  case class SeqValue(v: List[ConfigVal]) extends ConfigVal

  case class SyntaxConfig(options: Map[String, ConfigVal]) {
    
    def getAxis: AxisList = options.get("Axis").collect { 
      case SeqValue(values) => 
        AxisList(values.collect { 
          case StrValue(v) => Var(v) 
        })
    }.getOrElse(defaultAxis)

    def getMaxTime: MaxTime = options.get("MaxTimeValue").collect { 
      case MaxTimeValue(v) => 
        MaxTime(v)
    }.getOrElse(defaultMaxTime)

    def getMaxIterations: MaxIterations = options.get("MaxIterationsValue").collect { 
      case MaxIterationsValue(v) => 
        MaxIterations(v.toInt)
    }.getOrElse(defaultMaxIterations)
    
    private val defaultAxis = AxisList(List())
    private val defaultMaxTime = MaxTime(20.0)
    private val defaultMaxIterations = MaxIterations(100)
  }
  
  case class AxisList(v: List[Var]) 
  case class Var(v: String) 
  case class MaxTime(v: Double) 
  case class MaxIterations(v: Int) 

}

