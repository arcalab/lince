package hprog.ast
import Syntax._

sealed abstract class SyntaxConfig   

object SyntaxConfig {

  sealed trait ConfigVal
  case class StrValue(v: String) extends ConfigVal
  case class MaxIterationsValue(v: Double) extends ConfigVal  
  case class GraphTypeValue(v: String) extends ConfigVal  
  case class MaxTimeValue(v: Double) extends ConfigVal
  case class AxisListValue(v: List[ConfigVal]) extends ConfigVal
  case class VarList(v: List[ConfigVal]) extends ConfigVal
  case class InitialValuesValue(v: List[(String, List[Double])]) extends ConfigVal 
  case class PerturbationUpToValue(v: Double) extends ConfigVal

  case class SyntaxConfig(options: Map[String, ConfigVal]) {
    
    def getAxis: AxisList = options.get("Axis").collect { 
      case AxisListValue(values) => 
        AxisList(values.collect { 
          case VarList(List(StrValue(v1), StrValue(v2), StrValue(v3))) => TripleVar(v1, v2, v3)
          case VarList(List(StrValue(v1), StrValue(v2))) => PairVar(v1, v2)
          case StrValue(v) => SingleVar(v) 
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

    def getGraphType: GraphType = options.get("GraphTypeValue").collect { 
      case GraphTypeValue(v) => 
        GraphType(v)
    }.getOrElse(defaultGraphType)
    
    def getInitialValues: InitialValues = options.get("InitialValues").collect {
      case InitialValuesValue(values) => 
        InitialValues(values.toMap)
    }.getOrElse(defaultInitialValues)
    
    def getPerturbationUpTo: PerturbationUpTo = options.get("PerturbationUpTo").collect {
      case PerturbationUpToValue(v) => 
        PerturbationUpTo(v)
    }.getOrElse(defaultPerturbationUpTo)

    private val defaultAxis = AxisList(List())
    private val defaultMaxTime = MaxTime(20.0)
    private val defaultMaxIterations = MaxIterations(1000)
    private val defaultGraphType = GraphType("scatter")    
    private val defaultInitialValues = InitialValues(Map.empty[String, List[Double]])
    private val defaultPerturbationUpTo = PerturbationUpTo(0.0) 
  }
  
  sealed trait VarType
  case class SingleVar(v: String) extends VarType
  case class PairVar(v1: String, v2: String) extends VarType
  case class TripleVar(v1: String, v2: String, v3: String) extends VarType

  case class AxisList(v: List[VarType]) 
  case class MaxTime(v: Double) 
  case class MaxIterations(v: Int) 
  case class GraphType(v: String) 
  case class InitialValues(v: Map[String, List[Double]])
  case class PerturbationUpTo(v: Double)
}
