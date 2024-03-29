package hprog.backend

import hprog.ast.{SDiv, SSub, SVal}
import hprog.frontend.CommonTypes.Valuation
import hprog.frontend.{Eval, Traj}

object TrajToJSV2 {

  // Type of intermediate structures
  private type Traces      = Map[String,TraceVar]
  private type TraceVar    = Map[Double,Either[Double,(Double,Double)]] // time -> 1 or 2 points (if boundary)
  private type Boundaries  = Map[String,BoundaryVar]
  private type BoundaryVar = Map[Either[Double,Double],(Double,String)] // left/right of a time t -> value and comment
  
  // Auxiliar Types
  type JSString = String // 


  def apply(traj:Traj,divName:String, range:Option[(Double,Double)]=None, hideCont:Boolean=true, variables_List: List[String]): JSString = {
    
    val dur = traj.getDur    

    // trick to avoid many sampling when already lots of boundaries exist
    val nbrSamples = 0.max(100 - traj.getInits.getOrElse(Map()).size)


    val max: Double = Eval(dur.getOrElse(SVal(10)),0)

    val colorIDs: Map[String,Int] =
      traj.getVars.zipWithIndex.toMap

    // traces are mappings from variables t0 lists of "y" values
    var traces:Traces          = Map().withDefaultValue(Map())
   // each point in time has 1 or 2 valuesSS
    var boundaries: Boundaries = Map().withDefaultValue(Map())
   // each boundary variable maps begin or end times to a value and a note.

    // Generate sampling values for time - 100 values from 0 to max
    val (start,end) = range match {
      case Some((s,e)) =>
        if (dur.isDefined)
          (s.max(0).min(max),e.max(0).min(max))
        else
          (s max 0, e max 0)
      case None => if (max<=0) (0.0,0.0) else (0.0,max)
    }
    val samples =
      if (nbrSamples==0)
        Nil
      else if ((end-start)<=0)
        List(SVal(start))
      else
        SVal(start) :: (0 to nbrSamples).toList.map(_=> SDiv(SSub(SVal(end),SVal(start)),SVal(nbrSamples)))
           // to end by ((end-start) / 10)

    // checks if a time value is within the scope
    def inScope(t:Double): Boolean = t>=start && t<=end


    val sampleValues = traj.evalBatch(SVal(start),SVal(end), SDiv(SSub(SVal(end),SVal(start)),SVal(nbrSamples))) //(samples)


    for ((t,x) <- sampleValues; (variable,value) <- x)
      traces += variable -> (traces(variable) + (Eval(t)->Left(Eval(value))))
    // Add ending points to "boundaries" and to "traces"
    for (e <- traj.getEnds;
         (t,endValues) <- e if inScope(Eval(t,0));
         (variable,endValue) <- endValues) {
      val t2 = Eval(t,0) // evaluate the time expression of next point
      val endValue2 = Eval(endValue,t2) // evaluate value for that point

      traces     += variable -> (traces(variable)     + (t2->Left(endValue2)))
      boundaries += variable -> (boundaries(variable) + (Left(t2)->(endValue2,"")))
    }

    // add init points to boundaries and traces
    for (is <- traj.getInits;
         (t,initValues) <- is if inScope(Eval(t,0));
         (variable,initValue) <- initValues) {
      val t2 = Eval(t,0)
      val initValue2 = Eval(initValue,t2)

      val nextTraceT = traces(variable).get(t2) match {
        case Some(Left(v)) => Right(v,initValue2)
        case Some(x) => x
        case None => Left(initValue2)
      }
      traces     += variable -> (traces(variable)     + (t2->nextTraceT))
      boundaries += variable -> (boundaries(variable) + (Right(t2)->(initValue2,"")))
    }

    // adding notes to boundary points
    for (n <- traj.getNotes ; (expr,note) <- n) {
      val t = Eval(expr)
      if (inScope(t))
        boundaries = boundaries.view.mapValues(vals => addNote(t,note,vals)).toMap
    }
    def addNote(t: Double, n: String, vals: BoundaryVar): BoundaryVar = {
      vals.get(Right(t)) match {
        case Some((value,n2)) => vals + (Right(t)->(value,n2++n++"<br>"))
        case None => vals.get(Left(t)) match {
          case Some((value,n2)) => vals + (Left(t) -> (value,n2++n++"<br>"))
          case None => vals + (Left(t)->(0,n)) // should not happen...
        }
      }
    }

    // clean boundaries in continuous points
    if (hideCont) {
      boundaries = boundaries.view.mapValues(filterCont).toMap
    }

    /////S
    // Build the JavaScript code to generate graph
    /////
    var js = "var colors = Plotly.d3.scale.category10();\n"

    var varList = if (variables_List.isEmpty) traces.keys.toList.take(2) else variables_List

    val (js2, graph_name, dict_Graph) = buildTraces(traces,colorIDs, varList)  

    js += js2
    js += buildBoundaries(boundaries,colorIDs, varList, dict_Graph)
    js += buildWarnings(traj,inScope,colorIDs, dict_Graph)

    val traceNames = List("t_" + graph_name) ++   boundaries.keys.filter(varList.contains).flatMap 
                      { key => List("b_out_" + key, "b_in_" + key, "w_" + key)}.toList

    js += s"var data = ${traceNames.mkString("[",",","]")};" +
      s"\nvar layout = {hovermode:'closest'};" +
      s"\nPlotly.newPlot('$divName', data, layout, {showSendToCloud: true});"
    
    println(js)

    js    
  }


  ///////

/**
  * Constructs JavaScript blocks for traces based on the specified traces, color IDs, and variable list.
  * Also builds a dictionary to store the values of the graph.
  *
  * @param traces         Map containing traces for different variables.
  * @param colorIDs       Map associating variable names with color IDs.
  * @param varList List of variable names to consider.
  * @return               A tuple containing the JavaScript blocks, graph name, and dictionary of graph values.
  */

  private def buildTraces(traces: Traces, colorIDs: Map[String, Int], varList: List[String]): (String, String, Map[Double, (String, String)])  = {
    var js = ""    
    var graph_name = ""    
    var dict_Graph: Map[Double, (String, String)] = Map()      

    if(varList.length == 2){

      var t: List[Double] = List.empty
      var x_axis: List[String] = List.empty
      var y_axis: List[String] = List.empty

      for ((variable, values) <- traces) {
        if(variable == varList(0)) {        
          graph_name ++= variable
          val (time, x) = processValues(values.toList)   
          t = time
          x_axis = x     
        } 
        else if(variable == varList(1)){
          val (time, y) = processValues(values.toList)           
          y_axis = y
        }
      }
      dict_Graph = t.zip(x_axis.zip(y_axis)).toMap

      if (x_axis.nonEmpty && y_axis.nonEmpty) {
        js +=
          s"""var t_${graph_name.toString} = {
            |   x: ${x_axis.mkString("[", ",", "]")},
            |   y: ${y_axis.mkString("[", ",", "]")},
            |   mode: 'lines',
            |   line: {color: colors(${colorIDs.getOrElse(graph_name,0)})},
            |   legendgroup: 'g_${remove_variable(graph_name.toString)}',
            |   name: '${remove_variable(graph_name.toString)}'
            |};
            """.stripMargin
      }
    }
    else if(varList.length == 3) {
      var t: List[Double] = List.empty
      var x_axis: List[String] = List.empty
      var y_axis: List[String] = List.empty
      var z_axis: List[String] = List.empty
    }
    
    (js, graph_name, dict_Graph)
  }

  /**
  * Processes the values of a trace to extract and separate the time values (xt) and variable values (x).
  *
  * @param values List of time-value pairs, where the time can be either a Double or a Double.
  * @return       A tuple containing the list of time values (xt) and the list of variable values (x).
  */
  def processValues(values: List[(Double, Either[Double,(Double, Double)])]): (List[Double], List[String]) = {
    val tr = values.sortWith(_._1 <= _._1).flatMap(expandPoint)
    val (xt, x) = tr.unzip
    (xt, x)
  }


  /**
  * Constructs JavaScript blocks for boundaries based on the specified boundaries, color IDs, variable list, and dictionary of graph values.
  *
  * @param boundaries     Map containing boundaries for different variables.
  * @param colorIDs       Map associating variable names with color IDs.
  * @param varList List of variable names to consider.
  * @param dict_Graph     Dictionary that have the values of the graph.
  * @return               JavaScript blocks representing the specified boundaries.
  */
  private def buildBoundaries(boundaries: Boundaries, colorIDs: Map[String, Int], varList: List[String], dict_Graph: Map[Double, (String, String)]): String = {
    var js = ""

    for ((variable, values) <- boundaries) {
      val (outs,ins) = values.toList.partition(pair=>pair._1.isLeft)
      if(variable == varList(0) || variable == varList(1)){
        js += mkMarkers(variable,"out",outs,
          s"""{color: 'rgb(255, 255, 255)',
            | size: 10,
            | line: {
            |   color: colors(${colorIDs.getOrElse(variable, 0)}),
            |   width: 2}}""".stripMargin, varList, dict_Graph)
        js += mkMarkers(variable,"in",ins,
          s"""{color: colors(${colorIDs.getOrElse(variable, 0)}),
            | size: 10,
            | line: {
            |   color: colors(${colorIDs.getOrElse(variable, 0)}),
            |   width: 2}}""".stripMargin, varList, dict_Graph)
      }
    }
    js
  }

  /**
  * Constructs JavaScript blocks for warnings based on the specified trajectory, scope, color IDs, and dictionary of graph values.
  *
  * @param traj         The trajectory containing warnings.
  * @param inScope      Function to check if a value is within scope.
  * @param colorIDs     Map associating variable names with color IDs.
  * @param dict_Graph   Dictionary that have the values of the graph.
  * @return             JavaScript blocks representing the specified warnings.
  */
  private def buildWarnings(traj: Traj, inScope:Double=>Boolean, colorIDs: Map[String, Int], dict_Graph: Map[Double, (String, String)]): String = {
    var js = ""
    for (variable <- traj.getVars) {
      js += mkWarnings(variable,traj,inScope,
        s"""{color: colors(${colorIDs.getOrElse(variable, 0)}),
           | size: 15,
           | line: {
           |   color: 'rgb(0,0,0)',
           |   width: 2}}""".stripMargin, dict_Graph)
    }
    js
  }

 def remove_variable(variable:String):String = {
  var aux=variable.substring(1,variable.length)
  return aux
 }

  private def expandPoint(point:(Double,Either[Double,(Double,Double)])): List[(Double,String)] =
    point match {
      case (t,Left(v)) => List((t,v.toString))
      case (t,Right((v1,v2))) => List((t,v1.toString),(t,"null"),(t,v2.toString))
    }

  private def filterCont(boundary: BoundaryVar): BoundaryVar = {
    boundary.filter({
      case (Left(t),v1)  => boundary.get(Right(t)) match {
          case Some(v2) => v1._1 != v2._1
          case None     => true
        }
      case (Right(t),v1) => boundary.get(Left(t)) match {
          case Some(v2) => v1._1 != v2._1
          case None     => true
        }
    })
  }

  /**
  * Constructs a JavaScript block for markers based on the specified variable, direction, data, style, and dictionary of graph values.
  *
  * @param variable       The name of the variable.
  * @param inout          Specifies the direction of the marker (either "in" or "out").
  * @param data           List of data points to be plotted.
  * @param style          Style of the marker.
  * @param variables_List List of variable names to consider.
  * @param dict_Graph     Dictionary the have the values of the graph.
  * @return               JavaScript block representing the specified markers.
  */
  private def mkMarkers(variable:String, inout:String, data:List[(Either[Double,Double],(Double,String))],style: String, variables_List: List[String], dict_Graph: Map[Double, (String, String)]): String = {

    var time_values = data.map(_._1.fold(x=>x,x=>x))
    val (xValue, yValue) = dict_Graph.getOrElse(time_values.headOption.getOrElse(0.0), ("", ""))
    
    s"""var b_${inout}_$variable = {
       |   x: [${xValue}],
       |   y: [${yValue}],
       |   text: ${data.map(s=>"'" + fixStr(s._2._2) + "'").mkString("[",",","]")},
       |   mode: 'markers',
       |   marker: $style,
       |   type: 'scatter',
       |   legendgroup: 'g_${remove_variable(variable)}',
       |   name: 'boundary of ${remove_variable(variable)}',
       |   showlegend: false
       |};""".stripMargin
  
  }

  /**
  * Constructs a JavaScript block for warnings based on the specified variable, trajectory, scope, style, and dictionary of graph values.
  *
  * @param variable     The name of the variable.
  * @param traj         The trajectory containing warnings.
  * @param inScope      Function to check if a value is within scope.
  * @param style        Style of the warning marker.
  * @param dict_Graph   Dictionary that have the values of the graph.
  * @return             JavaScript block representing the specified warnings.
  */
  private def mkWarnings(variable: String, traj: Traj
                       , inScope: Double=>Boolean
                       , style:String
                       , dict_Graph: Map[Double, (String, String)]): String = {                        

    (traj.getWarnings,traj.getInits,traj.getEnds) match {
      case (Some(warns),Some(inits),Some(ends)) =>
        val values = (ends ++ inits).map(kv => Eval(kv._1) -> kv._2)
        val (x,y,msg) = warns
          .toList
          .map(es => (Eval(es._1, 0), "'" + fixStr(es._2) + "'"))
          .filter(es => inScope(es._1))
          .sorted
          .map(warn=>(warn._1, Eval(
            values.getOrElse(warn._1,Map():Valuation) // get Valuation at warning warn
                  .getOrElse(variable, SVal(0)) // get expression of Variable
            ), warn._2))
          .unzip3
        
        val (x_axis, y_axis) = x.map(dict_Graph.getOrElse(_, ("", ""))).unzip

        s"""var w_$variable = {
           |   x: ${x_axis.mkString("[",",","]")},
           |   y: ${y_axis.mkString("[",",","]")},
           |   text: ${msg.mkString("[",",","]")},
           |   mode: 'markers',
           |   marker: $style,
           |   type: 'scatter',
           |   legendgroup: 'g_${remove_variable(variable)}',
           |   name: 'Warning',
           |   showlegend: false
           |};""".stripMargin


      case _ => s"var w_$variable = {};"
      }
  }

  private def fixStr(str:String): String =
    str.replaceAll("\\\\", "\\\\\\\\")

}
