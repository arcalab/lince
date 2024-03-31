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

    var dict_Graph2D: Map[Double, (String, String)] = Map() 
    var dict_Graph3D: Map[Double, (String, String, String)] = Map()
    var jsOutput = ""    
    var graph_name = "" 

    var js = "var colors = Plotly.d3.scale.category10();\n"
    println(variables_List)
    var varList = if (variables_List.isEmpty) traces.keys.toList.take(2) else variables_List

    if (variables_List.length == 2){
      val (js2, g_name, dict_Graph) = buildTraces2D(traces,colorIDs, varList)
      jsOutput = js2
      dict_Graph2D = dict_Graph
      graph_name = g_name
    }  
    else{
      val (js2, g_name, dict_Graph) = buildTraces3D(traces,colorIDs, varList) 
      jsOutput = js2
      dict_Graph3D = dict_Graph
      graph_name = g_name
    } 

    js += jsOutput
    val jsBoundaries = if (variables_List.length == 2) buildBoundaries2D(boundaries,colorIDs, varList, dict_Graph2D, graph_name) else buildBoundaries3D(boundaries,colorIDs, varList, dict_Graph3D, graph_name)
    js += jsBoundaries

    val jsWarnings = if (variables_List.length == 2) buildWarnings2D(traj,inScope,colorIDs, dict_Graph2D, graph_name) else buildWarnings3D(traj,inScope,colorIDs, dict_Graph3D, graph_name)
    js += jsWarnings

    val traceNames = List("t_" + graph_name) ++   boundaries.keys.filter(varList.contains).flatMap 
                      { key => List("b_out_" + key, "b_in_" + key, "w_" + key)}.toList

    js += s"var data = ${traceNames.mkString("[",",","]")};"

    if (variables_List.length == 2) {
      js += s"\nvar layout = {hovermode:'closest'};" 
    }
    else {js += s"""
      var layout = {
          hovermode: 'closest',
          scene: {
              xaxis: {title: '${variables_List(0).replaceAll("_", "")}'},
              yaxis: {title: '${variables_List(1).replaceAll("_", "")}'},
              zaxis: {title: '${variables_List(2).replaceAll("_", "")}'}
          }
      };
      """
    }
    
    js += s"\nPlotly.newPlot('$divName', data, layout, {showSendToCloud: true});"
    
    println(js)

    js    
  }


    /////////////////////////////////////////////////
    //////    Functions to build a 2D Graph    ////// 
    /////////////////////////////////////////////////

/**
  * Constructs JavaScript blocks for traces 2D based on the specified traces, color IDs, and variable list.
  * Also builds a dictionary to store the values of the graph.
  *
  * @param traces         Map containing traces for different variables.
  * @param colorIDs       Map associating variable names with color IDs.
  * @param varList List of variable names to consider.
  * @return               A tuple containing the JavaScript blocks, graph name, and dictionary of graph values.
  */

  private def buildTraces2D(traces: Traces, colorIDs: Map[String, Int], varList: List[String]): (String, String, Map[Double, (String, String)])  = {
    var js = ""    
    var graph_name = ""    
    var dict_Graph: Map[Double, (String, String)] = Map()      

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
    
    (js, graph_name, dict_Graph)
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
  private def buildBoundaries2D(boundaries: Boundaries
                              , colorIDs: Map[String, Int]
                              , varList: List[String]
                              , dict_Graph: Map[Double, (String, String)]
                              , graph_name: String): String = {
    var js = ""

    for ((variable, values) <- boundaries) {
      val (outs,ins) = values.toList.partition(pair=>pair._1.isLeft)
      if(variable == varList(0) || variable == varList(1)){
        js += mkMarkers2D(variable,"out",outs,
          s"""{color: 'rgb(255, 255, 255)',
            | size: 10,
            | line: {
            |   color: colors(${colorIDs.getOrElse(variable, 0)}),
            |   width: 2}}""".stripMargin, varList, dict_Graph, graph_name)
        js += mkMarkers2D(variable,"in",ins,
          s"""{color: colors(${colorIDs.getOrElse(variable, 0)}),
            | size: 10,
            | line: {
            |   color: colors(${colorIDs.getOrElse(variable, 0)}),
            |   width: 2}}""".stripMargin, varList, dict_Graph, graph_name)
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
  private def buildWarnings2D(traj: Traj, inScope:Double=>Boolean
                            , colorIDs: Map[String, Int]
                            , dict_Graph: Map[Double, (String, String)]
                            , graph_name: String): String = {
    var js = ""
    for (variable <- traj.getVars) {
      js += mkWarnings2D(variable,traj,inScope,
        s"""{color: colors(${colorIDs.getOrElse(variable, 0)}),
           | size: 15,
           | line: {
           |   color: 'rgb(0,0,0)',
           |   width: 2}}""".stripMargin, dict_Graph, graph_name)
    }
    js
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
  private def mkMarkers2D(variable:String, inout:String
                        , data:List[(Either[Double,Double],(Double,String))]
                        ,style: String, variables_List: List[String]
                        , dict_Graph: Map[Double, (String, String)]
                        , graph_name: String): String = {

    var time_values = data.map(_._1.fold(x=>x,x=>x))
    val (xValue, yValue) = dict_Graph.getOrElse(time_values.headOption.getOrElse(0.0), ("", ""))
    
    s"""var b_${inout}_$variable = {
       |   x: [${xValue}],
       |   y: [${yValue}],
       |   text: ${data.map(s=>"'" + fixStr(s._2._2) + "'").mkString("[",",","]")},
       |   mode: 'markers',
       |   marker: $style,
       |   type: 'scatter',
       |   legendgroup: 'g_${remove_variable(graph_name)}',
       |   name: 'boundary of ${remove_variable(variable)}',
       |   showlegend: false,
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
  private def mkWarnings2D(variable: String, traj: Traj
                       , inScope: Double=>Boolean
                       , style:String
                       , dict_Graph: Map[Double, (String, String)]
                       , graph_name: String): String = {                        

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
           |   legendgroup: 'g_${remove_variable(graph_name)}',
           |   name: 'Warning',
           |   showlegend: false
           |};""".stripMargin


      case _ => s"var w_$variable = {};"
      }
  }

    /////////////////////////////////////////////////
    //////    Functions to build a 3D Graph    ////// 
    /////////////////////////////////////////////////

/**
  * Constructs JavaScript blocks for traces 2D based on the specified traces, color IDs, and variable list.
  * Also builds a dictionary to store the values of the graph.
  *
  * @param traces         Map containing traces for different variables.
  * @param colorIDs       Map associating variable names with color IDs.
  * @param varList        List of variable names to consider.
  * @return               A tuple containing the JavaScript blocks, graph name, and dictionary of graph values.
  */

  private def buildTraces3D(traces: Traces, colorIDs: Map[String, Int], varList: List[String]): (String, String, Map[Double, (String, String, String)])  = {
    var js = ""    
    var graph_name = ""    
    var dict_Graph: Map[Double, (String, String, String)] = Map()      

    var t: List[Double] = List.empty
    var x_axis: List[String] = List.empty
    var y_axis: List[String] = List.empty
    var z_axis: List[String] = List.empty

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
      else if(variable == varList(2)){
        val (time, z) = processValues(values.toList)           
        z_axis = z
      }
    }

    dict_Graph = t.zip(x_axis.zip(y_axis.zip(z_axis))).map { 
      case (time, (x, (y, z))) => (time, (x, y, z))
    }.toMap

    if (x_axis.nonEmpty && y_axis.nonEmpty && z_axis.nonEmpty) {
      js +=
        s"""var t_${graph_name.toString} = {
          |   x: ${x_axis.mkString("[", ",", "]")},
          |   y: ${y_axis.mkString("[", ",", "]")},
          |   z: ${z_axis.mkString("[", ",", "]")},
          |   mode: 'lines',
          |   line: {color: colors(${colorIDs.getOrElse(graph_name,0)})},
          |   legendgroup: 'g_${remove_variable(graph_name.toString)}',
          |   name: '${remove_variable(graph_name.toString)}',
          |   type: 'scatter3d'
          |};
          """.stripMargin
    }   
    
    (js, graph_name, dict_Graph)
  }

  /**
  * Constructs JavaScript blocks for boundaries based on the specified boundaries, color IDs, variable list, and dictionary of graph values.
  *
  * @param boundaries     Map containing boundaries for different variables.
  * @param colorIDs       Map associating variable names with color IDs.
  * @param varList        List of variable names to consider.
  * @param dict_Graph     Dictionary that have the values of the graph.
  * @return               JavaScript blocks representing the specified boundaries.
  */
  private def buildBoundaries3D(boundaries: Boundaries
                              , colorIDs: Map[String, Int]
                              , varList: List[String]
                              , dict_Graph: Map[Double, (String, String, String)]
                              , graph_name: String): String = {
    var js = ""

    for ((variable, values) <- boundaries) {
      val (outs,ins) = values.toList.partition(pair=>pair._1.isLeft)
      if(variable == varList(0) || variable == varList(1) || variable == varList(2)){
        js += mkMarkers3D(variable,"out",outs,
          s"""{color: 'rgb(255, 255, 255)',
            | size: 10,
            | line: {
            |   color: colors(${colorIDs.getOrElse(variable, 0)}),
            |   width: 2}}""".stripMargin, varList, dict_Graph, graph_name)
        js += mkMarkers3D(variable,"in",ins,
          s"""{color: colors(${colorIDs.getOrElse(variable, 0)}),
            | size: 10,
            | line: {
            |   color: colors(${colorIDs.getOrElse(variable, 0)}),
            |   width: 2}}""".stripMargin, varList, dict_Graph, graph_name)
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
  private def buildWarnings3D(traj: Traj, inScope:Double=>Boolean
                            , colorIDs: Map[String, Int]
                            , dict_Graph: Map[Double, (String, String, String)]
                            , graph_name: String): String = {
    var js = ""
    for (variable <- traj.getVars) {
      js += mkWarnings3D(variable,traj,inScope,
        s"""{color: colors(${colorIDs.getOrElse(variable, 0)}),
           | size: 15,
           | line: {
           |   color: 'rgb(0,0,0)',
           |   width: 2}}""".stripMargin, dict_Graph, graph_name)
    }
    js
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
  private def mkMarkers3D(variable:String, inout:String
                        , data:List[(Either[Double,Double],(Double,String))]
                        , style: String, variables_List: List[String]
                        , dict_Graph: Map[Double, (String, String, String)]
                        , graph_name: String): String = {

    var time_values = data.map(_._1.fold(x=>x,x=>x))
    val (xValue, yValue, zValue) = dict_Graph.getOrElse(time_values.headOption.getOrElse(0.0), ("", ""))
    
    s"""var b_${inout}_$variable = {
       |   x: [${xValue}],
       |   y: [${yValue}],
       |   z: [${zValue}],
       |   text: ${data.map(s=>"'" + fixStr(s._2._2) + "'").mkString("[",",","]")},
       |   mode: 'markers',
       |   marker: $style,
       |   type: 'scatter',
       |   legendgroup: 'g_${remove_variable(graph_name)}',
       |   name: 'boundary of ${remove_variable(variable)}',
       |   showlegend: false,
       |   type: 'scatter3d'
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
  private def mkWarnings3D(variable: String, traj: Traj
                       , inScope: Double=>Boolean
                       , style:String
                       , dict_Graph: Map[Double, (String, String, String)]
                       , graph_name: String): String = {                        

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
        
        val (x_axis, y_axis, z_axis) = x.map(dict_Graph.getOrElse(_, ("", "", ""))).unzip3

        s"""var w_$variable = {
           |   x: ${x_axis.mkString("[",",","]")},
           |   y: ${y_axis.mkString("[",",","]")},
           |   z: ${z_axis.mkString("[",",","]")},
           |   text: ${msg.mkString("[",",","]")},
           |   mode: 'markers',
           |   marker: $style,
           |   type: 'scatter',
           |   legendgroup: 'g_${remove_variable(graph_name)}',
           |   name: 'Warning',
           |   showlegend: false,
           |   type: 'scatter3d'
           |};""".stripMargin


      case _ => s"var w_$variable = {};"
      }
  }


    ///////////////////////////////////////
    //////    Auxiliar Functions     ////// 
    ///////////////////////////////////////

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

  private def fixStr(str:String): String =
    str.replaceAll("\\\\", "\\\\\\\\")

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
}
