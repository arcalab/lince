/*package hprog.backend

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


  def apply(traj:Traj,divName:String, range:Option[(Double,Double)]=None, hideCont:Boolean=true, variables_List: List[(String, String, Option[String])], graphType: String): JSString = {
    
    val dur = traj.getDur    

    // trick to avoid many sampling when already lots of boundaries exist
    val nbrSamples = 0.max(100 - traj.getInits.getOrElse(Map()).size)

    val max: Double = Eval(dur.getOrElse(SVal(10)),0)
   
    /*val colorIDs: Map[String,Int] =
      traj.getVars.zipWithIndex.toMap
    */
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
    var graph_name: String = "" 
    var graph_color:String = ""
    var varList: List[String] = List()
    var graphsNamesList: List[String] = List()
    var warningsBoundariesList: List[String] = List()
    var counter: Int = 0   
    var colorIDs: Map[String,Int] = Map()    
  
    var variables_List2D: List[(String, String)] = List.empty
    var variables_List3D: List[(String, String, String)] = List.empty

    var varList_Temp = if (variables_List.isEmpty){
      traj.getVars.map(varName => ("t", varName, None))
    } else variables_List

    var js = "var colors = Plotly.d3.scale.category10();\n"

    varList_Temp.foreach {
      case (x, y, None) => variables_List2D = variables_List2D ::: List((x, y))
      case (x, y, Some(z)) => variables_List3D = variables_List3D ::: List((x, y, z))       
    }

    if(variables_List2D.nonEmpty && variables_List3D.nonEmpty){
      println("Problems in the axis definition")
    }
    else if(variables_List2D.nonEmpty){

      //List of String Colors
      var colorsForVariables = variables_List2D.map{              
          case(var1,var2) => s"$var1$var2"        
      }
    
      colorIDs = colorsForVariables.zipWithIndex.toMap
    
      for ((xValue, yValue) <- variables_List2D) {
        varList = List(xValue,yValue) 
        graph_color = s"$xValue$yValue"       

        val (jsGraphs, g_name, dict_Graph) = buildTraces2D(traces,colorIDs, varList, counter, graphType, graph_color)
        dict_Graph2D = dict_Graph
        graph_name = g_name       
        
        graphsNamesList = graphsNamesList ++ List("t_" + graph_name)      
        warningsBoundariesList = if (xValue == "t") warningsBoundariesList ++ List("b_out_" + yValue + counter.toString, "b_in_" + yValue + counter.toString, "w_" + yValue + counter.toString) 
        else warningsBoundariesList ++ List("b_out_" + xValue + counter.toString, "b_in_" + xValue + counter.toString, "w_" + xValue + counter.toString, "b_out_" + yValue + counter.toString, "b_in_" + yValue + counter.toString, "w_" + yValue + counter.toString) 
      
        val jsBoundaries = buildBoundaries2D(boundaries,colorIDs, varList, dict_Graph2D, graph_name, graph_color, counter, graphType)
        val jsWarnings = buildWarnings2D(traj,varList,inScope,colorIDs, dict_Graph2D, graph_name, graph_color, counter, graphType)

        js += jsGraphs
        js += jsBoundaries
        js += jsWarnings

        counter = counter + 1
      }
      val traceNames = graphsNamesList ++ warningsBoundariesList

      js += s"var data = ${traceNames.mkString("[",",","]")};"   
      js += s"\nvar layout = {hovermode:'closest'};"         
      js += s"\nPlotly.newPlot('$divName', data, layout, {showSendToCloud: true});"    
    }
    else if (variables_List3D.nonEmpty){

      //List of String Colors
      var colorsForVariables = variables_List3D.map{        
          case (var1, var2, var3) =>  s"$var1$var2$var3"                  
      }

      colorIDs = colorsForVariables.zipWithIndex.toMap

      for ((xValue, yValue, zValue) <- variables_List3D) {
        varList = List(xValue,yValue,zValue) 
        graph_color = s"$xValue$yValue$zValue" 

        val (jsGraphs, g_name, dict_Graph) = buildTraces3D(traces,colorIDs, varList, counter, graphType, graph_color)
        dict_Graph3D = dict_Graph
        graph_name = g_name        
        
        graphsNamesList = graphsNamesList ++ List("t_" + graph_name)      
        warningsBoundariesList = if (xValue == "t") warningsBoundariesList ++ List("b_out_" + yValue + counter.toString, "b_in_" + yValue + counter.toString, "w_" + yValue + counter.toString) ++ List("b_out_" + zValue + counter.toString, "b_in_" + zValue + counter.toString, "w_" + zValue + counter.toString) 
        else warningsBoundariesList ++ List("b_out_" + xValue + counter.toString, "b_in_" + xValue + counter.toString, "w_" + xValue + counter.toString, "b_out_" + yValue + counter.toString, "b_in_" + yValue + counter.toString, "w_" + yValue + counter.toString) ++ List("b_out_" + zValue + counter.toString, "b_in_" + zValue + counter.toString, "w_" + zValue + counter.toString) 
      
        val jsBoundaries = buildBoundaries3D(boundaries,colorIDs, varList, dict_Graph3D, graph_name, graph_color, counter, graphType)
        val jsWarnings = buildWarnings3D(traj,varList,inScope,colorIDs, dict_Graph3D, graph_name, graph_color, counter, graphType)

        js += jsGraphs
        js += jsBoundaries
        js += jsWarnings

        counter = counter + 1
      }

      val traceNames = graphsNamesList ++ warningsBoundariesList

      js += s"var data = ${traceNames.mkString("[",",","]")};"  
      js += s"\nvar layout = {hovermode:'closest'};" 
      js += s"\nPlotly.newPlot('$divName', data, layout, {showSendToCloud: true});"
    }  

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

  private def buildTraces2D(traces: Traces
                          , colorIDs: Map[String, Int]
                          , varList: List[String]
                          , counter: Int
                          , graphType: String
                          , graph_color: String): (String, String, Map[Double, (String, String)])  = {
    var js = ""    
    var graph_name = varList(0) + varList(1) + counter.toString
    var dict_Graph: Map[Double, (String, String)] = Map()      

    var t: List[Double] = List.empty
    var time_axis: List[String] = List.empty
    var x_axis: List[String] = List.empty
    var y_axis: List[String] = List.empty

    println("VALOR de TRACES ----------------------")
    println(traces)
    for ((variable, values) <- traces) {
      if(variable == varList(0)) {  
        val (time, x) = processValues(values.toList)   
        t = time
        x_axis = x  
        time_axis = time.map(_.toString)  
      } 
      else if(variable == varList(1)){        
        val (time, y) = processValues(values.toList)           
        y_axis = y
        t = time
        time_axis = time.map(_.toString)
      }
    }   

    if(varList(0) == "t") {  
      dict_Graph = t.zip(time_axis.zip(y_axis)).toMap
      x_axis = time_axis
     }
     else {
      dict_Graph = t.zip(x_axis.zip(y_axis)).toMap
     }

    if (x_axis.nonEmpty && y_axis.nonEmpty) {
      js +=
        s"""var t_${graph_name.toString} = {
          |   x: ${x_axis.mkString("[", ",", "]")},
          |   y: ${y_axis.mkString("[", ",", "]")},
          |   mode: 'lines',
          |   line: {color: colors(${colorIDs.getOrElse(graph_color,0)})},
          |   legendgroup: 'g_${graph_name.toString}',
          |   type: '${graphType}',
          |   name: '${varList(0).replaceAll("_","") + " vs " + varList(1).replaceAll("_","")}'
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
                              , graph_name: String
                              , graph_color: String
                              , counter: Int
                              , graphType: String): String = {
    var js = ""

    for ((variable, values) <- boundaries) {
      val (outs,ins) = values.toList.partition(pair=>pair._1.isLeft)
      if(variable == varList(0) || variable == varList(1)){
        js += mkMarkers2D(variable,"out",outs,
          s"""{color: 'rgb(255, 255, 255)',
            | size: 10,
            | line: {
            |   color: colors(${colorIDs.getOrElse(graph_color, 0)}),
            |   width: 2}}""".stripMargin, varList, dict_Graph, graph_name, counter, graphType)
        js += mkMarkers2D(variable,"in",ins,
          s"""{color: colors(${colorIDs.getOrElse(graph_color, 0)}),
            | size: 10,
            | line: {
            |   color: colors(${colorIDs.getOrElse(graph_color, 0)}),
            |   width: 2}}""".stripMargin, varList, dict_Graph, graph_name, counter, graphType)
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
  private def buildWarnings2D(traj: Traj
                            , varList: List[String]
                            , inScope: Double=>Boolean
                            , colorIDs: Map[String, Int]
                            , dict_Graph: Map[Double, (String, String)]                            
                            , graph_name: String
                            , graph_color: String
                            , counter: Int
                            , graphType: String): String = {
    var js = ""
    for (variable <- traj.getVars) {
      if(variable == varList(0) || variable == varList(1)){
        js += mkWarnings2D(variable,traj,varList, inScope,
          s"""{color: colors(${colorIDs.getOrElse(graph_color, 0)}),
            | size: 15,
            | line: {
            |   color: 'rgb(0,0,0)',
            |   width: 2}}""".stripMargin, dict_Graph, graph_name, counter, graphType)
      }
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
  private def mkMarkers2D(variable:String
                        , inout:String
                        , data:List[(Either[Double,Double],(Double,String))]
                        , style: String, variables_List: List[String]
                        , dict_Graph: Map[Double, (String, String)]
                        , graph_name: String
                        , counter: Int
                        , graphType: String): String = {

    println("Entrou nos bounds ------------------------")
    println("valor de data ")
    println(data)
    var time_values = data.map(_._1.fold(x=>x,x=>x))    
    val (xValues, yValues) = time_values.flatMap(dict_Graph.get).unzip

    println("valor do dicionario")
    println(dict_Graph)
    println("valor de times")
    println(time_values)
    println("valor de x")
    println(xValues)
    println("valor de y")
    println(yValues)
    
    s"""var b_${inout}_${variable + counter.toString} = {
       |   x: ${xValues.mkString("[",",","]")},
       |   y: ${yValues.mkString("[",",","]")},
       |   text: ${data.map(s=>"'" + fixStr(s._2._2) + "'").mkString("[",",","]")},
       |   mode: 'markers',
       |   marker: $style,
       |   type: '${graphType}',
       |   legendgroup: 'g_${graph_name}',
       |   name: 'boundary of ${remove_variable({variable})}',
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
  private def mkWarnings2D(variable: String
                        , traj: Traj
                        , varList: List[String]
                        , inScope: Double=>Boolean
                        , style: String
                        , dict_Graph: Map[Double, (String, String)]
                        , graph_name: String
                        , counter: Int
                        , graphType: String): String = {                        

    (traj.getWarnings,traj.getInits,traj.getEnds) match {
      case (Some(warns),Some(inits),Some(ends)) =>

        println("Entrou nos warnings%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
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
        
        println("Valor de x")
        println(x)        
        println("valor y")
        println(y)
        println("valor msg")
        println(msg)

        val (x_axis, y_axis) = x.map(dict_Graph.getOrElse(_, ("", ""))).unzip

        s"""var w_${variable + counter.toString} = {
          |   x: ${x_axis.mkString("[",",","]")},
          |   y: ${y_axis.mkString("[",",","]")},
          |   text: ${msg.mkString("[",",","]")},
          |   mode: 'markers',
          |   marker: $style,
          |   type: '${graphType}',
          |   legendgroup: 'g_${graph_name}',
          |   name: 'Warning',
          |   showlegend: false
          |};""".stripMargin
        
            
      case _ => s"var w_$variable = {};"
      }
  }

    /////////////////////////////////////////////////
    //////    Functions to build a 3D Graph    ////// 
    /////////////////////////////////////////////////

  private def buildTraces3D(traces: Traces
                          , colorIDs: Map[String, Int]
                          , varList: List[String]
                          , counter: Int
                          , graphType: String
                          , graph_color: String): (String, String, Map[Double, (String, String, String)])  = {
    var js = ""    
    var graph_name = varList(0) + varList(1) + varList(2) + counter.toString        
    var dict_Graph: Map[Double, (String, String, String)] = Map()      

    var t: List[Double] = List.empty
    var time_axis: List[String] = List.empty
    var x_axis: List[String] = List.empty
    var y_axis: List[String] = List.empty
    var z_axis: List[String] = List.empty

    for ((variable, values) <- traces) {
      if(variable == varList(0)) {  
        val (time, x) = processValues(values.toList)   
        t = time
        x_axis = x  
        time_axis = time.map(_.toString)  
      } 
      else if(variable == varList(1)){
        val (time, y) = processValues(values.toList)           
        y_axis = y
        t = time
        time_axis = time.map(_.toString)
      }
      else if(variable == varList(2)){
        val (time, z) = processValues(values.toList)           
        z_axis = z
        t = time
        time_axis = time.map(_.toString) 
      }
    }   

    if (varList(0) == "t") {
      dict_Graph = t.zip(time_axis.zip(y_axis.zip(z_axis))).map { case (t, (x, (y, z))) => (t, (x, y, z)) }.toMap
      x_axis = time_axis
    } else {
      dict_Graph = t.zip(x_axis.zip(y_axis.zip(z_axis))).map { case (t, (x, (y, z))) => (t, (x, y, z))}.toMap
    }

    if (x_axis.nonEmpty && y_axis.nonEmpty && z_axis.nonEmpty) {
      js +=
        s"""var t_${graph_name.toString} = {
          |   x:${x_axis.mkString("[", ",", "]")},
          |   y:${y_axis.mkString("[", ",", "]")},
          |   z:${z_axis.mkString("[", ",", "]")},
          |   mode: 'lines',
          |   line: {color: colors(${colorIDs.getOrElse(graph_color,0)})},
          |   legendgroup: 'g_${graph_name.toString}',
          |   name: '${varList(0).replaceAll("_","") + " vs " + varList(1).replaceAll("_","") + " vs " + varList(12).replaceAll("_","")}',
          |   type: '${graphType}'
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
                              , graph_name: String
                              , graph_color: String
                              , counter: Int
                              , graphType: String): String = {
    var js = ""

    for ((variable, values) <- boundaries) {
      val (outs,ins) = values.toList.partition(pair=>pair._1.isLeft)
      if(variable == varList(0) || variable == varList(1) || variable == varList(2)){
        js += mkMarkers3D(variable,"out",outs,
          s"""{color: 'rgb(255, 255, 255)',
            | size: 10,
            | line: {
            |   color: colors(${colorIDs.getOrElse(graph_color, 0)}),
            |   width: 2}}""".stripMargin, varList, dict_Graph, counter, graph_name, graphType)
        js += mkMarkers3D(variable,"in",ins,
          s"""{color: colors(${colorIDs.getOrElse(graph_color, 0)}),
            | size: 10,
            | line: {
            |   color: colors(${colorIDs.getOrElse(graph_color, 0)}),
            |   width: 2}}""".stripMargin, varList, dict_Graph, counter, graph_name, graphType)
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
  private def buildWarnings3D(traj: Traj
                            , varList: List[String]
                            , inScope: Double=>Boolean
                            , colorIDs: Map[String, Int]
                            , dict_Graph: Map[Double, (String, String, String)]
                            , graph_name: String
                            , graph_color: String
                            , counter: Int
                            , graphType: String): String = {
    var js = ""
    for (variable <- traj.getVars) {
      if(variable == varList(0) || variable == varList(1) || variable == varList(2)){
        js += mkWarnings3D(variable,traj,varList,inScope,
          s"""{color: colors(${colorIDs.getOrElse(graph_color, 0)}),
            | size: 15,
            | line: {
            |   color: 'rgb(0,0,0)',
            |   width: 2}}""".stripMargin, dict_Graph, graph_name, counter, graphType)
      }
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
  private def mkMarkers3D(variable: String
                        , inout: String
                        , data: List[(Either[Double,Double],(Double,String))]
                        , style: String
                        , variables_List: List[String]
                        , dict_Graph: Map[Double, (String, String, String)]
                        , counter: Int
                        , graph_name: String
                        , graphType: String): String = {

    var time_values = data.map(_._1.fold(x=>x,x=>x))
    val (xValue, yValue, zValue) = dict_Graph.getOrElse(time_values.headOption.getOrElse(0.0), ("", ""))
    
    s"""var b_${inout}_${variable + counter.toString} = {
       |   x: [${xValue}],
       |   y: [${yValue}],
       |   z: [${zValue}],
       |   text: ${data.map(s=>"'" + fixStr(s._2._2) + "'").mkString("[",",","]")},
       |   mode: 'markers',
       |   marker: $style,
       |   legendgroup: 'g_${graph_name}',
       |   name: 'boundary of ${remove_variable(variable)}',
       |   showlegend: false,
       |   type: '${graphType}'
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
  private def mkWarnings3D(variable: String
                        , traj: Traj
                        , varList: List[String]
                        , inScope: Double=>Boolean
                        , style: String
                        , dict_Graph: Map[Double, (String, String, String)]
                        , graph_name: String
                        , counter: Int
                        , graphType: String): String = {                        

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
      
        s"""var w_${variable + counter.toString} = {
          |   x: ${x_axis.mkString("[",",","]")},
          |   y: ${y_axis.mkString("[",",","]")},
          |   z: ${z_axis.mkString("[",",","]")},
          |   text: ${msg.mkString("[",",","]")},
          |   mode: 'markers',
          |   marker: $style,
          |   legendgroup: 'g_${graph_name}',
          |   name: 'Warning',
          |   showlegend: false,
          |   type: '${graphType}'
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
*/

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


  def apply(traj:Traj,divName:String, range:Option[(Double,Double)]=None, hideCont:Boolean=true, variables_List: List[(String, String, Option[String])], graphType: String): JSString = {
    
    val dur = traj.getDur    

    // trick to avoid many sampling when already lots of boundaries exist
    val nbrSamples = 0.max(100 - traj.getInits.getOrElse(Map()).size)

    val max: Double = Eval(dur.getOrElse(SVal(10)),0)
   
    /*val colorIDs: Map[String,Int] =
      traj.getVars.zipWithIndex.toMap
    */
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

    var dict_Graph2D: Map[Double, (Either[Double,(Double,Double)], Either[Double,(Double,Double)])] = Map() 
    var dict_Graph3D: Map[Double, (Either[Double,(Double,Double)], Either[Double,(Double,Double)], Either[Double,(Double,Double)])] = Map()    
    var graph_name: String = "" 
    var graph_color:String = ""
    var varList: List[String] = List()
    var graphsNamesList: List[String] = List()
    var warningsBoundariesList: List[String] = List()
    var counter: Int = 0   
    var colorIDs: Map[String,Int] = Map()    
  
    var variables_List2D: List[(String, String)] = List.empty
    var variables_List3D: List[(String, String, String)] = List.empty

    var varList_Temp = if (variables_List.isEmpty){
      traj.getVars.map(varName => ("t", varName, None))
    } else variables_List

    var js = "var colors = Plotly.d3.scale.category10();\n"

    varList_Temp.foreach {
      case (x, y, None) => variables_List2D = variables_List2D ::: List((x, y))
      case (x, y, Some(z)) => variables_List3D = variables_List3D ::: List((x, y, z))       
    }

    if(variables_List2D.nonEmpty && variables_List3D.nonEmpty){
      println("Problems in the axis definition")
    }
    else if(variables_List2D.nonEmpty){

      //List of String Colors
      var colorsForVariables = variables_List2D.map{              
          case(var1,var2) => s"$var1$var2"        
      }
    
      colorIDs = colorsForVariables.zipWithIndex.toMap
    
      for ((xValue, yValue) <- variables_List2D) {
        varList = List(xValue,yValue) 
        graph_color = s"$xValue$yValue"       

        val (jsGraphs, g_name, dict_Graph) = buildTraces2D(traces,colorIDs, varList, counter, graphType, graph_color)
        dict_Graph2D = dict_Graph
        graph_name = g_name       
        
        graphsNamesList = graphsNamesList ++ List("t_" + graph_name)      
        warningsBoundariesList = if (xValue == "t") warningsBoundariesList ++ List("b_out_" + yValue + counter.toString, "b_in_" + yValue + counter.toString, "w_" + yValue + counter.toString) 
        else warningsBoundariesList ++ List("b_out_" + xValue + counter.toString, "b_in_" + xValue + counter.toString, "w_" + xValue + counter.toString, "b_out_" + yValue + counter.toString, "b_in_" + yValue + counter.toString, "w_" + yValue + counter.toString) 
      
        val jsBoundaries = buildBoundaries2D(boundaries,colorIDs, varList, dict_Graph2D, graph_name, graph_color, counter, graphType)
        val jsWarnings = buildWarnings2D(traj,varList,inScope,colorIDs, dict_Graph2D, graph_name, graph_color, counter, graphType)

        js += jsGraphs
        js += jsBoundaries
        js += jsWarnings

        counter = counter + 1
      }
      val traceNames = graphsNamesList ++ warningsBoundariesList
      //val traceNames = graphsNamesList

      js += s"var data = ${traceNames.mkString("[",",","]")};"   
      js += s"\nvar layout = {hovermode:'closest'};"         
      js += s"\nPlotly.newPlot('$divName', data, layout, {showSendToCloud: true});"    
    }
    /*else if (variables_List3D.nonEmpty){

      //List of String Colors
      var colorsForVariables = variables_List3D.map{        
          case (var1, var2, var3) =>  s"$var1$var2$var3"                  
      }

      colorIDs = colorsForVariables.zipWithIndex.toMap

      for ((xValue, yValue, zValue) <- variables_List3D) {
        varList = List(xValue,yValue,zValue) 
        graph_color = s"$xValue$yValue$zValue" 

        val (jsGraphs, g_name, dict_Graph) = buildTraces3D(traces,colorIDs, varList, counter, graphType, graph_color)
        dict_Graph3D = dict_Graph
        graph_name = g_name        
        
        graphsNamesList = graphsNamesList ++ List("t_" + graph_name)      
        warningsBoundariesList = if (xValue == "t") warningsBoundariesList ++ List("b_out_" + yValue + counter.toString, "b_in_" + yValue + counter.toString, "w_" + yValue + counter.toString) ++ List("b_out_" + zValue + counter.toString, "b_in_" + zValue + counter.toString, "w_" + zValue + counter.toString) 
        else warningsBoundariesList ++ List("b_out_" + xValue + counter.toString, "b_in_" + xValue + counter.toString, "w_" + xValue + counter.toString, "b_out_" + yValue + counter.toString, "b_in_" + yValue + counter.toString, "w_" + yValue + counter.toString) ++ List("b_out_" + zValue + counter.toString, "b_in_" + zValue + counter.toString, "w_" + zValue + counter.toString) 
      
        val jsBoundaries = buildBoundaries3D(boundaries,colorIDs, varList, dict_Graph3D, graph_name, graph_color, counter, graphType)
        val jsWarnings = buildWarnings3D(traj,varList,inScope,colorIDs, dict_Graph3D, graph_name, graph_color, counter, graphType)

        js += jsGraphs
        js += jsBoundaries
        js += jsWarnings

        counter = counter + 1
      }

      val traceNames = graphsNamesList ++ warningsBoundariesList

      js += s"var data = ${traceNames.mkString("[",",","]")};"  
      js += s"\nvar layout = {hovermode:'closest'};" 
      js += s"\nPlotly.newPlot('$divName', data, layout, {showSendToCloud: true});"
    }  */

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

  private def buildTraces2D(traces: Traces
                          , colorIDs: Map[String, Int]
                          , varList: List[String]
                          , counter: Int
                          , graphType: String
                          , graph_color: String): (String, String, Map[Double, (Either[Double,(Double,Double)], Either[Double,(Double,Double)])])  = {
    var js = ""    
    var graph_name = varList(0) + varList(1) + counter.toString
    var dict_Graph: Map[Double, (Either[Double,(Double,Double)], Either[Double,(Double,Double)])] = Map()      

    var t: List[Double] = List.empty
    var time_axis: List[Either[Double, (Double, Double)]] = List.empty
    var x_axis: String = ""
    var y_axis: String = ""
    var tuple1: List[Either[Double, (Double, Double)]] = List.empty
    var tuple2: List[Either[Double, (Double, Double)]] = List.empty

    for ((variable, values) <- traces) {
      if(variable == varList(0)) {  
        val (time, x) = processValues(values.toList)   
        t = values.keys.toList
        tuple1 = values.values.toList  
        time_axis = convertToEitherList(t)
      } 
      else if(variable == varList(1)){        
        val (time, y) = processValues(values.toList)           
        tuple2 = values.values.toList
        t = values.keys.toList
        time_axis = convertToEitherList(t)
      }
    } 

    if(varList(0) == "t") { 
      dict_Graph = t.zip(time_axis.zip(tuple2)).toMap
      val(x_temp, y_temp) = buildAxes2D(time_axis,tuple2)
      x_axis = x_temp
      y_axis = y_temp
     }
     else {
      dict_Graph = t.zip(tuple1.zip(tuple2)).toMap
      val(x_temp, y_temp) = buildAxes2D(tuple1,tuple2)
      x_axis = x_temp
      y_axis = y_temp
     }    
    if (x_axis.nonEmpty && y_axis.nonEmpty) {
      js +=
        s"""var t_${graph_name.toString} = {
          |   x: ${x_axis},
          |   y: ${y_axis},
          |   mode: 'lines',
          |   line: {color: colors(${colorIDs.getOrElse(graph_color,0)})},
          |   legendgroup: 'g_${graph_name.toString}',
          |   type: '${graphType}',
          |   name: '${varList(0).replaceAll("_","") + " vs " + varList(1).replaceAll("_","")}'
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
                              , dict_Graph: Map[Double, (Either[Double,(Double,Double)], Either[Double,(Double,Double)])]
                              , graph_name: String
                              , graph_color: String
                              , counter: Int
                              , graphType: String): String = {
    var js = ""

    for ((variable, values) <- boundaries) {
      val (outs,ins) = values.toList.partition(pair=>pair._1.isLeft)
      if(variable == varList(0) || variable == varList(1)){
        js += mkMarkers2D(variable,"out",outs,
          s"""{color: 'rgb(255, 255, 255)',
            | size: 10,
            | line: {
            |   color: colors(${colorIDs.getOrElse(graph_color, 0)}),
            |   width: 2}}""".stripMargin, varList, dict_Graph, graph_name, counter, graphType)
        js += mkMarkers2D(variable,"in",ins,
          s"""{color: colors(${colorIDs.getOrElse(graph_color, 0)}),
            | size: 10,
            | line: {
            |   color: colors(${colorIDs.getOrElse(graph_color, 0)}),
            |   width: 2}}""".stripMargin, varList, dict_Graph, graph_name, counter, graphType)
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
  private def buildWarnings2D(traj: Traj
                            , varList: List[String]
                            , inScope: Double=>Boolean
                            , colorIDs: Map[String, Int]
                            , dict_Graph: Map[Double, (Either[Double,(Double,Double)], Either[Double,(Double,Double)])]                           
                            , graph_name: String
                            , graph_color: String
                            , counter: Int
                            , graphType: String): String = {
    var js = ""
    for (variable <- traj.getVars) {
      if(variable == varList(0) || variable == varList(1)){
        js += mkWarnings2D(variable,traj,varList, inScope,
          s"""{color: colors(${colorIDs.getOrElse(graph_color, 0)}),
            | size: 15,
            | line: {
            |   color: 'rgb(0,0,0)',
            |   width: 2}}""".stripMargin, dict_Graph, graph_name, counter, graphType)
      }
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
  private def mkMarkers2D(variable:String
                        , inout:String
                        , data:List[(Either[Double,Double],(Double,String))]
                        , style: String, variables_List: List[String]
                        , dict_Graph: Map[Double, (Either[Double,(Double,Double)], Either[Double,(Double,Double)])]
                        , graph_name: String
                        , counter: Int
                        , graphType: String): String = {    

    var time_values = data.map(_._1.fold(x=>x,x=>x))
    println("valor de times") 
    println(time_values)  
    val (x_values, y_values) = time_values.flatMap(dict_Graph.get).unzip 
    println("valor de x")
    println(x_values)
    println("Valor de y")
    println(y_values)
    val (x_axis, y_axis) = buildAxes2DforBpundaries(x_values,y_values, inout)

    println("valor de xaxis")
    println(x_axis)
    println("Valor de yaxis")
    println(y_axis)  
    
    s"""var b_${inout}_${variable + counter.toString} = {
       |   x: ${x_axis},
       |   y: ${y_axis},
       |   text: ${data.map(s=>"'" + fixStr(s._2._2) + "'").mkString("[",",","]")},
       |   mode: 'markers',
       |   marker: $style,
       |   type: '${graphType}',
       |   legendgroup: 'g_${graph_name}',
       |   name: 'boundary of ${remove_variable({variable})}',
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
  private def mkWarnings2D(variable: String
                        , traj: Traj
                        , varList: List[String]
                        , inScope: Double=>Boolean
                        , style: String
                        , dict_Graph: Map[Double, (Either[Double,(Double,Double)], Either[Double,(Double,Double)])]
                        , graph_name: String
                        , counter: Int
                        , graphType: String): String = {   

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
        
        val (xvalues, yvalues) = x.map(key => dict_Graph(key)).unzip
        val (x_axis, y_axis) = buildAxes2D(xvalues,yvalues)
        
        s"""var w_${variable + counter.toString} = {
          |   x: ${x_axis},
          |   y: ${y_axis},
          |   text: ${msg.mkString("[",",","]")},
          |   mode: 'markers',
          |   marker: $style,
          |   type: '${graphType}',
          |   legendgroup: 'g_${graph_name}',
          |   name: 'Warning',
          |   showlegend: false
          |};""".stripMargin
        
            
      case _ => s"var w_$variable = {};"
      }
  }
/*
    /////////////////////////////////////////////////
    //////    Functions to build a 3D Graph    ////// 
    /////////////////////////////////////////////////

  private def buildTraces3D(traces: Traces
                          , colorIDs: Map[String, Int]
                          , varList: List[String]
                          , counter: Int
                          , graphType: String
                          , graph_color: String): (String, String, Map[Double, (Either[Double,(Double,Double)], Either[Double,(Double,Double)], Either[Double,(Double,Double)])])  = {
    var js = ""    
    var graph_name = varList(0) + varList(1) + varList(2) + counter.toString        
    var dict_Graph: Map[Double, (Either[Double,(Double,Double)], Either[Double,(Double,Double)], Either[Double,(Double,Double)])] = Map()      

    var t: List[Double] = List.empty
    var time_axis: List[String] = List.empty
    var x_axis: List[String] = List.empty
    var y_axis: List[String] = List.empty
    var z_axis: List[String] = List.empty

    for ((variable, values) <- traces) {
      if(variable == varList(0)) {  
        val (time, x) = processValues(values.toList)   
        t = time
        x_axis = x  
        time_axis = time.map(_.toString)  
      } 
      else if(variable == varList(1)){
        val (time, y) = processValues(values.toList)           
        y_axis = y
        t = time
        time_axis = time.map(_.toString)
      }
      else if(variable == varList(2)){
        val (time, z) = processValues(values.toList)           
        z_axis = z
        t = time
        time_axis = time.map(_.toString) 
      }
    }   

    if (varList(0) == "t") {
      dict_Graph = t.zip(time_axis.zip(y_axis.zip(z_axis))).map { case (t, (x, (y, z))) => (t, (x, y, z)) }.toMap
      x_axis = time_axis
    } else {
      dict_Graph = t.zip(x_axis.zip(y_axis.zip(z_axis))).map { case (t, (x, (y, z))) => (t, (x, y, z))}.toMap
    }

    if (x_axis.nonEmpty && y_axis.nonEmpty && z_axis.nonEmpty) {
      js +=
        s"""var t_${graph_name.toString} = {
          |   x:${x_axis.mkString("[", ",", "]")},
          |   y:${y_axis.mkString("[", ",", "]")},
          |   z:${z_axis.mkString("[", ",", "]")},
          |   mode: 'lines',
          |   line: {color: colors(${colorIDs.getOrElse(graph_color,0)})},
          |   legendgroup: 'g_${graph_name.toString}',
          |   name: '${varList(0).replaceAll("_","") + " vs " + varList(1).replaceAll("_","") + " vs " + varList(12).replaceAll("_","")}',
          |   type: '${graphType}'
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
                              , dict_Graph: Map[Double, (Either[Double,(Double,Double)], Either[Double,(Double,Double)], Either[Double,(Double,Double)])]
                              , graph_name: String
                              , graph_color: String
                              , counter: Int
                              , graphType: String): String = {
    var js = ""

    for ((variable, values) <- boundaries) {
      val (outs,ins) = values.toList.partition(pair=>pair._1.isLeft)
      if(variable == varList(0) || variable == varList(1) || variable == varList(2)){
        js += mkMarkers3D(variable,"out",outs,
          s"""{color: 'rgb(255, 255, 255)',
            | size: 10,
            | line: {
            |   color: colors(${colorIDs.getOrElse(graph_color, 0)}),
            |   width: 2}}""".stripMargin, varList, dict_Graph, counter, graph_name, graphType)
        js += mkMarkers3D(variable,"in",ins,
          s"""{color: colors(${colorIDs.getOrElse(graph_color, 0)}),
            | size: 10,
            | line: {
            |   color: colors(${colorIDs.getOrElse(graph_color, 0)}),
            |   width: 2}}""".stripMargin, varList, dict_Graph, counter, graph_name, graphType)
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
  private def buildWarnings3D(traj: Traj
                            , varList: List[String]
                            , inScope: Double=>Boolean
                            , colorIDs: Map[String, Int]
                            , dict_Graph: Map[Double, (Either[Double,(Double,Double)], Either[Double,(Double,Double)], Either[Double,(Double,Double)])]
                            , graph_name: String
                            , graph_color: String
                            , counter: Int
                            , graphType: String): String = {
    var js = ""
    for (variable <- traj.getVars) {
      if(variable == varList(0) || variable == varList(1) || variable == varList(2)){
        js += mkWarnings3D(variable,traj,varList,inScope,
          s"""{color: colors(${colorIDs.getOrElse(graph_color, 0)}),
            | size: 15,
            | line: {
            |   color: 'rgb(0,0,0)',
            |   width: 2}}""".stripMargin, dict_Graph, graph_name, counter, graphType)
      }
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
  private def mkMarkers3D(variable: String
                        , inout: String
                        , data: List[(Either[Double,Double],(Double,String))]
                        , style: String
                        , variables_List: List[String]
                        , dict_Graph: Map[Double, (Either[Double,(Double,Double)], Either[Double,(Double,Double)], Either[Double,(Double,Double)])]
                        , counter: Int
                        , graph_name: String
                        , graphType: String): String = {

    var time_values = data.map(_._1.fold(x=>x,x=>x))
    val (xValue, yValue, zValue) = dict_Graph.getOrElse(time_values.headOption.getOrElse(0.0), ("", ""))
    
    s"""var b_${inout}_${variable + counter.toString} = {
       |   x: [${xValue}],
       |   y: [${yValue}],
       |   z: [${zValue}],
       |   text: ${data.map(s=>"'" + fixStr(s._2._2) + "'").mkString("[",",","]")},
       |   mode: 'markers',
       |   marker: $style,
       |   legendgroup: 'g_${graph_name}',
       |   name: 'boundary of ${remove_variable(variable)}',
       |   showlegend: false,
       |   type: '${graphType}'
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
  private def mkWarnings3D(variable: String
                        , traj: Traj
                        , varList: List[String]
                        , inScope: Double=>Boolean
                        , style: String
                        , dict_Graph: Map[Double, (Either[Double,(Double,Double)], Either[Double,(Double,Double)], Either[Double,(Double,Double)])]
                        , graph_name: String
                        , counter: Int
                        , graphType: String): String = {                        

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
      
        s"""var w_${variable + counter.toString} = {
          |   x: ${x_axis.mkString("[",",","]")},
          |   y: ${y_axis.mkString("[",",","]")},
          |   z: ${z_axis.mkString("[",",","]")},
          |   text: ${msg.mkString("[",",","]")},
          |   mode: 'markers',
          |   marker: $style,
          |   legendgroup: 'g_${graph_name}',
          |   name: 'Warning',
          |   showlegend: false,
          |   type: '${graphType}'
          |};""".stripMargin
        

      case _ => s"var w_$variable = {};"
      }
  }*/


    ///////////////////////////////////////
    //////    Auxiliar Functions     ////// 
    ///////////////////////////////////////   

    /*def buildAxes(firstAxe: List[Either[Double,(Double, Double)]], secondAxe: List[Either[Double,(Double, Double)]]): (String, String) = {
      val (firstPart, secondPart) = (firstAxe, secondAxe).zipped.map {
        case (Left(a), Left(b)) => (List(a.toString), List(b.toString))
        case (Left(a), Right((b, c))) => (List(a.toString, a.toString, a.toString), List(b.toString, "null", c.toString))
        case (Right((a, c)), Left(b)) => (List(a.toString, a.toString, c.toString), List(b.toString, "null", b.toString))
        case (Right((a, c)), Right((b, d))) => (List(a.toString, a.toString, c.toString), List(b.toString, "null", d.toString))
      }.unzip
      (firstPart.flatten.mkString("[",",","]"), secondPart.flatten.mkString("[",",","]"))
    }*/

   

    import scala.collection.immutable.List

    def buildAxes2D(firstAxe: List[Either[Double,(Double, Double)]], secondAxe: List[Either[Double,(Double, Double)]]): (String, String) = {
      var combined = List.empty[(Either[Double,(Double, Double)], Either[Double,(Double, Double)])]
      var sortedCombined = List.empty[(Either[Double,(Double, Double)], Either[Double,(Double, Double)])]

      combined = (firstAxe, secondAxe).zipped.toList
      
      sortedCombined = combined.sortBy {
        case (Left(a), _) => a
        case (Right((a, _)), _) => a
      }


      // Preparando as partes ordenadas
      val (firstPart, secondPart) = sortedCombined.map {
        case (Left(a), Left(b)) => (List(a.toString), List(b.toString))
        case (Left(a), Right((b, c))) => (List(a.toString, a.toString, a.toString), List(b.toString, "null", c.toString))
        case (Right((a, c)), Left(b)) => (List(a.toString, a.toString, c.toString), List(b.toString, "null", b.toString))
        case (Right((a, c)), Right((b, d))) => (List(a.toString, a.toString, c.toString), List(b.toString, "null", d.toString))
      }.unzip
      
      (firstPart.flatten.mkString("[", ",", "]"), secondPart.flatten.mkString("[", ",", "]"))
    } 

    def buildAxes2DforBpundaries(firstAxe: List[Either[Double,(Double, Double)]]
                                , secondAxe: List[Either[Double,(Double, Double)]]
                                , inout:String): (String, String) = {

      var combined = List.empty[(Either[Double,(Double, Double)], Either[Double,(Double, Double)])]
      var sortedCombined = List.empty[(Either[Double,(Double, Double)], Either[Double,(Double, Double)])]

      combined = (firstAxe, secondAxe).zipped.toList
      
      sortedCombined = combined.sortBy {
        case (Left(a), _) => a
        case (Right((a, _)), _) => a
      }
      
      if(inout == "in") {      
        val (firstPart, secondPart) = sortedCombined.map {
          case (Left(a), Left(b)) => (List(a.toString), List(b.toString))
          case (Left(a), Right((b, c))) => (List(a.toString, a.toString), List(c.toString, "null"))
          case (Right((a, c)), Left(b)) => (List(c.toString, c.toString), List(b.toString, "null"))
          case (Right((a, c)), Right((b, d))) => (List(c.toString, c.toString), List(d.toString, "null"))
        }.unzip
        
        (firstPart.flatten.mkString("[", ",", "]"), secondPart.flatten.mkString("[", ",", "]"))

      } else {
        println("Entrou no out-----------------")
        val (firstPart, secondPart) = sortedCombined.map {
          case (Left(a), Left(b)) => (List(a.toString), List(b.toString))
          case (Left(a), Right((b, c))) => (List(a.toString, a.toString), List(b.toString,"null"))
          case (Right((a, c)), Left(b)) => (List(a.toString, a.toString), List(b.toString, "null"))
          case (Right((a, c)), Right((b, d))) => (List(a.toString, a.toString), List(b.toString,"null"))
        }.unzip
        
        (firstPart.flatten.mkString("[", ",", "]"), secondPart.flatten.mkString("[", ",", "]"))
      }
      
    } 

    def convertToEitherList(inputList: List[Double]): List[Either[Double, (Double, Double)]] = {
      inputList.map(value => Left(value))
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
