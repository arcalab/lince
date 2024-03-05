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

    var dict_Graph: Map[Double, (String, String)] =  Map.empty

    // trick to avoid many sampling when already lots of boundaries exist
    val nbrSamples = 0.max(100 - traj.getInits.getOrElse(Map()).size)


    val max: Double = Eval(dur.getOrElse(SVal(10)),0)

    val colorIDs: Map[String,Int] =
      traj.getVars.zipWithIndex.toMap

    // traces are mappings from variables t0 lists of "y" values
    var traces:Traces          = Map().withDefaultValue(Map())
   // each point in time has 1 or 2 values
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

    /////
    // Build the JavaScript code to generate graph
    /////
    var js = "var colors = Plotly.d3.scale.category10();\n"


    js += buildTraces(traces,colorIDs, variables_List, dict_Graph)
    js += buildBoundaries(boundaries,colorIDs, variables_List, dict_Graph)
    //js += buildWarnings(traj,inScope,colorIDs)


    //val traceNames = traces.keys.map("t_"+_).toList //++
                     //boundaries.keys.map("b_out_"+_).toList ++
                     //boundaries.keys.map("b_in_"+_).toList ++
                     //boundaries.keys.map("w_"+_).toList
    
    val traceNames = List("t__x") ++ boundaries.keys.map("b_out_"+_).toList ++ boundaries.keys.map("b_in_"+_).toList

    js += s"var data = ${traceNames.mkString("[",",","]")};" +
      s"\nvar layout = {hovermode:'closest'};" +
      s"\nPlotly.newPlot('$divName', data, layout, {showSendToCloud: true});"
    
    println(js)

    js    
  }


  ///////

/**
  * Builds a JavaScript trace block based on specified traces, colorIDs, and variable list.
  *
  * @param traces         Map containing traces for different variables.
  * @param colorIDs       Map associating variable names with color IDs.
  * @param variables_List List of variable names to consider.
  * @param dict_Graph     Dictionary to store the the values of the graph
  * @return               JavaScript block representing the specified traces.
  */

  private def buildTraces(traces: Traces, colorIDs: Map[String, Int], variables_List: List[String], dict_Graph: Map[Double, (String, String)]): String = {
    var js = ""    
    var graph_name = ""
    var xtTmp: List[Double] = List.empty
    var xTmp: List[String] = List.empty
    var ytTmp: List[Double] = List.empty
    var yTmp: List[String] = List.empty

    for ((variable, values) <- traces) {
    if (variable == variables_List(0)) {
      graph_name ++= variable
      val tr = values.toList.sortWith(_._1 <= _._1).flatMap(expandPoint)
      val (xt, x) = tr.unzip
      xtTmp = xt
      xTmp = x
    } else if (variable == variables_List(1)) {
      val tr = values.toList.sortWith(_._1 <= _._1).flatMap(expandPoint)
      val (yt, y) = tr.unzip
      ytTmp = yt
      yTmp = y
    }
  }
    dict_Graph = xtTmp.zip(xTmp.zip(yTmp)).toMap

    if (xTmp.nonEmpty && yTmp.nonEmpty) {
      js +=
        s"""var t_${graph_name.toString} = {
          |   x: ${xTmp.mkString("[", ",", "]")},
          |   y: ${yTmp.mkString("[", ",", "]")},
          |   mode: 'lines',
          |   line: {color: colors(${colorIDs.getOrElse(graph_name.toString, 0)})},
          |   legendgroup: 'g_${remove_variable(graph_name.toString)}',
          |   name: '${remove_variable(graph_name.toString)}'
          |};
          """.stripMargin
    }
    js
  }

  private def buildBoundaries(boundaries: Boundaries, colorIDs: Map[String, Int], variables_List: List[String], dict_Graph: Map[Double, (String, String)]): String = {
    var js = ""
    for ((variable, values) <- boundaries) {
      val (outs,ins) = values.toList.partition(pair=>pair._1.isLeft)
      if(variable == variables_List(0) || variable == variables_List(1)){
        js += mkMarkers(variable,"out",outs,
          s"""{color: 'rgb(255, 255, 255)',
            | size: 10,
            | line: {
            |   color: colors(${colorIDs.getOrElse(variable, 0)}),
            |   width: 2}}""".stripMargin, variables_List, dict_Graph)
        js += mkMarkers(variable,"in",ins,
          s"""{color: colors(${colorIDs.getOrElse(variable, 0)}),
            | size: 10,
            | line: {
            |   color: colors(${colorIDs.getOrElse(variable, 0)}),
            |   width: 2}}""".stripMargin, variables_List, dict_Graph)
      }
    }
    js
  }

  private def buildWarnings(traj: Traj, inScope:Double=>Boolean, colorIDs: Map[String, Int]): String = {
    var js = ""
    for (variable <- traj.getVars) {
      js += mkWarnings(variable,traj,inScope,
        s"""{color: colors(${colorIDs.getOrElse(variable, 0)}),
           | size: 15,
           | line: {
           |   color: 'rgb(0,0,0)',
           |   width: 2}}""".stripMargin)
    }
    js
  }

 def remove_variable(variable:String):String = {
  //var aux=variable.substring(0,variable.length-1)
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

  
//  marker: {color: colors(${colorID.getOrElse(variable, 0)})},

  private def mkWarnings(variable: String, traj: Traj
                       , inScope: Double=>Boolean
                       , style:String): String = {

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

        s"""var w_$variable = {
           |   x: ${x.mkString("[",",","]")},
           |   y: ${y.mkString("[",",","]")},
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
