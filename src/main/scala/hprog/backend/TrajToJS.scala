package hprog.backend

import hprog.ast.BVal
import hprog.frontend.Semantics.Valuation
import hprog.frontend.Traj

object TrajToJS {

  // Type of intermediate structures
  private type Traces      = Map[String,TraceVar]
  private type TraceVar    = Map[Double,Either[Double,(Double,Double)]] // time -> 1 or 2 points (if boundary)
  private type Boundaries  = Map[String,BoundaryVar]
  private type BoundaryVar = Map[Either[Double,Double],(Double,String)] // left/right of a time t -> value and comment
  // private type Warnings    = Map[String,WarningVar]
  // private type WarningVar  = Map[Double,Set[String]] //time -> possible warning


  def apply(traj:Traj[Valuation],range:Option[(Double,Double)]=None,hideCont:Boolean=true): String = {

    val max: Double = traj.dur.getOrElse(10)
    //      val x0 = traj(0)
    var colorIDs: Map[String,Int] =
      traj(0).keys.zipWithIndex.toMap

    // traces are mappings from variables t0 lists of "y" values
    var traces:Traces          = Map().withDefaultValue(Map())
    var boundaries: Boundaries = Map().withDefaultValue(Map())
    // var warnings: Warnings     = Map().withDefaultValue(Map())

    // Generate sampling values for time - 100 values from 0 to max
    val (start,end) = range match {
      case Some((s,e)) =>
        if (traj.dur.isDefined)
          (s.max(0).min(max),e.max(0).min(max))
        else
          (s max 0, e max 0)
      case None => if (max<=0) (0.0,0.0) else (0.0,max)
    }
    val samples = if ((end-start)<=0) List(start) else start to end by ((end-start) / 100)


    for (t: Double <- samples)
      for ((variable, value) <- traj(t))
        traces += variable -> (traces(variable) + (t->Left(value)))
    //      println("d")
    ////
    // Add starting/ending points with notes when available
    for ((t,beforeVals) <- traj.ends if t>=start && t<=end; (variable,beforeVal) <- beforeVals) {
      val oldNote = boundaries(variable).getOrElse[(Double,String)](Right(t),(0,""))._2
      boundaries += variable ->
        (boundaries(variable) + (Right(t) ->(beforeVal, oldNote)))
      val traceBoundary: Either[Double,(Double,Double)] =
        traj.inits.get(t).flatMap(valuation=>valuation.get(variable)) match {
          case Some(afterVal) if !hideCont || afterVal!=beforeVal => // could be simplified...
            Right((beforeVal,afterVal))
          case _ => Left(beforeVal)
        }
      traces += variable -> (traces(variable) + (t->traceBoundary))
    }
    for ((t,valt) <- traj.inits if t>=start && t<=end; (variable,value) <- valt) {
      val oldNote = boundaries(variable).getOrElse[(Double,String)](Left(t),(0,""))._2
      val newNote = traj.notes.get(t) match {
        case Some(str) if str.nonEmpty => str
        case _                         => oldNote
      }
      boundaries += variable -> (boundaries(variable) + (Left(t) ->(value, newNote)))
//      boundaries += variable -> ((t, value,txt) :: boundaries(variable))
      // if very first, need to update the trace
      if (!traj.ends.contains(t))
        traces += variable -> (traces(variable) + (t -> Left(value)))
    }
    // for ((t,valt) <- traj.warnings) {
    //   warnings = warnings + //(variable -> Map())
    //     (warnings(variable)(t) ++ traj.warnings(t))
    // }

    // clean boundaries in continuous points
    if (hideCont) {
      boundaries = boundaries.map(filterCont)
    }

    /////
    // Build the JavaScript code to generate graph
    /////
    var js = "var colors = Plotly.d3.scale.category10();\n"

    //      println("e")
    js += buildTraces(traces,colorIDs)
    js += buildBoundaries(boundaries,colorIDs)
    js += buildDeviations(traj,start,end,colorIDs)

    //////
    // Build the boundary nodes 
    //////
    //////
    // build warnings in boundary nodes
    //////
    val traceNames = traces.keys.map("t_"+_).toList ++
                     boundaries.keys.map("b_out_"+_).toList ++
                     boundaries.keys.map("b_in_"+_).toList ++
                     boundaries.keys.map("w_"+_).toList


    js += s"var data = ${traceNames.mkString("[",",","]")};" +
      s"\nvar layout = {hovermode:'closest'};" +
      s"\nPlotly.newPlot('graphic', data, layout, {showSendToCloud: true});"

    //println("JS:\n"+js)
    js
  }


  ///////

  private def buildTraces(traces: Traces, colorIDs: Map[String, Int]): String = {
    var js = ""
    for ((variable, values) <- traces) {
      val tr = values.toList.sortWith(_._1 <= _._1).flatMap(expandPoint)
      val (xs,ys) = tr.unzip
      ////   ${ys.map(_.map(_.toString()).getOrElse("null")).mkString("[", ",", "]")},
      js +=
        s"""var t_${variable} = {
           |   x: ${xs.mkString("[",",","]")},
           |   y: ${ys.mkString("[",",","]")},
           |   mode: 'lines',
           |   line: {color: colors(${colorIDs.getOrElse(variable,0)})},
           |   legendgroup: 'g_${variable}',
           |   name: '$variable'
           |};
             """.stripMargin
    }
    js
  }

  private def buildBoundaries(boundaries: Boundaries, colorIDs: Map[String, Int]): String = {
    var js = ""
    for ((variable, values) <- boundaries) {
      //      val sortedVals = values.sorted
      val (ins,outs) = values.toList.partition(pair=>pair._1.isLeft)
      js += mkMarkers(variable,"out",outs,
        s"""{color: 'rgb(255, 255, 255)',
           | size: 10,
           | line: {
           |   color: colors(${colorIDs.getOrElse(variable, 0)}),
           |   width: 2}}""".stripMargin)
      js += mkMarkers(variable,"in",ins,
        s"""{color: colors(${colorIDs.getOrElse(variable, 0)}),
           | size: 10,
           | line: {
           |   color: colors(${colorIDs.getOrElse(variable, 0)}),
           |   width: 2}}""".stripMargin)
    }
    js
  }

  private def buildDeviations(traj: Traj[Valuation], start:Double, end:Double, colorIDs: Map[String, Int]): String = {
    var js = ""
    for (variable <- traj(0).keys) {
      js += mkWarnings(variable,traj,start,end,
        s"""{color: colors(${colorIDs.getOrElse(variable, 0)}),
           | size: 15,
           | line: {
           |   color: 'rgb(0,0,0)',
           |   width: 2}}""".stripMargin)
    }
    js
  }



  private def expandPoint(point:(Double,Either[Double,(Double,Double)])): List[(Double,String)] =
    point match {
      case (t,Left(v)) => List((t,v.toString))
      case (t,Right((v1,v2))) => List((t,v1.toString),(t,"null"),(t,v2.toString))
    }

  private def filterCont(boundary: (String,BoundaryVar)): (String,BoundaryVar) = {
    (boundary._1, boundary._2.filter({
      case (Left(t),v1)  => boundary._2.get(Right(t)) match {
          case Some(v2) => v1._1 != v2._1
          case None     => true
        }
      case (Right(t),v1) => boundary._2.get(Left(t)) match {
          case Some(v2) => v1._1 != v2._1
          case None     => true
        }
    }))
  }

  private def mkMarkers(variable:String,
                        inout:String,
                        data:List[(Either[Double,Double],(Double,String))],
                        style: String): String =
    s"""var b_${inout}_$variable = {
       |   x: ${data.map(_._1.fold(x=>x,x=>x)).mkString("[", ",", "]")},
       |   y: ${data.map(_._2._1).mkString("[", ",", "]")},
       |   text: ${data.map(s=>"'" + fixStr(s._2._2) + "'").mkString("[",",","]")},
       |   mode: 'markers',
       |   marker: $style,
       |   type: 'scatter',
       |   legendgroup: 'g_$variable',
       |   name: 'boundary of $variable',
       |   showlegend: false
       |};""".stripMargin

//  marker: {color: colors(${colorID.getOrElse(variable, 0)})},

  private def mkWarnings(variable: String, traj: Traj[Valuation]
                       , start: Double, end: Double
                       , style:String): String = {

    val (x,y,msg) = traj
      .warnings(Some(Map()))
      .toList
      .filter(x => x._1 >= start && x._1 <= end)
      .map(x=>(x._1,traj(x._1)(variable),"'"+x._2._1.map(w=>s"${fixStr(w)}").mkString("</br>")+"'"))
      .unzip3

    s"""var w_$variable = { 
       |   x: ${x.mkString("[",",","]")},
       |   y: ${y.mkString("[",",","]")},
       |   text: ${msg.mkString("[",",","]")},
       |   mode: 'markers',
       |   marker: ${style},
       |   type: 'scatter',
       |   legendgroup: 'g_$variable',
       |   name: 'Warning',
       |   showlegend: false
       |};""".stripMargin
  }

  private def fixStr(str:String): String =
    str.replaceAll("\\\\", "\\\\\\\\")

}
