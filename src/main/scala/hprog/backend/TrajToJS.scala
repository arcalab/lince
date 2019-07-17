package hprog.backend

import hprog.ast.{BVal, SVal}
import hprog.frontend.Semantics.Valuation
import hprog.frontend.{Eval, Traj}

object TrajToJS {

  // Type of intermediate structures
  private type Traces      = Map[String,TraceVar]
  private type TraceVar    = Map[Double,Either[Double,(Double,Double)]] // time -> 1 or 2 points (if boundary)
  private type Boundaries  = Map[String,BoundaryVar]
  private type BoundaryVar = Map[Either[Double,Double],(Double,String)] // left/right of a time t -> value and comment
  // private type Warnings    = Map[String,WarningVar]
  // private type WarningVar  = Map[Double,Set[String]] //time -> possible warning


  def apply(traj:Traj[Valuation],range:Option[(Double,Double)]=None,hideCont:Boolean=true): String = {

    val max: Double = Eval(traj.dur.getOrElse(SVal(10)),0)
    //      val x0 = traj(0)
    var colorIDs: Map[String,Int] =
      traj(0).keys.zipWithIndex.toMap

    // traces are mappings from variables t0 lists of "y" values
    var traces:Traces          = Map().withDefaultValue(Map())
       // each point in time has 1 or 2 values
    var boundaries: Boundaries = Map().withDefaultValue(Map())
       // each boundary variable maps begin or end times to a value and a note.

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
    val samples = if ((end-start)<=0) List(start) else start to end by ((end-start) / 10)

    // checks if a time value is within the scope
    def inScope(t:Double): Boolean = t>=start && t<=end

    // add values of the trace (line) of thr traj
    for (t: Double <- samples)
      for ((variable, value) <- traj(t))
        traces += variable -> (traces(variable) + (t->Left(value)))
    //      println("d")
    ////

    // Add ending points to "boundaries" and to "traces"
    for ((t,endValues) <- traj.ends if inScope(Eval(t,0));
         (variable,endValue) <- endValues) {
      val t2 = Eval(t,0) // evaluate the time expression of next point
      val endValue2 = Eval(endValue,t2) // evaluate value for that point
//      val oldNote = boundaries(variable).getOrElse[(Double,String)](Right(t2),(0,""))._2
//      boundaries += variable ->
//        (boundaries(variable) + (Right(t2) ->(endValue, oldNote)))
//      val traceBoundary: Either[Double,(Double,Double)] =
//        traj.inits.get(t).flatMap(valuation=>valuation.get(variable)) match {
//          case Some(afterVal) if !hideCont || Eval(afterVal)!=Eval(endValue) => // could be simplified...
//            Right((Eval(endValue),Eval(afterVal)))
//          case _ => Left(Eval(endValue))
//        }
//      traces += variable -> (traces(variable) + (t2->traceBoundary))
      traces     += variable -> (traces(variable)     + (t2->Left(endValue2)))
      boundaries += variable -> (boundaries(variable) + (Right(t2)->(endValue2,"")))
    }

    // add init points to boundaries and traces
    for ((t,initValues) <- traj.inits if inScope(Eval(t,0));
         (variable,initValue) <- initValues) {
      val t2 = Eval(t,0)
      val initValue2 = Eval(initValue,t2)

      val nextTraceT = traces(variable).get(t2) match {
        case Some(Left(v)) => Right(v,initValue2)
        case Some(x) => x
        case None => Left(initValue2)
      }
      traces     += variable -> (traces(variable)     + (t2->nextTraceT))
      boundaries += variable -> (boundaries(variable) + (Left(t2)->(initValue2,"")))
    }

    println(s"all notes: ${traj.notes.mkString("/")}")
    for ((expr,note) <- traj.notes) {
      boundaries = boundaries.mapValues(vals => addNote(Eval(expr,0),note,vals))
    }
    def addNote(t: Double, n: String, vals: BoundaryVar): BoundaryVar = {
      vals.get(Right(t)) match {
        case Some((value,n2)) => vals + (Right(t)->(value,n2++n++"<br>"))
        case None => vals + (Left(t)->(0,n)) // should not happen...
      }
    }
    println(s"boundaries with notes: ${boundaries}")
    println(s"all warnings: ${traj.warnings.mkString("/")}")


    // Add starting/ending points with notes when available
//    for ((t,valt) <- traj.inits if Eval(t)>=start && Eval(t)<=end;
//         (variable,value) <- valt) {
//      val t2 = Eval(t)
//      val oldNote = boundaries(variable).getOrElse[(Double,String)](Left(t2),(0,""))._2
//      val newNote = traj.notes.get(t) match {
//        case Some(str) if str.nonEmpty => str
//        case _                         => oldNote
//      }
//      boundaries += variable -> (boundaries(variable) + (Left(t) ->(value, newNote)))
////      boundaries += variable -> ((t, value,txt) :: boundaries(variable))
//      // if very first, need to update the trace
//      if (!traj.ends.contains(t))
//        traces += variable -> (traces(variable) + (t -> Left(value)))
//    }
//    // for ((t,valt) <- traj.warnings) {
//    //   warnings = warnings + //(variable -> Map())
//    //     (warnings(variable)(t) ++ traj.warnings(t))
//    // }

    // clean boundaries in continuous points
    if (hideCont) {
      boundaries = boundaries.mapValues(filterCont)
    }

    /////
    // Build the JavaScript code to generate graph
    /////
    var js = "var colors = Plotly.d3.scale.category10();\n"

    //      println("e")
    js += buildTraces(traces,colorIDs)
    js += buildBoundaries(boundaries,colorIDs)
    js += buildWarnings(traj,inScope,colorIDs)

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

  private def buildWarnings(traj: Traj[Valuation], inScope:Double=>Boolean, colorIDs: Map[String, Int]): String = {
    var js = ""
    for (variable <- traj(0).keys) {
      js += mkWarnings(variable,traj,inScope,
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
                       , inScope: Double=>Boolean
                       , style:String): String = {

    val (x,y,msg) = traj
      .warnings
      .map(es=>(Eval(es._1,0),"'"+fixStr(es._2)+"'"))
      .filter(es=>inScope(es._1))
      .map(es=>(es._1,traj(es._1)(variable),es._2))
      .unzip3
//      .warnings(Some(Map()))
//      .toList
//      .filter(x => x._1 >= start && x._1 <= end)
//      .map(x=>(x._1,traj(x._1)(variable),"'"+x._2._1.map(w=>s"${fixStr(w)}").mkString("</br>")+"'"))
//      .unzip3

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
